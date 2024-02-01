package saturn.backend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import saturn.common._
import saturn.insns._

class PermuteSequencer(exu_insns: Seq[VectorInstruction])(implicit p: Parameters) extends PipeSequencer(new PermuteMicroOp)(p) {
  def accepts(inst: VectorIssueInst) = inst.vmu && ((!inst.vm && inst.mop =/= mopUnit) || inst.mop(0)) || new VectorDecoder(inst.funct3, inst.funct6, inst.rs1, inst.rs2, exu_insns.filter(_.props.contains(UsesPermuteSeq.Y)), Nil).matched

  val valid = RegInit(false.B)
  val inst  = Reg(new BackendIssueInst)
  val eidx  = Reg(UInt(log2Ceil(maxVLMax).W))
  val rvs2_mask = Reg(UInt(egsTotal.W))
  val rvm_mask = Reg(UInt(egsPerVReg.W))
  val head = Reg(Bool())
  val slide_offset = Reg(UInt((1+log2Ceil(maxVLMax)).W))
  val slide = !inst.vmu && inst.funct3 =/= OPIVV
  val slide_up = !inst.funct6(0)
  val rs2 = Mux(inst.rs1_is_rs2, inst.rs1, inst.rs2)
  val gatherei16 = inst.funct3 === OPIVV && inst.opif6 === OPIFunct6.rgatherei16

  val elementwise = inst.vmu
  val renvm = inst.renvm
  val renv2 = inst.renv2
  val incr_eew = Mux(inst.vmu, inst.mem_idx_size,
    Mux(gatherei16, 1.U, inst.vconfig.vtype.vsew))
  val eff_vl = Mux(slide,
    Mux(slide_up, inst.vconfig.vl - slide_offset, min(inst.vconfig.vtype.vlMax, inst.vconfig.vl + slide_offset)),
    inst.vconfig.vl
  )(log2Ceil(maxVLMax),0)
  val next_eidx = get_next_eidx(eff_vl, eidx, incr_eew, 0.U, false.B, elementwise)
  val tail = next_eidx === eff_vl

  io.dis.ready := !valid || (tail && io.iss.fire)

  when (io.dis.fire) {
    val iss_inst = io.dis.bits
    val offset = Mux(iss_inst.isOpi, get_max_offset(Mux(iss_inst.funct3(2), iss_inst.rs1_data, iss_inst.imm5)), 1.U)
    val slide = !iss_inst.vmu && iss_inst.funct3 =/= OPIVV
    val slide_up = !iss_inst.funct6(0)
    val slide_start = Mux(slide_up, 0.U, offset)
    val vlmax = iss_inst.vconfig.vtype.vlMax
    val slide_no_read = Mux(slide_up,
      iss_inst.vconfig.vl <= offset,
      offset >= vlmax)
    valid := Mux(!slide, true.B, !slide_no_read)
    inst  := iss_inst
    eidx  := Mux(!slide, iss_inst.vstart, slide_start)
    slide_offset := offset

    val rs2 = Mux(iss_inst.rs1_is_rs2, iss_inst.rs1, iss_inst.rs2)
    val renv2_arch_mask = get_arch_mask(rs2, iss_inst.emul)
    rvs2_mask := Mux(iss_inst.renv2, FillInterleaved(egsPerVReg, renv2_arch_mask), 0.U)
    rvm_mask := Mux(iss_inst.renvm, ~(0.U(egsPerVReg.W)), 0.U)
    head := true.B
  } .elsewhen (io.iss.fire) {
    valid := !tail
    head := false.B
  }

  io.vat := inst.vat
  io.seq_hazard.valid := valid
  io.seq_hazard.bits.rintent := rvs2_mask | rvm_mask
  io.seq_hazard.bits.wintent := false.B
  io.seq_hazard.bits.vat := inst.vat

  val vs2_read_oh = Mux(renv2, UIntToOH(io.rvs2.req.bits), 0.U)
  val vm_read_oh  = Mux(renvm, UIntToOH(io.rvm.req.bits), 0.U)

  val raw_hazard = ((vm_read_oh | vs2_read_oh) & io.older_writes) =/= 0.U
  val data_hazard = raw_hazard

  io.rvs2.req.valid := valid && renv2
  io.rvs2.req.bits := getEgId(rs2, eidx, incr_eew, false.B)
  io.rvm.req.valid := valid && renvm
  io.rvm.req.bits := getEgId(0.U, eidx, 0.U, true.B)

  io.iss.valid := valid && !data_hazard && (!renvm || io.rvm.req.ready) && (!renv2 || io.rvs2.req.ready)
  io.iss.bits.rvs2_data := io.rvs2.resp
  io.iss.bits.rvs2_eew  := incr_eew
  io.iss.bits.eidx      := eidx
  io.iss.bits.vl        := eff_vl
  io.iss.bits.rvm_data  := Mux(renvm, io.rvm.resp, ~(0.U(dLen.W)))
  io.iss.bits.vmu       := inst.vmu
  io.iss.bits.tail      := tail

  when (io.iss.fire && !tail) {
    when (next_is_new_eg(eidx, next_eidx, incr_eew, false.B)) {
      rvs2_mask := rvs2_mask & ~vs2_read_oh
    }
    when (next_is_new_eg(eidx, next_eidx, 0.U, true.B)) {
      rvm_mask := rvm_mask & ~UIntToOH(io.rvm.req.bits)
    }
    eidx := next_eidx
  }

  io.busy := valid
}
