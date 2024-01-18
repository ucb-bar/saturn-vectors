package vector.backend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import vector.common._
import vector.insns._

class IndexMaskSequencer(exu_insns: Seq[VectorInstruction])(implicit p: Parameters) extends PipeSequencer(new IndexMaskMicroOp)(p) {
  def accepts(inst: VectorIssueInst) = inst.vmu && ((!inst.vm && inst.mop =/= mopUnit) || inst.mop(0)) || new VectorDecoder(inst.funct3, inst.funct6, inst.rs1, inst.rs2, exu_insns.filter(_.props.contains(UsesIndexMaskSeq.Y)), Nil).matched

  val valid = RegInit(false.B)
  val inst  = Reg(new VectorIssueInst)
  val eidx  = Reg(UInt(log2Ceil(maxVLMax).W))
  val rvs2_mask = Reg(UInt(egsTotal.W))
  val rvm_mask = Reg(UInt(egsPerVReg.W))
  val head = Reg(Bool())

  val elementwise = inst.vmu
  val renvm = !inst.vm && inst.mop =/= mopUnit
  val renv2 = inst.mop(0)
  val incr_eew = Mux(inst.vmu, inst.mem_idx_size, inst.vconfig.vtype.vsew)
  val next_eidx = Mux(elementwise,
    eidx +& 1.U,
    get_next_eidx(inst.vconfig.vl, eidx, incr_eew, 0.U, false.B))
  val tail = next_eidx === inst.vconfig.vl

  io.dis.ready := !valid || (tail && io.iss.fire)

  when (io.dis.fire) {
    val iss_inst = io.dis.bits
    valid := true.B
    inst  := iss_inst
    eidx  := iss_inst.vstart

    val renv2_arch_mask = get_arch_mask(iss_inst.rs2, iss_inst.pos_lmul, 3)
    rvs2_mask := FillInterleaved(egsPerVReg, renv2_arch_mask)
    rvm_mask := Mux(!iss_inst.vm && iss_inst.mop =/= mopUnit, ~(0.U(egsPerVReg.W)), 0.U)
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
  io.rvs2.req.bits := getEgId(inst.rs2, eidx, incr_eew, false.B)
  io.rvm.req.valid := valid && renvm
  io.rvm.req.bits := getEgId(0.U, eidx, 0.U, true.B)

  io.iss.valid := valid && !data_hazard && (!renvm || io.rvm.req.ready) && (!renv2 || io.rvs2.req.ready)
  io.iss.bits.rvs2_data := io.rvs2.resp
  io.iss.bits.rvs2_eew  := incr_eew
  io.iss.bits.eidx      := eidx
  io.iss.bits.rvm_data  := io.rvm.resp
  io.iss.bits.vmu       := inst.vmu

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
