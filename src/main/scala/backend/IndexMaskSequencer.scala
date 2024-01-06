package vector.backend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import vector.common._

class IndexMaskSequencer(implicit p: Parameters) extends PipeSequencer()(p) {
  def issQEntries = vParams.vimissqEntries
  val issq = Module(new DCEQueue(new VectorIssueInst, issQEntries, pipe=true))

  def accepts(inst: VectorIssueInst) = inst.vmu && ((!inst.vm && inst.mop =/= mopUnit) || inst.mop(0))
  io.dis.ready := !accepts(io.dis.bits) || issq.io.enq.ready
  issq.io.enq.valid := io.dis.valid && accepts(io.dis.bits)
  issq.io.enq.bits := io.dis.bits

  for (i <- 0 until issQEntries) {
    val inst = issq.io.peek(i).bits
    io.iss_hazards(i).valid    := issq.io.peek(i).valid
    io.iss_hazards(i).bits.vat := inst.vat
    io.iss_hazards(i).bits.rintent := get_arch_mask(inst.rs2, inst.pos_lmul, 3) | Mux(!inst.vm && inst.mop =/= mopUnit, 1.U, 0.U)
    io.iss_hazards(i).bits.wintent := 0.U
  }

  val valid = RegInit(false.B)
  val inst  = Reg(new VectorIssueInst)
  val eidx  = Reg(UInt(log2Ceil(maxVLMax).W))
  val rvs2_mask = Reg(UInt(egsTotal.W))
  val rvm_mask = Reg(UInt(egsPerVReg.W))
  val head = Reg(Bool())

  val renvm = !inst.vm && inst.mop =/= mopUnit
  val renv2 = inst.mop(0)
  val next_eidx = eidx +& 1.U
  val tail = next_eidx === inst.vconfig.vl

  issq.io.deq.ready := !valid || (tail && io.iss.fire)

  when (issq.io.deq.fire) {
    val iss_inst = issq.io.deq.bits
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
  io.rvs2.req.bits := getEgId(inst.rs2, eidx, inst.mem_idx_size, false.B)
  io.rvm.req.valid := valid && renvm
  io.rvm.req.bits := getEgId(0.U, eidx, 0.U, true.B)

  io.iss.valid := valid && !data_hazard && (!renvm || io.rvm.req.ready) && (!renv2 || io.rvs2.req.ready)
  io.iss.bits.rvs1_data := DontCare
  io.iss.bits.rvs2_data := io.rvs2.resp
  io.iss.bits.rvd_data  := DontCare
  io.iss.bits.rvs1_eew  := DontCare
  io.iss.bits.rvs2_eew  := inst.mem_idx_size
  io.iss.bits.rvd_eew   := DontCare
  io.iss.bits.vd_eew    := DontCare
  io.iss.bits.eidx      := eidx
  io.iss.bits.wvd_eg    := DontCare
  io.iss.bits.rs1        := inst.rs1
  io.iss.bits.rd         := inst.rd
  io.iss.bits.funct3     := DontCare
  io.iss.bits.funct6     := DontCare
  io.iss.bits.tail       := tail
  io.iss.bits.head       := head
  io.iss.bits.vl         := inst.vconfig.vl
  io.iss.bits.vat        := inst.vat
  io.iss.bits.vm         := inst.vm
  io.iss.bits.rm         := DontCare
  io.iss.bits.acc        := false.B

  val tail_mask = get_tail_mask(~(0.U(dLenB.W)), next_eidx, inst.mem_elem_size)
  val vm_mask = Mux(!renvm, ~(0.U(dLenB.W)), get_vm_mask(io.rvm.resp, eidx, inst.mem_elem_size))
  io.iss.bits.wmask      := vm_mask
  io.iss.bits.rmask      := vm_mask
  io.iss.bits.rvm_data   := io.rvm.resp

  when (io.iss.fire && !tail) {
    when (next_is_new_eg(eidx, next_eidx, inst.mem_idx_size, false.B)) {
      rvs2_mask := rvs2_mask & ~vs2_read_oh
    }
    when (next_is_new_eg(eidx, next_eidx, 0.U, true.B)) {
      rvm_mask := rvm_mask & ~UIntToOH(io.rvm.req.bits)
    }
    eidx := next_eidx
  }

  io.busy := valid
}
