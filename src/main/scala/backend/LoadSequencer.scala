package saturn.backend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import saturn.common._

class LoadSequencer(implicit p: Parameters) extends PipeSequencer[LoadRespMicroOp]()(p) {
  def accepts(inst: VectorIssueInst) = inst.vmu && !inst.opcode(5)

  val io = IO(new PipeSequencerIO(new LoadRespMicroOp))

  val valid = RegInit(false.B)
  val inst  = Reg(new BackendIssueInst)
  val eidx  = Reg(UInt(log2Ceil(maxVLMax).W))
  val sidx  = Reg(UInt(3.W))
  val wvd_mask = Reg(UInt(egsTotal.W))
  val rvm_mask = Reg(UInt(egsPerVReg.W))
  val head     = Reg(Bool())

  val renvm     = !inst.vm
  val next_eidx = get_next_eidx(inst.vconfig.vl, eidx, inst.mem_elem_size, 0.U, false.B, false.B)
  val tail      = next_eidx === inst.vconfig.vl && sidx === inst.seg_nf

  io.dis.ready := !valid || (tail && io.iss.fire) && !io.dis_stall

  when (io.dis.fire) {
    val iss_inst = io.dis.bits
    valid := true.B
    inst  := iss_inst
    eidx  := iss_inst.vstart
    sidx  := iss_inst.segstart

    val wvd_arch_mask = Wire(Vec(32, Bool()))
    for (i <- 0 until 32) {
      val group = i.U >> iss_inst.emul
      val rd_group = iss_inst.rd >> iss_inst.emul
      wvd_arch_mask(i) := group >= rd_group && group <= (rd_group + iss_inst.nf)
    }
    wvd_mask := FillInterleaved(egsPerVReg, wvd_arch_mask.asUInt)
    rvm_mask := Mux(!iss_inst.vm, ~(0.U(egsPerVReg.W)), 0.U)
    head := true.B
  } .elsewhen (io.iss.fire) {
    valid := !tail
    head := false.B
  }

  io.vat := inst.vat
  io.seq_hazard.valid := valid
  io.seq_hazard.bits.rintent := hazardMultiply(rvm_mask)
  io.seq_hazard.bits.wintent := hazardMultiply(wvd_mask)
  io.seq_hazard.bits.vat     := inst.vat

  val vm_read_oh  = Mux(renvm, UIntToOH(io.rvm.req.bits.eg), 0.U)
  val vd_write_oh = UIntToOH(io.iss.bits.wvd_eg)

  val raw_hazard = (vm_read_oh & io.older_writes) =/= 0.U
  val waw_hazard = (vd_write_oh & io.older_writes) =/= 0.U
  val war_hazard = (vd_write_oh & io.older_reads) =/= 0.U
  val data_hazard = raw_hazard || waw_hazard || war_hazard

  io.rvm.req.valid := valid && renvm
  io.rvm.req.bits.eg := getEgId(0.U, eidx, 0.U, true.B)
  io.rvm.req.bits.oldest := inst.vat === io.vat_head

  io.iss.valid := valid && !data_hazard && (!renvm || io.rvm.req.ready)
  io.iss.bits.wvd_eg    := getEgId(inst.rd + (sidx << inst.emul), eidx, inst.mem_elem_size, false.B)
  io.iss.bits.tail       := tail
  io.iss.bits.vat        := inst.vat
  io.iss.bits.debug_id   := inst.debug_id
  io.iss.bits.eidx       := eidx

  val head_mask = get_head_mask(~(0.U(dLenB.W)), eidx     , inst.mem_elem_size)
  val tail_mask = get_tail_mask(~(0.U(dLenB.W)), next_eidx, inst.mem_elem_size)

  io.iss.bits.eidx_wmask := Mux(sidx > inst.segend && inst.seg_nf =/= 0.U, 0.U, head_mask & tail_mask)
  io.iss.bits.use_rmask := renvm
  io.iss.bits.elem_size := inst.mem_elem_size

  when (io.iss.fire && !tail) {
    when (next_is_new_eg(eidx, next_eidx, inst.mem_elem_size, false.B) && vParams.enableChaining.B) {
      wvd_mask := wvd_mask & ~vd_write_oh
    }
    when (next_is_new_eg(eidx, next_eidx, 0.U, true.B) && vParams.enableChaining.B) {
      rvm_mask := rvm_mask & ~UIntToOH(io.rvm.req.bits.eg)
    }
    when (sidx === inst.seg_nf) {
      sidx := 0.U
      eidx := next_eidx
    } .otherwise {
      sidx := sidx + 1.U
    }
  }

  io.busy := valid
  io.head := head
}
