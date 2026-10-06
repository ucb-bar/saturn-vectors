package saturn.backend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import saturn.common._

class StoreSequencerIO(implicit p: Parameters) extends SequencerIO(new StoreDataMicroOp) {
  val rvd  = Decoupled(new VectorReadReq)
  val rvm  = Decoupled(new VectorReadReq)
}

class StoreSequencer(implicit p: Parameters) extends Sequencer[StoreDataMicroOp]()(p) {
  def accepts(inst: VectorIssueInst) = inst.vmu && inst.opcode(5)

  val io = IO(new StoreSequencerIO)

  val valid    = RegInit(false.B)
  val inst     = Reg(new VectorIssueInst)
  val eidx     = Reg(UInt(log2Ceil(maxVLMax).W))
  val sidx     = Reg(UInt(3.W))
  val rvd_mask = Reg(UInt(egsTotal.W))
  val rvm_mask = Reg(UInt(egsPerVReg.W))
  val sub_mlen = Reg(UInt(2.W))
  val head     = Reg(Bool())

  val renvm     = !inst.vm && inst.mop === mopUnit
  val next_eidx = get_next_eidx(inst.vconfig.vl, eidx, inst.mem_elem_size, sub_mlen, false.B, false.B, mLen)
  val tail      = next_eidx === inst.vconfig.vl && sidx === inst.seg_nf

  io.dis.ready := !valid || (tail && io.iss.fire) && !io.dis_stall

  when (io.dis.fire) {
    val iss_inst = io.dis.bits
    valid := true.B
    inst  := iss_inst
    eidx  := iss_inst.vstart
    sidx  := 0.U

    val rvd_arch_mask = Wire(Vec(32, Bool()))
    for (i <- 0 until 32) {
      val group = i.U >> iss_inst.emul
      val rd_group = iss_inst.rd >> iss_inst.emul
      rvd_arch_mask(i) := group >= rd_group && group <= (rd_group + iss_inst.nf)
    }
    rvd_mask := FillInterleaved(egsPerVReg, rvd_arch_mask.asUInt)
    rvm_mask := Mux(!iss_inst.vm, ~(0.U(egsPerVReg.W)), 0.U)
    sub_mlen := Mux(iss_inst.seg_nf =/= 0.U && (mLenOffBits.U > (3.U +& iss_inst.mem_elem_size)),
      mLenOffBits.U - 3.U - iss_inst.mem_elem_size,
      0.U)
    head := true.B
  } .elsewhen (io.iss.fire) {
    valid := !tail
    head := false.B
  }

  io.vat := inst.vat
  io.seq_hazard.valid := valid
  io.seq_hazard.bits.rintent := hazardMultiply(rvd_mask | rvm_mask)
  io.seq_hazard.bits.wintent := 0.U
  io.seq_hazard.bits.vat := inst.vat

  val vd_read_oh = UIntToOH(io.rvd.bits.eg)
  val vm_read_oh = Mux(renvm, UIntToOH(io.rvm.bits.eg), 0.U)

  val raw_hazard = ((vm_read_oh | vd_read_oh) & io.older_writes) =/= 0.U
  val data_hazard = raw_hazard

  val oldest = inst.vat === io.vat_head

  io.rvd.valid := valid && io.iss.ready
  io.rvd.bits.eg := getEgId(inst.rd + (sidx << inst.emul), eidx, inst.mem_elem_size, false.B)
  io.rvd.bits.oldest := oldest
  io.rvm.valid := valid && renvm && io.iss.ready
  io.rvm.bits.eg := getEgId(0.U, eidx, 0.U, true.B)
  io.rvm.bits.oldest := oldest

  io.iss.valid := valid && !data_hazard && (!renvm || io.rvm.ready) && io.rvd.ready
  io.iss.bits.use_stmask := renvm
  io.iss.bits.eidx := eidx
  io.iss.bits.elem_size := inst.mem_elem_size
  io.iss.bits.debug_id := inst.debug_id
  io.iss.bits.tail := tail
  io.iss.bits.vat := inst.vat

  val head_mask = get_head_mask(~(0.U(mLenB.W)), eidx     , inst.mem_elem_size, mLen)
  val tail_mask = get_tail_mask(~(0.U(mLenB.W)), next_eidx, inst.mem_elem_size, mLen)
  io.iss.bits.eidx_mask := head_mask & tail_mask

  when (io.iss.fire && !tail) {
    if (vParams.enableChaining) {
      when (next_is_new_eg(eidx, next_eidx, inst.mem_elem_size, false.B)) {
        rvd_mask := rvd_mask & ~UIntToOH(io.rvd.bits.eg)
      }
      when (next_is_new_eg(eidx, next_eidx, 0.U, true.B)) {
        rvm_mask := rvm_mask & ~UIntToOH(io.rvm.bits.eg)
      }
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
