package saturn.backend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import saturn.common._

class LoadSequencerIO(implicit p: Parameters) extends SequencerIO(new LoadRespMicroOp) {
  val rvm  = Decoupled(new VectorReadReq)

  // sf.vtle32 writes OPU tile state (see Backend.scala): while OuterProductSequencer is
  // busy, a new tile load must not start (it could race an in-flight sf.mm/sf.vtzero.t/
  // sf.vtmv.*/sf.vtse32 touching the same tile). Ordinary loads are unaffected.
  val opu_busy = Input(Bool())
  val tile_ld_busy = Output(Bool())
}

class LoadSequencer(implicit p: Parameters) extends Sequencer[LoadRespMicroOp]()(p) {
  def accepts(inst: VectorIssueInst) = inst.vmu && !inst.opcode(5)

  val io = IO(new LoadSequencerIO)
  val tssIdxBits = log2Ceil(opuTE max 1) max 1

  val valid = RegInit(false.B)
  val inst  = Reg(new BackendIssueInst)
  val eidx  = Reg(UInt(log2Ceil(maxVLMax).W))
  val sidx  = Reg(UInt(3.W))
  val wvd_mask = Reg(UInt(egsTotal.W))
  val rvm_mask = Reg(UInt(egsPerVReg.W))
  val head     = Reg(Bool())
  // sf.vtle32: destination is OPU tile state (see Backend.scala), addressed by a TSS
  // latched from rs2 at dispatch (bit positions match OuterProductSequencer's TSS decode)
  val tile     = Reg(UInt(2.W))
  val tile_col = Reg(Bool())
  val tss_idx  = Reg(UInt(tssIdxBits.W))

  val renvm     = !inst.vm
  val next_eidx = get_next_eidx(inst.vconfig.vl, eidx, inst.mem_elem_size, 0.U, false.B, false.B, mLen)
  val tail      = next_eidx === inst.vconfig.vl && sidx === inst.seg_nf

  // Only the resident-instruction-derived interlock belongs in io.dis.ready: this
  // backend picks which sequencer accepts an instruction based on io.dis.ready itself
  // (see chosen_seq/ready_seqs in Backend.scala), so anything computed here from
  // io.dis.bits/io.dis.valid closes a combinational cycle. The opu_busy/sf.vtle32 check
  // is instead applied to io.iss.valid below, once inst.tile_ld is a clean registered
  // value (and io.dis.bits itself can otherwise be X while io.dis.valid is false).
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
    // sf.vtle32 has no VRF destination register (rd/vd is fixed to 0 in its encoding)
    wvd_mask := Mux(iss_inst.tile_ld, 0.U, FillInterleaved(egsPerVReg, wvd_arch_mask.asUInt))
    rvm_mask := Mux(!iss_inst.vm, ~(0.U(egsPerVReg.W)), 0.U)
    val tss = iss_inst.rs2_data
    tile     := tss(30, 29)
    tile_col := tss(24)
    tss_idx  := tss(tssIdxBits-1, 0)
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

  val vm_read_oh  = Mux(renvm, UIntToOH(io.rvm.bits.eg), 0.U)
  val vd_write_oh = UIntToOH(io.iss.bits.wvd_eg)

  val raw_hazard = (vm_read_oh & io.older_writes) =/= 0.U
  val waw_hazard = (vd_write_oh & io.older_writes) =/= 0.U
  val war_hazard = (vd_write_oh & io.older_reads) =/= 0.U
  val data_hazard = raw_hazard || waw_hazard || war_hazard

  io.rvm.valid := valid && renvm
  io.rvm.bits.eg := getEgId(0.U, eidx, 0.U, true.B)
  io.rvm.bits.oldest := inst.vat === io.vat_head

  // sf.vtle32 (inst.tile_ld, a clean registered value once resident): don't let this
  // instruction's response start writing OPU tile state while OuterProductSequencer may
  // still be busy -- see io.dis.ready's comment for why this isn't checked there.
  io.iss.valid := valid && !data_hazard && (!renvm || io.rvm.ready) && !(inst.tile_ld && io.opu_busy)
  io.iss.bits.wvd_eg    := getEgId(inst.rd + (sidx << inst.emul), eidx, inst.mem_elem_size, false.B)
  io.iss.bits.tail       := tail
  io.iss.bits.vat        := inst.vat
  io.iss.bits.debug_id   := inst.debug_id
  io.iss.bits.eidx       := eidx

  val head_mask = get_head_mask(~(0.U(dLenB.W)), eidx     , inst.mem_elem_size, dLen)
  val tail_mask = get_tail_mask(~(0.U(dLenB.W)), next_eidx, inst.mem_elem_size, dLen)
  io.iss.bits.eidx_wmask := Mux(sidx > inst.segend && inst.seg_nf =/= 0.U, 0.U, head_mask & tail_mask)
  io.iss.bits.use_rmask := renvm
  io.iss.bits.elem_size := inst.mem_elem_size
  io.iss.bits.tile_ld := inst.tile_ld
  io.iss.bits.tile := tile
  io.iss.bits.tile_col := tile_col
  io.iss.bits.tss_idx := tss_idx

  when (io.iss.fire && !tail) {
    if (vParams.enableChaining) {
      when (next_is_new_eg(eidx, next_eidx, inst.mem_elem_size, false.B)) {
        wvd_mask := wvd_mask & ~vd_write_oh
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
  io.tile_ld_busy := valid && inst.tile_ld
}
