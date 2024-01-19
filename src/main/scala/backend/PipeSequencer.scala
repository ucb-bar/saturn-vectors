package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tile.{CoreModule}
import vector.common._

abstract class PipeSequencer[T <: Data](issType: T)(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {

  val io = IO(new Bundle {
    val dis = Flipped(Decoupled(new BackendIssueInst))

    val seq_hazard = Output(Valid(new SequencerHazard))

    val vat = Output(UInt(vParams.vatSz.W))
    val older_writes = Input(UInt(egsTotal.W))
    val older_reads  = Input(UInt(egsTotal.W))

    val busy = Output(Bool())

    val rvs1 = new VectorReadIO
    val rvs2 = new VectorReadIO
    val rvd  = new VectorReadIO
    val rvm  = new VectorReadIO
    val perm = new Bundle {
      val req = Decoupled(new CompactorReq(dLenB))
      val data = Input(UInt(dLen.W))
    }

    val iss = Decoupled(issType)
    val sub_dlen = Input(UInt(log2Ceil(dLenB).W))

    val acc = Input(Valid(new VectorWrite(dLen)))
  })
  def accepts(inst: VectorIssueInst): Bool

  def min(a: UInt, b: UInt) = Mux(a > b, b, a)
  def get_head_mask(bit_mask: UInt, eidx: UInt, eew: UInt) = bit_mask << (eidx << eew)(dLenOffBits-1,0)
  def get_tail_mask(bit_mask: UInt, eidx: UInt, eew: UInt) = bit_mask >> (0.U(dLenOffBits.W) - (eidx << eew)(dLenOffBits-1,0))
  def get_vm_mask(mask_resp: UInt, eidx: UInt, eew: UInt) = {
    val vm_off  = ((1 << dLenOffBits) - 1).U(log2Ceil(dLen).W)
    val vm_eidx = eidx & ~(vm_off >> eew)(log2Ceil(dLen)-1,0)
    val vm_resp = mask_resp >> vm_eidx
    Mux1H(UIntToOH(eew), (0 until 4).map { w => FillInterleaved(1 << w, vm_resp) })
  }
  def get_next_eidx(vl: UInt, eidx: UInt, eew: UInt, sub_dlen: UInt, reads_mask: Bool) = {
    val next = Mux(reads_mask,
      eidx +& dLen.U,
      (((eidx >> (dLenOffBits.U - eew - sub_dlen)) +& 1.U) << (dLenOffBits.U - eew - sub_dlen))(log2Ceil(maxVLMax)+1,0)
    )
    min(vl, next)
  }
  def next_is_new_eg(eidx: UInt, next_eidx: UInt, eew: UInt, masked: Bool) = {
    val offset = Mux(masked, log2Ceil(dLen).U, dLenOffBits.U - eew)
    (next_eidx >> offset) =/= (eidx >> offset)
  }


  io.rvs1.req.valid := false.B
  io.rvs1.req.bits := DontCare
  io.rvs2.req.valid := false.B
  io.rvs2.req.bits := DontCare
  io.rvd.req.valid := false.B
  io.rvd.req.bits := DontCare
  io.rvm.req.valid := false.B
  io.rvm.req.bits := DontCare
  io.perm.req.valid := false.B
  io.perm.req.bits := DontCare
}
