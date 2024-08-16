package saturn.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tile.{CoreModule, CoreBundle}
import saturn.common._

class SequencerIO[T <: Data](issType: T)(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  // From issue queue
  val dis = Flipped(Decoupled(new BackendIssueInst))
  val dis_stall = Input(Bool()) // used to disable OOO

  // Emits pending reads/writes + age
  val seq_hazard = Output(Valid(new SequencerHazard))
  val vat = Output(UInt(vParams.vatSz.W))

  // Consumes older reads/writes
  val older_writes = Input(UInt(egsTotal.W))
  val older_reads  = Input(UInt(egsTotal.W))

  // Used to determine when this is the oldest insn
  val vat_head = Input(UInt(vParams.vatSz.W))

  val busy = Output(Bool())
  val head = Output(Bool())

  // Issued operation
  val iss = Decoupled(issType)
}

abstract class Sequencer[T <: Data](implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {

  val io: SequencerIO[T]

  def accepts(inst: VectorIssueInst): Bool

  def min(a: UInt, b: UInt) = Mux(a > b, b, a)
  def get_max_offset(offset: UInt): UInt = min(offset, maxVLMax.U)(log2Ceil(maxVLMax),0)
  def get_head_mask(bit_mask: UInt, eidx: UInt, eew: UInt) = bit_mask << (eidx << eew)(dLenOffBits-1,0)
  def get_tail_mask(bit_mask: UInt, eidx: UInt, eew: UInt) = bit_mask >> (0.U(dLenOffBits.W) - (eidx << eew)(dLenOffBits-1,0))
  def get_next_eidx(vl: UInt, eidx: UInt, eew: UInt, sub_dlen: UInt, reads_mask: Bool, elementwise: Bool) = {
    val next = Wire(UInt((1+log2Ceil(maxVLMax)).W))
    next := Mux(elementwise, eidx +& 1.U, Mux(reads_mask,
      eidx +& dLen.U,
      (((eidx >> (dLenOffBits.U - eew - sub_dlen)) +& 1.U) << (dLenOffBits.U - eew - sub_dlen))
    ))
    min(vl, next)
  }
  def next_is_new_eg(eidx: UInt, next_eidx: UInt, eew: UInt, masked: Bool) = {
    val offset = Mux(masked, log2Ceil(dLen).U, dLenOffBits.U - eew)
    (next_eidx >> offset) =/= (eidx >> offset)
  }
}
