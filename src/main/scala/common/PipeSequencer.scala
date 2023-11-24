package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

abstract class PipeSequencer(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val dis = new Bundle {
      val fire = Input(Bool())
      val ready = Output(Bool())
      val inst = Input(new VectorIssueInst)
    }

    val seq_hazards = new Bundle {
      val valid = Output(Bool())
      val rintent = Output(UInt(egsTotal.W))
      val wintent = Output(UInt(egsTotal.W))
      val vat = Output(UInt(vParams.vatSz.W))

      val writes = Input(UInt(egsTotal.W))
      val reads = Input(UInt(egsTotal.W))
    }

    val busy = Output(Bool())

    val rvs1 = new VectorReadIO
    val rvs2 = new VectorReadIO
    val rvd  = new VectorReadIO
    val rvm  = new VectorReadIO

    val iss = Decoupled(new VectorMicroOp)
  })
  def min(a: UInt, b: UInt) = Mux(a > b, b, a)
  def get_group_mask(log2mul: UInt, max: Int) = Mux1H((0 until max).map { i =>
    (i.U === log2mul, ~(((1 << i) - 1).U(5.W)))
  })
  def get_arch_mask(reg: UInt, mask: UInt) = VecInit.tabulate(32) { i => (i.U & mask) === (reg & mask) }.asUInt
  def get_head_mask(bit_mask: UInt, eidx: UInt, eew: UInt) = bit_mask << (eidx << eew)(dLenOffBits-1,0)
  def get_tail_mask(bit_mask: UInt, eidx: UInt, eew: UInt) = bit_mask >> (0.U(dLenOffBits.W) - (eidx << eew)(dLenOffBits-1,0))
  def get_vm_mask(mask_resp: UInt, eidx: UInt, eew: UInt) = {
    val vm_off  = ((1 << dLenOffBits) - 1).U(log2Ceil(dLen).W)
    val vm_eidx = eidx & ~(vm_off >> eew)(log2Ceil(dLen)-1,0)
    val vm_resp = mask_resp >> vm_eidx
    Mux1H(UIntToOH(eew), (0 until 4).map { w => FillInterleaved(1 << w, vm_resp) })
  }
  def get_next_eidx(vl: UInt, eidx: UInt, eew: UInt, sub_dlen: UInt) = min(vl,
    (((eidx >> (dLenOffBits.U - eew - sub_dlen)) + 1.U) << (dLenOffBits.U - eew - sub_dlen))(log2Ceil(maxVLMax)+1,0)
  )
  def next_mask_is_new_eg(eidx: UInt, next_eidx: UInt) = (eidx >> log2Ceil(dLen)) =/= (next_eidx >> (log2Ceil(dLen)))
  def next_is_new_eg(eidx: UInt, next_eidx: UInt, eew: UInt) = (next_eidx >> (dLenOffBits.U - eew)) =/= (eidx >> (dLenOffBits.U - eew))

  io.rvs1.req.valid := false.B
  io.rvs1.req.bits := DontCare
  io.rvs2.req.valid := false.B
  io.rvs2.req.bits := DontCare
  io.rvd.req.valid := false.B
  io.rvd.req.bits := DontCare
  io.rvm.req.valid := false.B
  io.rvm.req.bits := DontCare
}
