package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

class SegmenterReq(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val nf = UInt(3.W)
  val eew = UInt(2.W)
  val eidx = UInt(log2Ceil(maxVLMax).W)
  val tail_align = UInt(log2Ceil(dLenB).W)
  val tail = Bool()
}

class SegmentGen(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val done = Output(Bool())
    val inst = Input(new VectorIssueInst)
    val seg = Decoupled(new SegmenterReq)
  })

  val r_eidx = Reg(UInt(log2Ceil(maxVLMax).W))
  val r_head = RegInit(true.B)

  val eidx = Mux(r_head, io.inst.vstart, r_eidx)

  val next_eidx = eidx +& Mux(io.inst.nf =/= 0.U, 1.U, dLenB.U >> io.inst.mem_size)

  io.done := false.B
  io.seg.valid := io.valid
  io.seg.bits.nf := io.inst.nf
  io.seg.bits.eew := io.inst.mem_size
  io.seg.bits.eidx := eidx
  io.seg.bits.tail := next_eidx >= io.inst.vconfig.vl
  io.seg.bits.tail_align := io.inst.vconfig.vl << io.inst.mem_size

  when (io.seg.fire()) {
    r_head := io.done
    r_eidx := next_eidx
    io.done := next_eidx >= io.inst.vconfig.vl
  }
}
