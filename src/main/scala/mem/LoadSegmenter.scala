package saturn.mem

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import saturn.common._

class LoadSegmenter(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val done = Output(Bool())
    val op = Input(new VectorMemMacroOp)

    val compactor = Decoupled(new CompactorReq(mLenB))
    val compactor_data = Input(UInt(mLen.W))

    val resp = Decoupled(new Bundle {
      val data = UInt(mLen.W)
      val debug_id = UInt(debugIdSz.W)
    })
  })

  val segbuf = Module(new LoadSegmentBuffer(vParams.doubleBufferSegments))

  val r_eidx = Reg(UInt(log2Ceil(maxVLMax).W))
  val r_head = RegInit(true.B)
  val r_sidx = Reg(UInt(3.W))
  val eidx = Mux(r_head, io.op.vstart, r_eidx)
  val sidx = Mux(r_head, io.op.segstart, r_sidx)

  val mem_size = io.op.elem_size
  val incr = (mLenB.U - (Mux(io.op.seg_nf === 0.U, eidx, sidx) << mem_size)(mLenOffBits-1,0)) >> mem_size
  val eidx_incr = Mux(io.op.seg_nf =/= 0.U, 1.U, incr)
  val sidx_incr = incr
  val next_eidx = eidx +& eidx_incr
  val next_sidx = sidx +& sidx_incr

  val sidx_tail = next_sidx > io.op.seg_nf
  val eidx_tail = next_eidx >= io.op.vl

  when (io.op.seg_nf === 0.U) {
    io.compactor.valid := io.valid && !segbuf.io.busy && io.resp.ready
    io.compactor.bits.head := eidx << mem_size
    io.compactor.bits.tail := Mux(eidx_tail, io.op.vl << mem_size, 0.U)
  } .otherwise {
    io.compactor.valid := io.valid && segbuf.io.in.ready
    io.compactor.bits.head := sidx << mem_size
    io.compactor.bits.tail := Mux(sidx_tail, (io.op.nf +& 1.U) << mem_size, 0.U)
  }

  segbuf.io.in.valid := io.valid && io.op.seg_nf =/= 0.U && io.compactor.ready
  segbuf.io.in.bits.eew := mem_size
  segbuf.io.in.bits.nf := io.op.nf
  segbuf.io.in.bits.data := io.compactor_data
  segbuf.io.in.bits.eidx := eidx
  segbuf.io.in.bits.sidx := sidx
  segbuf.io.in.bits.sidx_tail := sidx_tail
  segbuf.io.in.bits.tail := eidx_tail
  segbuf.io.in.bits.segstart := io.op.segstart
  segbuf.io.in.bits.debug_id := io.op.debug_id

  segbuf.io.out.ready := io.resp.ready

  io.resp.valid := Mux(segbuf.io.busy,
    segbuf.io.out.valid,
    io.compactor.ready && io.valid && io.op.seg_nf === 0.U)
  io.resp.bits.data := Mux(segbuf.io.busy, segbuf.io.out.bits.data, io.compactor_data)
  io.resp.bits.debug_id := Mux(segbuf.io.busy, segbuf.io.out.bits.debug_id, io.op.debug_id)


  val seg_ready = Mux(io.op.seg_nf === 0.U,
    !segbuf.io.busy && io.compactor.ready && io.resp.ready,
    segbuf.io.in.ready && io.compactor.ready && sidx_tail)

  when (segbuf.io.in.fire) {

    r_head := false.B
    when (r_head) { r_eidx := io.op.vstart }
    r_sidx := next_sidx
    when (next_sidx > io.op.nf) {
      r_sidx := 0.U
    }
  }
  io.done := false.B
  when (seg_ready && io.valid) {
    r_head := eidx_tail
    r_eidx := next_eidx
    io.done := eidx_tail
  }
}
