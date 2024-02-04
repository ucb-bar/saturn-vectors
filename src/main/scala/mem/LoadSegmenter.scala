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

    val compactor = Decoupled(new CompactorReq(dLenB))
    val compactor_data = Input(UInt(dLen.W))

    val resp = Decoupled(UInt(dLen.W))
  })

  val segbuf = Module(new LoadSegmentBuffer(vParams.doubleBufferSegments))

  val r_eidx = Reg(UInt(log2Ceil(maxVLMax).W))
  val r_head = RegInit(true.B)
  val eidx = Mux(r_head, io.op.vstart, r_eidx)
  val sidx = RegInit(0.U(3.W))

  val mem_size = io.op.elem_size
  val eidx_incr = Mux(io.op.seg_nf =/= 0.U, 1.U,(dLenB.U - (eidx << mem_size)(dLenOffBits-1,0)) >> mem_size)
  val next_eidx = eidx +& eidx_incr
  val next_sidx = sidx +& (dLenB.U >> mem_size)

  val sidx_tail = next_sidx > io.op.seg_nf
  val eidx_tail = next_eidx >= io.op.vl

  when (io.op.seg_nf === 0.U) {
    io.compactor.valid := io.valid && !segbuf.io.busy && io.resp.ready
    io.compactor.bits.head := eidx << mem_size
    io.compactor.bits.tail := Mux(eidx_tail, io.op.vl << mem_size, 0.U)
  } .otherwise {
    io.compactor.valid := io.valid && segbuf.io.in.ready
    io.compactor.bits.head := 0.U
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

  segbuf.io.out.ready := io.resp.ready

  io.resp.valid := Mux(segbuf.io.busy,
    segbuf.io.out.valid,
    io.compactor.ready && io.valid && io.op.seg_nf === 0.U)
  io.resp.bits := Mux(segbuf.io.busy, segbuf.io.out.bits, io.compactor_data)

  when (segbuf.io.in.fire) {
    sidx := next_sidx
    when (next_sidx > io.op.nf) {
      sidx := 0.U
    }
  }

  val seg_ready = Mux(io.op.seg_nf === 0.U,
    !segbuf.io.busy && io.compactor.ready && io.resp.ready,
    segbuf.io.in.ready && io.compactor.ready && sidx_tail)

  io.done := false.B
  when (seg_ready && io.valid) {
    r_head := eidx_tail
    r_eidx := next_eidx
    io.done := eidx_tail
  }
}
