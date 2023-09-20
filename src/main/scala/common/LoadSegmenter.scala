package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

class LoadSegmenter(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val compactor = Decoupled(new CompactorReq)
    val compactor_data = Input(UInt(dLen.W))

    val seg = Flipped(Decoupled(new SegmenterReq))

    val resp = Decoupled(UInt(dLen.W))
  })

  val segbuf = Module(new LoadSegmentBuffer)

  val sidx = RegInit(0.U(3.W))
  val elems_per_dlen = dLenB.U >> io.seg.bits.eew
  val remaining_bytes = Mux(io.seg.bits.nf =/= 0.U,
    (io.seg.bits.nf - sidx +& 1.U) << io.seg.bits.eew,
    Mux(io.seg.bits.tail, io.seg.bits.tail_align, 0.U))
  val last = remaining_bytes <= dLenB.U

  io.compactor.valid := io.seg.valid && (io.seg.bits.nf === 0.U && !segbuf.io.busy || segbuf.io.in.ready)
  io.compactor.bits.head := Mux(io.seg.bits.nf === 0.U, io.seg.bits.eidx << io.seg.bits.eew, 0.U)
  io.compactor.bits.tail := Mux(remaining_bytes >= dLenB.U, 0.U, remaining_bytes)

  segbuf.io.in.valid := io.seg.valid && io.seg.bits.nf =/= 0.U && io.compactor.ready
  segbuf.io.in.bits.eew := io.seg.bits.eew
  segbuf.io.in.bits.nf := io.seg.bits.nf
  segbuf.io.in.bits.data := io.compactor_data
  segbuf.io.in.bits.eidx := io.seg.bits.eidx
  segbuf.io.in.bits.sidx := sidx
  segbuf.io.in.bits.sidx_tail := last
  segbuf.io.in.bits.tail := io.seg.bits.tail

  when (segbuf.io.in.fire) {
    sidx := sidx + elems_per_dlen
    when (last) {
      sidx := 0.U
    }
  }

  segbuf.io.out.ready := io.resp.ready

  io.resp.valid := Mux(segbuf.io.busy,
    segbuf.io.out.valid,
    io.compactor.ready && io.seg.valid && io.seg.bits.nf === 0.U)
  io.resp.bits := Mux(segbuf.io.busy, segbuf.io.out.bits, io.compactor_data)

  io.seg.ready := Mux(io.seg.bits.nf === 0.U,
    !segbuf.io.busy && io.compactor.ready && io.resp.ready,
    segbuf.io.in.ready && io.compactor.ready && last)
}
