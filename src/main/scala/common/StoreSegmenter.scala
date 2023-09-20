package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._


class StoreSegmenter(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val compactor = Decoupled(new CompactorReq)
    val compactor_data = Output(UInt(dLen.W))

    val seg = Flipped(Decoupled(new SegmenterReq))
    val stdata = Flipped(Decoupled(UInt(dLen.W)))
  })

  val transpose = Module(new StoreMultiTransposeArray(dLenB))

  val sidx = RegInit(0.U(3.W))

  transpose.io.in.valid := io.seg.valid && io.seg.bits.nf =/= 0.U && io.stdata.valid
  transpose.io.in.bits.eew := io.seg.bits.eew
  transpose.io.in.bits.tail := sidx === io.seg.bits.nf
  transpose.io.in.bits.elems := Mux(io.seg.bits.tail, io.seg.bits.tail_align, dLenB.U) >> io.seg.bits.eew
  transpose.io.in.bits.wdata := io.stdata.bits.asUInt
  transpose.io.out.ready := io.compactor.ready

  io.seg.ready := Mux(io.seg.bits.nf === 0.U,
    io.stdata.valid && io.compactor.ready,
    io.stdata.valid && transpose.io.in.ready && sidx === io.seg.bits.nf)

  io.stdata.ready := io.seg.valid && Mux(io.seg.bits.nf === 0.U,
    io.compactor.ready,
    transpose.io.in.ready)

  io.compactor.valid := Mux(transpose.io.busy,
    transpose.io.out.valid,
    io.stdata.valid && io.seg.valid)
  io.compactor.bits.head := 0.U
  io.compactor.bits.tail := Mux(transpose.io.busy,
    transpose.io.out.bits.count,
    Mux(io.seg.bits.tail, io.seg.bits.tail_align, 0.U))
  io.compactor_data := Mux(transpose.io.busy,
    transpose.io.out.bits.data.asUInt,
    io.stdata.bits)

  when (transpose.io.in.fire && io.seg.bits.nf =/= 0.U) {
    sidx := sidx + 1.U
    when (sidx === io.seg.bits.nf) {
      sidx := 0.U
    }
  }

}
