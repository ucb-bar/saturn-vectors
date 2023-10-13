package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._


class StoreSegmenter(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val done = Output(Bool())
    val inst = Input(new VectorIssueInst)

    val compactor = Decoupled(new CompactorReq(dLenB))
    val compactor_data = Output(UInt(dLen.W))
    val stdata = Flipped(Decoupled(UInt(dLen.W)))
  })

  val segbuf = Module(new StoreSegmentBuffer)

  val r_eidx = Reg(UInt(log2Ceil(maxVLMax).W))
  val r_head = RegInit(true.B)
  val eidx = Mux(r_head, io.inst.vstart, r_eidx)
  val sidx = RegInit(0.U(3.W))

  val mem_size = io.inst.mem_elem_size
  val sub_dlen = Mux(io.inst.seg_nf =/= 0.U && (log2Ceil(dLenB).U > (3.U +& mem_size)),
    log2Ceil(dLenB).U - 3.U - mem_size,
    0.U)
  val eidx_incr = (dLenB.U >> (mem_size +& sub_dlen))
  val next_eidx = eidx +& eidx_incr
  val next_sidx = sidx +& 1.U

  val sidx_tail = next_sidx > io.inst.seg_nf
  val eidx_tail = next_eidx >= io.inst.vconfig.vl

  io.stdata.ready := io.valid && Mux(io.inst.seg_nf === 0.U,
    !segbuf.io.busy && io.compactor.ready,
    segbuf.io.in.ready)

  segbuf.io.in.valid := io.valid && io.inst.seg_nf =/= 0.U && io.stdata.valid
  segbuf.io.in.bits.data := io.stdata.bits >> ((eidx << mem_size)(dLenOffBits-1,0) << 3)
  segbuf.io.in.bits.eew := mem_size
  segbuf.io.in.bits.nf := io.inst.nf
  segbuf.io.in.bits.rows := Mux(next_eidx >= io.inst.vconfig.vl, (io.inst.vconfig.vl - eidx), eidx_incr)
  segbuf.io.in.bits.sidx := sidx


  io.compactor.valid := Mux(segbuf.io.busy,
    segbuf.io.out.valid,
    io.stdata.valid && io.valid && io.inst.seg_nf === 0.U)
  io.compactor_data := Mux(segbuf.io.busy,
    segbuf.io.out.bits.data, io.stdata.bits)
  io.compactor.bits.head := Mux(segbuf.io.busy,
    segbuf.io.out.bits.head, eidx << mem_size)
  io.compactor.bits.tail := Mux(segbuf.io.busy,
    segbuf.io.out.bits.tail, Mux(eidx_tail, io.inst.vconfig.vl << mem_size, 0.U))

  segbuf.io.out.ready := io.compactor.ready

  io.done := false.B
  when (io.stdata.fire) {
    when (io.inst.seg_nf =/= 0.U && !sidx_tail) {
      sidx := next_sidx
    } .otherwise {
      r_eidx := next_eidx
      sidx := 0.U
      io.done := eidx_tail
      r_head := eidx_tail
    }
  }
}
