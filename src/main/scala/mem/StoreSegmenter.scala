package saturn.mem

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import saturn.common._

class StoreSegmenter(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val done = Output(Bool())
    val op = Input(new VectorMemMacroOp)

    val compactor = Decoupled(new CompactorReq(mLenB))
    val compactor_data = Output(Vec(mLenB, new MaskedByte))
    val stdata = Flipped(Decoupled(new VectorStoreData))
  })

  val segbuf = Module(new StoreSegmentBuffer(vParams.doubleBufferSegments))

  val r_eidx = Reg(UInt(log2Ceil(maxVLMax).W))
  val r_head = RegInit(true.B)
  val r_sidx = Reg(UInt(3.W))
  val eidx = Mux(r_head, io.op.vstart, r_eidx)
  val sidx = Mux(r_head, 0.U, r_sidx)

  val mem_size = io.op.elem_size
  val sub_mlen = Mux(io.op.seg_nf =/= 0.U && (log2Ceil(mLenB).U > (3.U +& mem_size)),
    log2Ceil(mLenB).U - 3.U - mem_size,
    0.U)
  val eidx_incr = (mLenB.U - ((eidx << (mem_size +& sub_mlen))(mLenOffBits-1,0))) >> (mem_size +& sub_mlen)
  val next_eidx = eidx +& eidx_incr
  val next_sidx = sidx +& 1.U

  val sidx_tail = next_sidx > io.op.seg_nf
  val eidx_tail = next_eidx >= io.op.vl

  when (io.valid && io.stdata.valid) {
    assert(io.stdata.bits.debug_id === io.op.debug_id)
  }

  io.stdata.ready := io.valid && Mux(io.op.seg_nf === 0.U,
    !segbuf.io.busy && io.compactor.ready,
    segbuf.io.in.ready)

  segbuf.io.in.valid := io.valid && io.op.seg_nf =/= 0.U && io.stdata.valid
  segbuf.io.in.bits.data := io.stdata.bits.stdata >> ((eidx << mem_size)(mLenOffBits-1,0) << 3)
  segbuf.io.in.bits.mask := io.stdata.bits.stmask >> (eidx << mem_size)(mLenOffBits-1,0)
  segbuf.io.in.bits.eew := mem_size
  segbuf.io.in.bits.nf := io.op.nf
  segbuf.io.in.bits.rows := Mux(next_eidx >= io.op.vl, (io.op.vl - eidx), eidx_incr)
  segbuf.io.in.bits.sidx := sidx
  segbuf.io.in.bits.segstart := io.op.segstart
  segbuf.io.in.bits.segend := io.op.seg_nf
  segbuf.io.in.bits.debug_id := io.op.debug_id

  io.compactor.valid := Mux(segbuf.io.busy,
    segbuf.io.out.valid,
    io.stdata.valid && io.valid && io.op.seg_nf === 0.U)
  io.compactor_data := Mux(segbuf.io.busy,
    segbuf.io.out.bits.data, io.stdata.bits).asMaskedBytes
  io.compactor.bits.head := Mux(segbuf.io.busy,
    segbuf.io.out.bits.head, eidx << mem_size)
  io.compactor.bits.tail := Mux(segbuf.io.busy,
    segbuf.io.out.bits.tail, Mux(eidx_tail, io.op.vl << mem_size, 0.U))

  segbuf.io.out.ready := io.compactor.ready

  io.done := false.B
  when (io.stdata.fire) {
    r_head := false.B
    when (io.op.seg_nf =/= 0.U && !sidx_tail) {
      when (r_head) { r_eidx := io.op.vstart }
      r_sidx := next_sidx
    } .otherwise {
      r_eidx := next_eidx
      r_sidx := 0.U
      io.done := eidx_tail
      r_head := eidx_tail
    }
  }
}

