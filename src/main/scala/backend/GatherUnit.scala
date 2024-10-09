package saturn.backend

import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import saturn.mem._
import saturn.exu._
import saturn.common._
import saturn.insns._


class GatherOp(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val sels = Vec(dLenB, UInt(log2Ceil(dLenB).W))
  val mask = UInt(dLenB.W)
  val zero = UInt(dLenB.W)
  val incr_dst = Bool()
  val src_eg = UInt(log2Ceil(egsTotal).W)
}

class GatherToExecuteIO(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val slide_req = Flipped(Decoupled(new CompactorReq(dLenB)))
  val slide_data = Output(UInt(dLen.W))

  val gather_eidx = Decoupled(UInt(64.W))
}

class GatherUnit(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val vxs = new GatherToExecuteIO
    val vps = Flipped(Decoupled(new PermuteMicroOpWithData))
  })

  val q = Module(new DCEQueue(new PermuteMicroOpWithData, 2))
  val slide_buffer = Module(new Compactor(dLenB, dLenB, UInt(8.W), false))
  val eidx_buffer = Module(new Queue(UInt(64.W), 2))

  io.vps.ready := q.io.enq.ready
  q.io.enq.valid := io.vps.valid && !io.vps.bits.vmu
  q.io.enq.bits := io.vps.bits
  io.vxs.gather_eidx <> eidx_buffer.io.deq

  q.io.deq.ready := Mux(q.io.deq.bits.slide, slide_buffer.io.push.ready, eidx_buffer.io.enq.ready)

  slide_buffer.io.push.valid := q.io.deq.valid && q.io.deq.bits.slide
  slide_buffer.io.push.bits.head := q.io.deq.bits.eidx << q.io.deq.bits.rvs2_eew
  slide_buffer.io.push.bits.tail := Mux(q.io.deq.bits.tail,
    q.io.deq.bits.vl << q.io.deq.bits.rvs2_eew,
    0.U)
  slide_buffer.io.push_data := q.io.deq.bits.rvs2_data.asTypeOf(Vec(dLenB, UInt(8.W)))

  eidx_buffer.io.enq.valid := q.io.deq.valid && !q.io.deq.bits.slide
  eidx_buffer.io.enq.bits := extractElem(q.io.deq.bits.rvs2_data, q.io.deq.bits.rvs2_eew, q.io.deq.bits.eidx)

  slide_buffer.io.pop <> io.vxs.slide_req
  io.vxs.slide_data := slide_buffer.io.pop_data.asUInt
}
