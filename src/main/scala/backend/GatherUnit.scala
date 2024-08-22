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
  val req = Flipped(Decoupled(new CompactorReq(dLenB)))
  val data = Output(UInt(dLen.W))

  //val gather = Flipped(Decoupled(new GatherOp))
}

class GatherUnit(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val vxs = new GatherToExecuteIO
    val vps = Flipped(Decoupled(new PermuteMicroOpWithData))
  })

  val q = Module(new DCEQueue(new PermuteMicroOpWithData, 1))
  val buffer = Module(new Compactor(dLenB, dLenB, UInt(8.W), false))

  io.vps.ready := q.io.enq.ready
  q.io.enq.valid := io.vps.valid && !io.vps.bits.vmu
  q.io.enq.bits := io.vps.bits

  q.io.deq.ready := buffer.io.push.ready

  buffer.io.push.valid := q.io.deq.valid
  buffer.io.push.bits.head := q.io.deq.bits.eidx << q.io.deq.bits.rvs2_eew
  buffer.io.push.bits.tail := Mux(q.io.deq.bits.tail,
    q.io.deq.bits.vl << q.io.deq.bits.rvs2_eew,
    0.U)
  buffer.io.push_data := q.io.deq.bits.rvs2_data.asTypeOf(Vec(dLenB, UInt(8.W)))

  buffer.io.pop <> io.vxs.req
  io.vxs.data := buffer.io.pop_data.asUInt

  // // Handle gathers
  // class IndexBundle extends Bundle {

  // }

  // val fired_bytes = RegInit(0.U(dLenB.W))

  // val idx_eew = q.io.deq.bits.rvs2_eew
  // val vd_eew = q.io.deq.bits.vtype.vsew

  // val 

  // val eidxs = (0 until 4).map { eew =>
  //   q.io.deq.bits.rvs2_data.asTypeOf(Vec(dLenB >> eew, UInt((8 << eew).W)))
  // }
  // val byte_eidxs = (0 until 4).map { eew =>
  //   eidxs(eew).map(e => Seq.fill((1 << eew), e)).flatten
  // }

  // val byte_eidxs = VecInit((0 until 4).map { eew =>
  //   val 
  //   q.io.deq.bits.rvs2_data.asTypeOf(Vec(dLenB >> eew, UInt((8 << eew).W)))
  // })(vd_eew)
  // byte_eidxs.map(_ << q.io.deq.bits.vtype.vsew)

  // val offsets = (0 until dLenB).map { b =>
  //   val eidx_offset = b >> vd_eew
  //   val byte_offset = b.U & ((1.U << vd_eew) - 1.U)
  //   val eidx = eidx +& eidx_offset
  // }
  // val byte_offsets = VecInit((0 until 4).map { eew =>
  //   val
  // })(vd_eew)

  // val max_byte_offset = q.io.deq.bits.vtype.vlMax << q.io.deq.bits.vtype.vsew

  // val byte_zeros = byte_offsets.map(_ >= max_byte_offset)





}
