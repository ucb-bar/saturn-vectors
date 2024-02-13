package saturn.mem

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._

import saturn.common._

class TLInterface(implicit p: Parameters) extends LazyModule()(p) with HasCoreParameters with HasVectorParams {
  val node = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(
    name      = "Vector Store Unit",
    sourceId  = IdRange(0, (2 << dmemTagBits))
  )))))

  override lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val vec = Flipped(new VectorMemIO)
      val vec_busy = Input(Bool())
      val mem_busy = Output(Bool())
    })

    val (tl_out, edge) = node.out(0)
    io.vec.scalar_check := DontCare

    val inflights = RegInit(0.U((1+dmemTagBits).W))
    when (tl_out.a.fire || tl_out.d.fire) {
      inflights := inflights + tl_out.a.fire - tl_out.d.fire
    }
    io.mem_busy := inflights =/= 0.U

    io.vec.load_req.ready := tl_out.a.ready
    io.vec.store_req.ready := false.B

    when (io.vec.load_req.valid) {
      tl_out.a.valid := true.B
      tl_out.a.bits := edge.Get(
        Cat(0.U, io.vec.load_req.bits.tag),
        io.vec.load_req.bits.addr,
        log2Ceil(dLenB).U)._2
      assert(io.vec.load_req.bits.phys)
    } .otherwise {
      assert(!io.vec.store_req.valid || io.vec.store_req.bits.phys)
      tl_out.a.valid := io.vec.store_req.valid
      tl_out.a.bits := edge.Put(
        Cat(1.U, io.vec.store_req.bits.tag),
        io.vec.store_req.bits.addr,
        log2Ceil(dLenB).U,
        io.vec.store_req.bits.data,
        io.vec.store_req.bits.mask)._2
      io.vec.store_req.ready := tl_out.a.ready
    }

    tl_out.d.ready := true.B
    val d_store = tl_out.d.bits.opcode === TLMessages.AccessAck
    io.vec.store_ack.valid := tl_out.d.valid && d_store
    io.vec.store_ack.bits := tl_out.d.bits.source
    io.vec.load_resp.valid := tl_out.d.valid && !d_store
    io.vec.load_resp.bits.data := tl_out.d.bits.data
    io.vec.load_resp.bits.tag := tl_out.d.bits.source
  }
}
