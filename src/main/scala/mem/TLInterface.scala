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

class TLLoadInterface(implicit p: Parameters) extends LazyModule()(p) with HasCoreParameters with HasVectorParams {
  val node = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(
    name      = s"Core ${tileId} Vector Load",
    sourceId  = IdRange(0, 1 << dmemTagBits)
  )))))
  override lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val busy = Output(Bool())
      val req = Flipped(Decoupled(new MemRequest))
      val resp = Valid(new LoadResponse)
    })

    val (out, edge) = node.out(0)
    val inflights = RegInit(0.U(dmemTagBits.W))
    when (out.a.fire || out.d.fire) {
      inflights := inflights + out.a.fire - out.d.fire
    }
    io.busy := inflights =/= 0.U

    io.req.ready := out.a.ready
    out.a.valid := io.req.valid
    out.a.bits := edge.Get(
      io.req.bits.tag,
      (io.req.bits.addr >> dLenOffBits) << dLenOffBits,
      log2Ceil(dLenB).U)._2

    out.d.ready := true.B
    io.resp.valid := out.d.valid
    io.resp.bits.data := out.d.bits.data
    io.resp.bits.tag := out.d.bits.source
  }
}

class TLStoreInterface(implicit p: Parameters) extends LazyModule()(p) with HasCoreParameters with HasVectorParams {
  val node = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(
    name      = s"Core ${tileId} Vector Store",
    sourceId  = IdRange(0, 1 << dmemTagBits)
  )))))
  override lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val busy = Output(Bool())
      val req = Flipped(Decoupled(new MemRequest))
      val ack = Valid(UInt(dmemTagBits.W))
    })

    val (out, edge) = node.out(0)
    val inflights = RegInit(0.U(dmemTagBits.W))
    when (out.a.fire || out.d.fire) {
      inflights := inflights + out.a.fire - out.d.fire
    }
    io.busy := inflights =/= 0.U

    io.req.ready := out.a.ready
    out.a.valid := io.req.valid
    out.a.bits := edge.Put(
      io.req.bits.tag,
      (io.req.bits.addr >> dLenOffBits) << dLenOffBits,
      log2Ceil(dLenB).U,
      io.req.bits.data,
      io.req.bits.mask)._2

    out.d.ready := true.B
    io.ack.valid := out.d.valid
    io.ack.bits := out.d.bits.source
  }
}

class TLInterface(implicit p: Parameters) extends LazyModule()(p) with HasCoreParameters with HasVectorParams {

  val reader = LazyModule(new TLLoadInterface)
  val writer = LazyModule(new TLStoreInterface)

  val arb = LazyModule(new TLXbar)
  def node = arb.node

  arb.node := reader.node
  arb.node := writer.node

  override lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val vec = Flipped(new VectorMemIO)
      val mem_busy = Output(Bool())
    })

    reader.module.io.req <> io.vec.load_req
    io.vec.load_resp <> reader.module.io.resp
    writer.module.io.req <> io.vec.store_req
    io.vec.store_ack <> writer.module.io.ack
    io.mem_busy := reader.module.io.busy || writer.module.io.busy
  }
}
