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

class TLInterface(tagBits: Int)(implicit p: Parameters) extends LazyModule()(p) with HasCoreParameters {
  val node = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(
    name      = s"Core ${tileId} Vector Load",
    sourceId  = IdRange(0, 1 << tagBits)
  )))))
  override lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {

    val (out, edge) = node.out(0)

    val widthBytes = edge.slave.beatBytes
    val offBits = log2Ceil(widthBytes)

    val io = IO(new Bundle {
      val busy = Output(Bool())
      val req = Flipped(Decoupled(new MemRequest(widthBytes, tagBits)))
      val resp = Valid(new MemResponse(widthBytes, tagBits))
    })

    val inflights = RegInit(0.U(tagBits.W))
    when (out.a.fire || out.d.fire) {
      inflights := inflights + out.a.fire - out.d.fire
    }
    io.busy := inflights =/= 0.U

    io.req.ready := out.a.ready
    out.a.valid := io.req.valid
    out.a.bits := Mux(io.req.bits.store,
      edge.Put(
        io.req.bits.tag,
        (io.req.bits.addr >> offBits) << offBits,
        log2Ceil(widthBytes).U,
        io.req.bits.data,
        io.req.bits.mask)._2,
      edge.Get(
        io.req.bits.tag,
        (io.req.bits.addr >> offBits) << offBits,
        log2Ceil(widthBytes).U)._2
    )

    out.d.ready := true.B
    io.resp.valid := out.d.valid
    io.resp.bits.data := out.d.bits.data
    io.resp.bits.tag := out.d.bits.source
  }
}


class TLSplitInterface(implicit p: Parameters) extends LazyModule()(p) with HasCoreParameters with HasVectorParams {

  val reader = LazyModule(new TLInterface(dmemTagBits))
  val writer = LazyModule(new TLInterface(dmemTagBits))

  val arb = LazyModule(new TLXbar)
  def node = TLWidthWidget(dLenB) := arb.node
  def edge = arb.node.edges.out(0)

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
    io.vec.store_ack <> writer.module.io.resp
    io.mem_busy := reader.module.io.busy || writer.module.io.busy
  }
}
