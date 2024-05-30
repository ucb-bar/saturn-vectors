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

class SGTLInterface(ports: Int, tagBits: Int)(implicit p: Parameters) extends LazyModule()(p) with HasCoreParameters with HasVectorParams {

  val readers = Seq.tabulate(ports) { i => LazyModule(new TLLoadInterface(tagBits)) }
  val writers = Seq.tabulate(ports) { i => LazyModule(new TLStoreInterface(tagBits)) }

  val identityNode = TLEphemeralNode()
  for (i <- 0 until ports) {
    val xbar = LazyModule(new TLXbar)
    xbar.node := readers(i).node
    xbar.node := writers(i).node
    identityNode := xbar.node
  }

  def node = TLWidthWidget(1) :=* identityNode

  override lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val vec = Flipped(new VectorSGMemIO(ports))
      val mem_busy = Output(Bool())
    })

    io.vec.base := DontCare // this should be set outside
    io.mem_busy := false.B
    for (i <- 0 until ports) {
      readers(i).module.io.req <> io.vec.load_req(i)
      io.vec.load_resp(i) <> readers(i).module.io.resp
      writers(i).module.io.req <> io.vec.store_req(i)
      io.vec.store_ack(i) <> writers(i).module.io.ack
      io.mem_busy := (readers.map(_.module.io.busy) ++ writers.map(_.module.io.busy)).orR
    }
  }
}
