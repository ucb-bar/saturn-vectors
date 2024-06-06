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

  val accessors = Seq.tabulate(ports) { i => LazyModule(new TLInterface(tagBits)) }
  val identityNode = TLEphemeralNode()
  accessors.foreach { a => identityNode := TLBuffer() := a.node }

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
      accessors(i).module.io.req <> io.vec.req(i)
      accessors(i).module.io.resp <> io.vec.resp(i)
    }
    io.mem_busy := accessors.map(_.module.io.busy).orR
  }
}
