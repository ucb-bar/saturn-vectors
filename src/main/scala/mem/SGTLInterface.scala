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

class SGTLInterface(implicit p: Parameters) extends LazyModule()(p) with HasCoreParameters with HasVectorParams {

  require(vParams.vsgBuffers >= 2)
  val accessors = Seq.tabulate(vParams.vsgPorts) { i => LazyModule(new TLInterface(sgmemTagBits)) }
  val identityNode = TLEphemeralNode()
  accessors.foreach { a => identityNode := TLBuffer(BufferParams(vParams.vsgBuffers), BufferParams.none) := a.node }

  def node = TLWidthWidget(1) :=* identityNode

  override lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val vec = Flipped(new VectorSGMemIO)
      val mem_busy = Output(Bool())
    })

    io.vec.base := DontCare // this should be set outside
    io.mem_busy := false.B
    for (i <- 0 until vParams.vsgPorts) {
      accessors(i).module.io.req <> io.vec.req(i)
      accessors(i).module.io.resp <> io.vec.resp(i)
    }
    io.mem_busy := accessors.map(_.module.io.busy).orR
  }
}
