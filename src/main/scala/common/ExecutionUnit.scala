package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

class VectorExecutionUnit(depth: Int)(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val iss = Flipped(Decoupled(new VectorIssueBeat(depth)))

    val write = Valid(new VectorWrite)
  })

  val wmask = RegInit(0.U(depth.W))
  val pipe_valids = Seq.fill(depth) { RegInit(false.B) }
  val pipe_bits = Seq.fill(depth) { Reg(new VectorIssueBeat(depth)) }

  wmask := wmask >> 1 | (io.iss.fire << io.iss.bits.wlat)
  io.write.valid := wmask(0)
  io.write.bits.eg := pipe_bits.last.wvd_eg
  io.write.bits.mask := pipe_bits.last.wmask
  io.write.bits.data := DontCare

  io.iss.ready := ((1.U << io.iss.bits.wlat) & wmask) === 0.U

  pipe_valids.head := io.iss.fire
  when (io.iss.fire) { pipe_bits.head := io.iss.bits }
  for (i <- 1 until depth) {
    pipe_valids(i) := pipe_valids(i-1)
    when (pipe_valids(i-1)) {
      pipe_bits(i) := pipe_bits(i-1)
    }
  }
}
