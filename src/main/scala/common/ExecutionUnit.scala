package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

class VectorExecutionUnit(depth: Int)(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val iss = Flipped(Decoupled(new VectorMicroOp(depth)))

    val writes = Vec(2, Valid(new VectorWrite))
    val vat_release = Valid(UInt(vParams.vatSz.W))
  })

  val wmask = RegInit(0.U(depth.W))
  val pipe_valids = Seq.fill(depth) { RegInit(false.B) }
  val pipe_bits = Seq.fill(depth) { Reg(new VectorMicroOp(depth)) }

  wmask := wmask >> 1 | ((io.iss.fire << io.iss.bits.wlat) >> 1)
  io.iss.ready := (1.U << (io.iss.bits.wlat) & wmask) === 0.U

  pipe_valids.head := io.iss.fire
  when (io.iss.fire) { pipe_bits.head := io.iss.bits }
  for (i <- 1 until depth) {
    pipe_valids(i) := pipe_valids(i-1)
    when (pipe_valids(i-1)) {
      pipe_bits(i) := pipe_bits(i-1)
    }
  }
  val vat_release_oh = pipe_valids.zip(pipe_bits).zipWithIndex.map { case ((v, b), i) =>
    (v && b.wlat === i.U && b.last)
  }
  io.vat_release.valid := vat_release_oh.orR
  io.vat_release.bits := Mux1H(vat_release_oh, pipe_bits.map(_.vat))

  val viu = Module(new VectorIntegerUnit)
  viu.io.iss.valid := io.iss.fire && io.iss.bits.funct3.isOneOf(OPIVI, OPIVX, OPIVV)
  viu.io.iss.bits := io.iss.bits
  viu.io.pipe(0).valid := pipe_valids(0)
  viu.io.pipe(0).bits := pipe_bits(0)

  io.writes := viu.io.writes
}
