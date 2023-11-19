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

  val viu = Module(new VectorIntegerUnit)
  viu.io.pipe(0).valid := pipe_valids(0) && io.iss.bits.isOpi
  viu.io.pipe(0).bits := pipe_bits(0)

  val viMul = Module(new VectorIntegerMultiply)
  viMul.io.pipe(0).valid := pipe_valids(0) && io.iss.bits.isOpm
  viMul.io.pipe(0).bits := pipe_bits(0)

  io.writes(0) := MuxCase(DontCare, Array(
                viu.io.writes(0).valid -> viu.io.writes(0),
                viMul.io.writes(0).valid -> viMul.io.writes(0)
  ))
  io.writes(1) := MuxCase(DontCare, Array(
                viu.io.writes(1).valid -> viu.io.writes(1),
                viMul.io.writes(1).valid -> viMul.io.writes(1)
  ))
  // io.writes := Mux(viMul.io.writes(0).valid, viMul.io.writes, viu.io.writes)
}
