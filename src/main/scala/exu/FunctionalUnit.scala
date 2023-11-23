package vector.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import vector.common._

abstract class FunctionalUnit(depth: Int)(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val iss = Input(Valid(new VectorMicroOp(depth)))

    val pipe = Input(Vec(depth, Valid(new VectorMicroOp(depth))))

    val writes = Vec(2, Valid(new VectorWrite))
  })
}
