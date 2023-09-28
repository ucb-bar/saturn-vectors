package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

abstract class VectorFunctionalUnit(depth: Int)(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val iss = Input(Valid(new VectorIssueBeat(depth)))
    val iss_vs1_data = UInt(dLen.W)
    val iss_vs2_data = UInt(dLen.W)
    val iss_vd_data = UInt(dLen.W)

    val pipe = Input(Vec(depth, new VectorIssueBeat(depth)))

    val write = Valid(new VectorWrite)
  })
}

class VectorIntegerUnit(implicit p: Parameters) extends VectorFunctionalUnit(1)(p) {

}
