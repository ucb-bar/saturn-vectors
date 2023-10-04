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

    val pipe = Input(Vec(depth, Valid(new VectorIssueBeat(depth))))

    val write = Valid(new VectorWrite)
  })
}

class VectorIntegerUnit(implicit p: Parameters) extends VectorFunctionalUnit(1)(p) {
  val is_sub = DecodeLogic(io.pipe(0).bits.inst.funct6,
    Seq(OPIFunct6.sub, OPIFunct6.rsub).map(_.litValue.U),
    Seq(OPIFunct6.add).map(_.litValue.U))
  val is_rsub = DecodeLogic(io.pipe(0).bits.inst.funct6,
    Seq(OPIFunct6.rsub).map(_.litValue.U),
    Seq(OPIFunct6.sub).map(_.litValue.U))

  val add_in1 = io.pipe(0).bits.rvs1_data.asTypeOf(Vec(dLenB, UInt(8.W)))
  val add_in2 = io.pipe(0).bits.rvs2_data.asTypeOf(Vec(dLenB, UInt(8.W)))
  val add_use_carry = Mux1H(UIntToOH(io.pipe(0).bits.rvs1_eew),
    (0 until 4).map { eew => Fill(dLenB >> eew, ~(1.U((1 << eew).W))) }
  )
  val add_carry = Wire(Vec(dLenB+1, UInt(1.W)))
  val add_out = Wire(Vec(dLenB, UInt(8.W)))

  add_carry(0) := is_sub

  for (i <- 0 until dLenB) {
    val in1 = Mux(is_rsub, add_in2(i), add_in1(i))
    val in2 = Mux(is_rsub, add_in1(i), add_in2(i))
    val full =  Mux(is_sub, ~in1, in1) +& in2 +& Mux(add_use_carry(i), add_carry(i), is_sub)
    add_out(i) := full(7,0)
    add_carry(i+1) := full(8)
  }

  io.write.valid := io.pipe(0).valid
  io.write.bits.eg := io.pipe(0).bits.wvd_eg
  io.write.bits.mask := io.pipe(0).bits.wmask
  io.write.bits.data := add_out.asUInt
}
