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

    val writes = Vec(2, Valid(new VectorWrite))
  })
}

class VectorIntegerUnit(implicit p: Parameters) extends VectorFunctionalUnit(1)(p) {
  val is_sub = VecDecode(io.pipe(0).bits.inst,
    Seq(OPIFunct6.sub, OPIFunct6.rsub, OPMFunct6.wsubu, OPMFunct6.wsub),
    Seq(OPIFunct6.add, OPMFunct6.waddu, OPMFunct6.wadd))
  val is_rsub = VecDecode(io.pipe(0).bits.inst,
    Seq(OPIFunct6.rsub),
    Seq(OPIFunct6.sub, OPMFunct6.wsubu, OPMFunct6.wsub))
  val is_wsext = VecDecode(io.pipe(0).bits.inst,
    Seq(OPMFunct6.wsub, OPMFunct6.wadd),
    Seq(OPMFunct6.wsubu, OPMFunct6.waddu))
  // val add_ext_carry = VecDecode(io.pipe(0).bits.inst,
  //   Seq(OPMFunct6.wsubu, OPMFunct6.wsub, OPMFunct6.wadd),
  //   Seq(OPMFunct6.waddu))

  val add_in1 = io.pipe(0).bits.rvs1_data.asTypeOf(Vec(dLenB, UInt(8.W)))
  val add_in2 = io.pipe(0).bits.rvs2_data.asTypeOf(Vec(dLenB, UInt(8.W)))
  val add_use_carry = Mux1H(UIntToOH(io.pipe(0).bits.rvs1_eew),
    (0 until 4).map { eew => Fill(dLenB >> eew, ~(1.U((1 << eew).W))) }
  )
  val add_carry = Wire(Vec(dLenB+1, UInt(1.W)))
  val add_out = Wire(Vec(dLenB, UInt(8.W)))
  val add_wideout_eew = (0 until 3).map { eew => Wire(Vec(dLenB >> eew, UInt((16 << eew).W))) }
  val add_wideout = Mux1H(UIntToOH(io.pipe(0).bits.rvs1_eew), add_wideout_eew.map(_.asUInt))

  add_carry(0) := is_sub

  for (i <- 0 until dLenB) {
    val in1 = Mux(is_rsub, add_in2(i), add_in1(i))
    val in2 = Mux(is_rsub, add_in1(i), add_in2(i))
    val full = Mux(is_sub, ~in1, in1) +& in2 +& Mux(add_use_carry(i), add_carry(i), is_sub)
    add_out(i) := full(7,0)
    add_carry(i+1) := full(8)
  }
  for (eew <- 0 until 3) {
    val out_vec = add_out.asTypeOf(Vec(dLenB >> eew, UInt((8 << eew).W)))
    for (i <- 0 until dLenB >> eew) {
      val carry = add_carry((i+1) << eew)

      val hi1 = (is_wsext && add_in1(((i + 1) << eew) - 1)(7)) ^ is_sub
      val hi2 = is_wsext && add_in2(((i + 1) << eew) - 1)(7)
      val hi = Mux(hi1 && hi2, ~(1.U((8 << eew).W)), Fill(8 << eew, hi1 ^ hi2))
      add_wideout_eew(eew)(i) := Cat(hi + carry, out_vec(i))
    }
    dontTouch(add_wideout_eew(eew))
  }

  io.writes(0).valid     := io.pipe(0).valid && (io.pipe(0).bits.wvd_eg(0) === 0.U || io.pipe(0).bits.wvd_widen2)
  io.writes(0).bits.eg   := io.pipe(0).bits.wvd_eg >> 1
  io.writes(0).bits.mask := io.pipe(0).bits.wmask
  io.writes(0).bits.data := add_out.asUInt

  io.writes(1).valid     := io.pipe(0).valid && (io.pipe(0).bits.wvd_eg(0) === 1.U || io.pipe(0).bits.wvd_widen2)
  io.writes(1).bits.eg   := io.pipe(0).bits.wvd_eg >> 1
  io.writes(1).bits.mask := io.pipe(0).bits.wmask
  io.writes(1).bits.data := add_out.asUInt

  when (io.pipe(0).bits.wvd_widen2) {
    val wide_mask = FillInterleaved(2, io.pipe(0).bits.wmask)
    io.writes(0).bits.mask := wide_mask(dLenB-1,0)
    io.writes(1).bits.mask := wide_mask >> dLenB
    io.writes(0).bits.data := add_wideout
    io.writes(1).bits.data := add_wideout >> dLen
  }
}
