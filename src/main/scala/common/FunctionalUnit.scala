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

  val ctrl_sub :: ctrl_rsub :: ctrl_add_sext :: ctrl_wide_in :: Nil = VecDecode.applyBools(io.pipe(0).bits.inst,
    Seq.fill(4)(BitPat.dontCare(1)), Seq(
      (OPIFunct6.add   , Seq(N,X,X,N)),
      (OPIFunct6.sub   , Seq(Y,N,X,N)),
      (OPIFunct6.rsub  , Seq(Y,Y,X,N)),
      (OPMFunct6.waddu , Seq(N,X,N,N)),
      (OPMFunct6.wadd  , Seq(N,X,Y,N)),
      (OPMFunct6.wsubu , Seq(Y,N,N,N)),
      (OPMFunct6.wsub  , Seq(Y,N,Y,N)),
      (OPMFunct6.wadduw, Seq(N,X,N,Y)),
      (OPMFunct6.waddw , Seq(N,X,Y,Y)),
      (OPMFunct6.wsubuw, Seq(Y,N,N,Y)),
      (OPMFunct6.wsubw , Seq(Y,N,Y,Y))
    ))

  val add_in1 = io.pipe(0).bits.rvs1_data.asTypeOf(Vec(dLenB, UInt(8.W)))
  val add_in2 = io.pipe(0).bits.rvs2_data.asTypeOf(Vec(dLenB, UInt(8.W)))
  val add_use_carry = Mux1H(UIntToOH(io.pipe(0).bits.rvs2_eew),
    (0 until 4).map { eew => Fill(dLenB >> eew, ~(1.U((1 << eew).W))) }
  )
  val add_carry = Wire(Vec(dLenB+1, UInt(1.W)))
  val add_out = Wire(Vec(dLenB, UInt(8.W)))
  val add_wide_out_eew = (0 until 3).map { eew => Wire(Vec(dLenB >> eew, UInt((16 << eew).W))) }
  val add_wide_out = Mux1H(UIntToOH(io.pipe(0).bits.rvs2_eew), add_wide_out_eew.map(_.asUInt))
  val add_wide_in1_eew = (0 until 3).map { eew => Wire(Vec(dLenB >> (eew + 1), UInt((16 << eew).W))) }
  val add_wide_in1 = Mux1H(UIntToOH(io.pipe(0).bits.rvs1_eew), add_wide_in1_eew.map(_.asTypeOf(Vec(dLenB, UInt(8.W)))))

  add_carry(0) := ctrl_sub

  for (i <- 0 until dLenB) {
    val in1 = Mux(ctrl_wide_in, add_wide_in1(i), Mux(ctrl_rsub, add_in2(i), add_in1(i)))
    val in2 = Mux(ctrl_rsub, add_in1(i), add_in2(i))
    val full = Mux(ctrl_sub, ~in1, in1) +& in2 +& Mux(add_use_carry(i), add_carry(i), ctrl_sub)
    add_out(i) := full(7,0)
    add_carry(i+1) := full(8)
  }
  for (eew <- 0 until 3) {
    val in_vec = add_in1.asTypeOf(Vec(dLenB >> eew, UInt((8 << eew).W)))
    for (i <- 0 until dLenB >> (eew + 1)) {
      val lo = Mux(io.pipe(0).bits.wvd_eg(0), in_vec(i + (dLenB >> (eew + 1))), in_vec(i))
      val hi = Fill(16 << eew, lo((8 << eew)-1) && ctrl_add_sext)
      add_wide_in1_eew(eew)(i) := Cat(hi, lo)
    }

    val out_vec = add_out.asTypeOf(Vec(dLenB >> eew, UInt((8 << eew).W)))
    for (i <- 0 until dLenB >> eew) {
      val carry = add_carry((i+1) << eew)

      val hi1 = (ctrl_add_sext && add_in1(((i + 1) << eew) - 1)(7)) ^ ctrl_sub
      val hi2 = ctrl_add_sext && add_in2(((i + 1) << eew) - 1)(7)
      val hi = Mux(hi1 && hi2, ~(1.U((8 << eew).W)), Fill(8 << eew, hi1 ^ hi2))
      add_wide_out_eew(eew)(i) := Cat(hi + carry, out_vec(i))
    }
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
    io.writes(0).bits.data := add_wide_out
    io.writes(1).bits.data := add_wide_out >> dLen
  }
}
