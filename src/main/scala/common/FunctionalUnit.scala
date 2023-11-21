package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

abstract class VectorFunctionalUnit(depth: Int)(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val pipe = Input(Vec(depth, Valid(new VectorMicroOp(depth))))

    val writes = Vec(2, Valid(new VectorWrite))
  })
  
  io.writes(0).valid     := io.pipe(0).valid && (io.pipe(0).bits.wvd_eg(0) === 0.U || io.pipe(0).bits.wvd_widen2)
  io.writes(0).bits.eg   := io.pipe(0).bits.wvd_eg >> 1
  io.writes(0).bits.mask := io.pipe(0).bits.wmask

  io.writes(1).valid     := io.pipe(0).valid && (io.pipe(0).bits.wvd_eg(0) === 1.U || io.pipe(0).bits.wvd_widen2)
  io.writes(1).bits.eg   := io.pipe(0).bits.wvd_eg >> 1
  io.writes(1).bits.mask := io.pipe(0).bits.wmask

  val wide_mask = FillInterleaved(2, io.pipe(0).bits.wmask)
  when (io.pipe(0).bits.wvd_widen2) {
    io.writes(0).valid := io.pipe(0).valid
    io.writes(1).valid := io.pipe(0).valid
    io.writes(0).bits.mask := wide_mask(dLenB-1,0)
    io.writes(1).bits.mask := wide_mask >> dLenB
  }
}

class VectorIntegerMultiply(implicit p: Parameters) extends VectorFunctionalUnit(1)(p) {
  
  val  ctrl_wide_in :: ctrl_rv1_signed :: ctrl_rv2_signed :: ctrl_madd :: ctrl_acc :: ctrl_sub :: ctrl_hi :: Nil = VecDecode.applyBools(
    io.pipe(0).bits.funct3, io.pipe(0).bits.funct6,
    Seq.fill(7)(BitPat.dontCare(1)), Seq(  
      (OPMFunct6.mulhu,   Seq(N,N,N,N,N,X,Y)),
      (OPMFunct6.mul,     Seq(N,Y,Y,N,N,X,N)),
      (OPMFunct6.mulhsu,  Seq(N,N,Y,N,N,X,Y)),
      (OPMFunct6.mulh,    Seq(N,Y,Y,N,N,X,Y)),
      (OPMFunct6.madd,    Seq(N,Y,Y,Y,Y,N,N)),
      (OPMFunct6.nmsub,   Seq(N,Y,Y,Y,Y,Y,N)),
      (OPMFunct6.macc,    Seq(N,Y,Y,N,Y,N,N)),
      (OPMFunct6.nmsac,   Seq(N,Y,Y,N,Y,Y,N)),

      (OPMFunct6.wmulu,   Seq(Y,N,N,N,N,X,N)),
      (OPMFunct6.wmulsu,  Seq(Y,N,Y,N,N,X,N)),
      (OPMFunct6.wmul,    Seq(Y,Y,Y,N,N,X,N)),
      (OPMFunct6.wmaccu,  Seq(Y,N,N,N,Y,N,N)),
      (OPMFunct6.wmacc,   Seq(Y,Y,Y,N,Y,N,N)),
      (OPMFunct6.wmaccus, Seq(Y,N,Y,N,Y,N,N)),
      (OPMFunct6.wmaccsu, Seq(Y,Y,N,N,Y,N,N))
    ))

  val eew = io.pipe(0).bits.rvs1_eew
  val in1 = io.pipe(0).bits.rvs1_data
  val in2 =  Mux(ctrl_madd, io.pipe(0).bits.rvd_data, io.pipe(0).bits.rvs2_data)

  val numSegMul = dLen/xLen
  val viMul = Seq.fill(numSegMul) {Module(new SegmentedIntegerMultiplier(1))}
  for (i <- 0 until numSegMul) {
    
    viMul(i).io.rv1_signed := ctrl_rv1_signed
    viMul(i).io.rv2_signed := ctrl_rv2_signed
    viMul(i).io.ctrl_acc := ctrl_acc
    viMul(i).io.ctrl_madd := ctrl_madd
    viMul(i).io.ctrl_sub := ctrl_sub
    viMul(i).io.eew := eew
    viMul(i).io.in1 := in1((i+1)*xLen-1, i*xLen)
    viMul(i).io.in2 := in2((i+1)*xLen-1, i*xLen)
  }

  val viMulOut = VecInit((0 until numSegMul).map { i => viMul(i).io.out }).asUInt
  io.writes(0).bits.data := viMulOut
  io.writes(1).bits.data := viMulOut

  when (io.pipe(0).bits.wvd_widen2) {
    io.writes(0).bits.data := viMulOut
    io.writes(1).bits.data := viMulOut >> dLen
  }
}

class VectorIntegerUnit(implicit p: Parameters) extends VectorFunctionalUnit(1)(p) {

  val ctrl_sub :: ctrl_add_sext :: ctrl_wide_in :: ctrl_cmask :: Nil = VecDecode.applyBools(
    io.pipe(0).bits.funct3, io.pipe(0).bits.funct6,
    Seq.fill(4)(BitPat.dontCare(1)), Seq(
      (OPIFunct6.add   , Seq(N,X,N,N)),
      (OPIFunct6.sub   , Seq(Y,X,N,N)),
      (OPIFunct6.rsub  , Seq(Y,X,N,N)),
      (OPMFunct6.waddu , Seq(N,N,N,N)),
      (OPMFunct6.wadd  , Seq(N,Y,N,N)),
      (OPMFunct6.wsubu , Seq(Y,N,N,N)),
      (OPMFunct6.wsub  , Seq(Y,Y,N,N)),
      (OPMFunct6.wadduw, Seq(N,N,Y,N)),
      (OPMFunct6.waddw , Seq(N,Y,Y,N)),
      (OPMFunct6.wsubuw, Seq(Y,N,Y,N)),
      (OPMFunct6.wsubw , Seq(Y,Y,Y,N)),
      (OPIFunct6.adc   , Seq(N,X,N,Y))
    ))

  val rsub = io.pipe(0).bits.isOpi && OPIFunct6(io.pipe(0).bits.funct6) === OPIFunct6.rsub
  val xunary0 = io.pipe(0).bits.isOpm && OPMFunct6(io.pipe(0).bits.funct6) === OPMFunct6.xunary0

  val add_in1 = io.pipe(0).bits.rvs1_data.asTypeOf(Vec(dLenB, UInt(8.W)))
  val add_in2 = io.pipe(0).bits.rvs2_data.asTypeOf(Vec(dLenB, UInt(8.W)))
  val add_use_carry = Mux1H(UIntToOH(io.pipe(0).bits.rvs2_eew),
    (0 until 4).map { eew => Fill(dLenB >> eew, ~(1.U((1 << eew).W))) }
  )
  val add_mask_carry = Mux1H(UIntToOH(io.pipe(0).bits.rvs2_eew), (0 until 4).map { eew =>
    VecInit((0 until dLenB >> eew).map { i => io.pipe(0).bits.rmask(i) | 0.U((1 << eew).W) }).asUInt
  })
  val add_carry = Wire(Vec(dLenB+1, UInt(1.W)))
  val add_out = Wire(Vec(dLenB, UInt(8.W)))
  val add_wide_out_eew = (0 until 3).map { eew => Wire(Vec(dLenB >> eew, UInt((16 << eew).W))) }
  val add_wide_out = Mux1H(UIntToOH(io.pipe(0).bits.rvs2_eew), add_wide_out_eew.map(_.asUInt))
  val add_wide_in1_eew = (0 until 3).map { eew => Wire(Vec(dLenB >> (eew + 1), UInt((16 << eew).W))) }
  val add_wide_in1 = Mux1H(UIntToOH(io.pipe(0).bits.rvs1_eew), add_wide_in1_eew.map(_.asTypeOf(Vec(dLenB, UInt(8.W)))))

  add_carry(0) := ctrl_sub

  for (i <- 0 until dLenB) {
    val in1 = Mux(ctrl_wide_in, add_wide_in1(i), Mux(rsub, add_in2(i), add_in1(i)))
    val in2 = Mux(rsub, add_in1(i), add_in2(i))
    val full = (Mux(ctrl_sub, ~in1, in1) +&
      in2 +&
      Mux(add_use_carry(i), add_carry(i), ctrl_sub) +&
      Mux(ctrl_cmask, add_mask_carry(i), 0.U)
    )
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

  val xunary0_eew_mul = io.pipe(0).bits.vd_eew - io.pipe(0).bits.rvs2_eew
  val xunary0_in = (1 until 4).map { m =>
    val w = dLen >> m
    val in = Wire(UInt(w.W))
    val in_mul = io.pipe(0).bits.rvs2_data.asTypeOf(Vec(1 << m, UInt(w.W)))
    in := in_mul(io.pipe(0).bits.wvd_eg(m-1,0))
    in
  }
  val xunary0_out = Mux1H((1 until 4).map { vd_eew => (0 until vd_eew).map { vs2_eew =>
    (io.pipe(0).bits.vd_eew === vd_eew.U && io.pipe(0).bits.rvs2_eew === vs2_eew.U) -> {
      val mul = vd_eew - vs2_eew
      val in = xunary0_in(mul-1).asTypeOf(Vec(dLenB >> vd_eew, UInt((8 << vs2_eew).W)))
      val out = Wire(Vec(dLenB >> vd_eew, UInt((8 << vd_eew).W)))
      out.zip(in).foreach { case (l, r) => l := Cat(
        Fill((8 << vd_eew) - (8 << vs2_eew), io.pipe(0).bits.rs1(0) && r((8 << vs2_eew)-1)),
        r)
      }
      out.asUInt
    }
  }}.flatten)

  val out = Mux(xunary0, xunary0_out, add_out.asUInt)

  io.writes(0).bits.data := out
  io.writes(1).bits.data := out

  when (io.pipe(0).bits.wvd_widen2) {
    io.writes(0).bits.data := add_wide_out
    io.writes(1).bits.data := add_wide_out >> dLen
  }
}
