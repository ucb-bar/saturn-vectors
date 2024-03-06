package saturn.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import saturn.common._
import saturn.insns._

class SegmentedMultiplyPipe(implicit p: Parameters) extends PipelinedFunctionalUnit(3)(p) {
  val supported_insns = Seq(
    MUL.VV, MUL.VX, MULH.VV, MULH.VX,
    MULHU.VV, MULHU.VX, MULHSU.VV, MULHSU.VX,
    WMUL.VV, WMUL.VX, WMULU.VV, WMULU.VX,
    WMULSU.VV, WMULSU.VX,
    MACC.VV, MACC.VX, NMSAC.VV, NMSAC.VX,
    MADD.VV, MADD.VX, NMSUB.VV, NMSUB.VX,
    WMACC.VV, WMACC.VX, WMACCU.VV, WMACCU.VX,
    WMACCSU.VV , WMACCSU.VX, WMACCUS.VV, WMACCUS.VX,
    SMUL.VV.elementWise, SMUL.VX.elementWise)
  io.iss.ready := new VectorDecoder(io.iss.op.funct3, io.iss.op.funct6, 0.U, 0.U, supported_insns, Nil).matched
  // TODO: SMUL currently operates at 1 element/cycle
  io.set_vxsat := false.B
  io.set_fflags.valid := false.B
  io.set_fflags.bits := DontCare

   val ctrl = new VectorDecoder(io.pipe(0).bits.funct3, io.pipe(0).bits.funct6, 0.U, 0.U, supported_insns, Seq(
     MULSign1, MULSign2, MULSwapVdV2))

  val in_eew = io.pipe(0).bits.rvs1_eew
  val out_eew = io.pipe(0).bits.vd_eew

  val in_vs1 = io.pipe(0).bits.rvs1_data
  val in_vs2 = io.pipe(0).bits.rvs2_data
  val in_vd  = io.pipe(0).bits.rvd_data

  val mul_in1 = in_vs1
  val mul_in2 = Mux(ctrl.bool(MULSwapVdV2), in_vd, in_vs2)

  val multipliers = Seq.fill(dLenB >> 3)(Module(new MultiplyBlock))
  for (i <- 0 until (dLenB >> 3)) {
    multipliers(i).io.in1_signed := ctrl.bool(MULSign1)
    multipliers(i).io.in2_signed := ctrl.bool(MULSign2)
    // multipliers(i).io.valid      := io.pipe(0).valid
    multipliers(i).io.eew        := io.pipe(0).bits.rvs1_eew
    multipliers(i).io.in1        := mul_in1.asTypeOf(Vec(dLenB >> 3, UInt(64.W)))(i)
    multipliers(i).io.in2        := mul_in2.asTypeOf(Vec(dLenB >> 3, UInt(64.W)))(i)
  }
  val mul_out_comb = VecInit(multipliers.map(_.io.out_data)).asUInt
  val mul_out = Pipe(io.pipe(0).valid, mul_out_comb, 2).bits
  //////////////////////////////////////////////
  // 2 Pipeline Stages in multipliers
  //////////////////////////////////////////////
  val in_eew_pipe = io.pipe(2).bits.rvs1_eew
  val out_eew_pipe = io.pipe(2).bits.vd_eew
  val ctrl_wmul = out_eew_pipe > in_eew_pipe
  val ctrl_smul = io.pipe(2).bits.isOpi
  val ctrl_pipe = new VectorDecoder(io.pipe(2).bits.funct3, io.pipe(2).bits.funct6, 0.U, 0.U, supported_insns, Seq(
    MULHi, MULSwapVdV2, MULAccumulate, MULSub))
  val in_vs2_pipe = io.pipe(2).bits.rvs2_data
  val in_vd_pipe  = io.pipe(2).bits.rvd_data

  val hi = VecInit.tabulate(4)({sew =>
    VecInit(mul_out.asTypeOf(Vec((2*dLenB) >> sew, UInt((8 << sew).W))).grouped(2).map(_.last).toSeq).asUInt
  })(in_eew_pipe)
  val lo = VecInit.tabulate(4)({sew =>
    VecInit(mul_out.asTypeOf(Vec((2*dLenB) >> sew, UInt((8 << sew).W))).grouped(2).map(_.head).toSeq).asUInt
  })(in_eew_pipe)
  val half_sel = (io.pipe(2).bits.eidx >> (dLenOffBits.U - out_eew_pipe))(0)
  val wide = Mux(half_sel, mul_out >> dLen, mul_out)(dLen-1,0)

  // TODO, handle SMUL > 1 elem/cycle
  val smul_prod = VecInit.tabulate(4)({sew =>
    if (sew == 3 && dLenB == 8) { mul_out.asSInt } else {
      mul_out.asTypeOf(Vec(dLenB >> sew, SInt((16 << sew).W)))(io.pipe(2).bits.eidx(log2Ceil(dLenB)-1-sew,0))
    }
  })(out_eew_pipe).asSInt

  val rounding_incr = VecInit.tabulate(4)({ sew => RoundingIncrement(io.pipe(2).bits.vxrm, smul_prod((8 << sew)-1,0)) })(out_eew_pipe)
  val smul = VecInit.tabulate(4)({ sew => smul_prod >> ((8 << sew) - 1) })(out_eew_pipe) + Cat(0.U(1.W), rounding_incr).asSInt
  val smul_clip_neg = VecInit.tabulate(4)({ sew => (-1 << ((8 << sew)-1)).S })(out_eew_pipe)
  val smul_clip_pos = VecInit.tabulate(4)({ sew => ((1 << ((8 << sew)-1)) - 1).S })(out_eew_pipe)
  val smul_clip_hi = smul > smul_clip_pos
  val smul_clip_lo = smul < smul_clip_neg
  val smul_clipped = Mux(smul_clip_hi, smul_clip_pos, 0.S) | Mux(smul_clip_lo, smul_clip_neg, 0.S) | Mux(!smul_clip_hi && !smul_clip_lo, smul, 0.S)
  val smul_sat = smul_clip_hi || smul_clip_lo
  val smul_splat = VecInit.tabulate(4)({ sew => Fill(dLenB >> sew, smul_clipped((8<<sew)-1,0)) })(out_eew_pipe)

  val adder_arr = Module(new AdderArray(dLenB))
  adder_arr.io.in1 := Mux(ctrl_wmul, wide, lo).asTypeOf(Vec(dLenB, UInt(8.W)))
  adder_arr.io.in2 := Mux(ctrl_pipe.bool(MULAccumulate), Mux(ctrl_pipe.bool(MULSwapVdV2), in_vs2_pipe, in_vd_pipe), 0.U(dLen.W)).asTypeOf(Vec(dLenB, UInt(8.W)))
  adder_arr.io.incr := VecInit.fill(dLenB)(false.B)
  adder_arr.io.mask_carry := 0.U
  adder_arr.io.signed := DontCare
  adder_arr.io.eew := out_eew_pipe
  adder_arr.io.avg := false.B
  adder_arr.io.rm := DontCare
  adder_arr.io.sub := ctrl_pipe.bool(MULSub)
  adder_arr.io.cmask := false.B

  val add_out = adder_arr.io.out

  val pipe_out = Mux(ctrl_smul, smul_splat, 0.U) | Mux(ctrl_pipe.bool(MULHi), hi, 0.U) | Mux(!ctrl_smul && !ctrl_pipe.bool(MULHi), add_out.asUInt, 0.U)
  // val pipe_out = Pipe(io.pipe(2).valid, out, depth-3).bits
  // val pipe_vxsat = Pipe(io.pipe(2).valid, smul_sat && ctrl_smul, depth-3).bits
  val pipe_vxsat = smul_sat && ctrl_smul
  io.pipe0_stall     := false.B
  io.write.valid     := io.pipe(2).valid
  io.write.bits.eg   := io.pipe(2).bits.wvd_eg
  io.write.bits.data := pipe_out
  io.write.bits.mask := FillInterleaved(8, io.pipe(2).bits.wmask)

  io.set_vxsat := io.pipe(2).valid && pipe_vxsat
  io.scalar_write.valid := false.B
  io.scalar_write.bits := DontCare
}
