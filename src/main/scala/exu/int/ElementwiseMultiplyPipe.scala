package saturn.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import saturn.common._
import saturn.insns._

class ElementwiseMultiplyPipe(depth: Int)(implicit p: Parameters) extends PipelinedFunctionalUnit(depth)(p) {
  val supported_insns = Seq(
    MUL.VV, MUL.VX, MULH.VV, MULH.VX,
    MULHU.VV, MULHU.VX, MULHSU.VV, MULHSU.VX,
    WMUL.VV, WMUL.VX, WMULU.VV, WMULU.VX,
    WMULSU.VV, WMULSU.VX,
    MACC.VV, MACC.VX, NMSAC.VV, NMSAC.VX,
    MADD.VV, MADD.VX, NMSUB.VV, NMSUB.VX,
    WMACC.VV, WMACC.VX, WMACCU.VV, WMACCU.VX,
    WMACCSU.VV , WMACCSU.VX, WMACCUS.VV, WMACCUS.VX,
    SMUL.VV, SMUL.VX).map(_.elementWise)

  io.iss.ready := new VectorDecoder(io.iss.op.funct3, io.iss.op.funct6, 0.U, 0.U, supported_insns, Nil).matched
  io.set_vxsat := false.B
  io.set_fflags.valid := false.B
  io.set_fflags.bits := DontCare

  val ctrl = new VectorDecoder(io.pipe(depth-1).bits.funct3, io.pipe(depth-1).bits.funct6, 0.U, 0.U, supported_insns, Seq(
    MULHi, MULSign1, MULSign2, MULSwapVdV2, MULAccumulate, MULSub))

  val ctrl_smul = io.pipe(depth-1).bits.isOpi

  val in_eew = io.pipe(depth-1).bits.rvs1_eew
  val out_eew = io.pipe(depth-1).bits.vd_eew
  val eidx = io.pipe(depth-1).bits.eidx

  val in_vs1 = Mux(ctrl.bool(MULSign1), sextElem(io.pipe(depth-1).bits.rvs1_elem, in_eew), io.pipe(depth-1).bits.rvs1_elem)
  val in_vs2 = Mux(ctrl.bool(MULSign2), sextElem(io.pipe(depth-1).bits.rvs2_elem, in_eew), io.pipe(depth-1).bits.rvs2_elem)
  val in_vd  = io.pipe(depth-1).bits.rvd_elem

  val prod = in_vs1.asSInt * Mux(ctrl.bool(MULSwapVdV2), in_vd, in_vs2).asSInt
  val hi = VecInit.tabulate(4)({ eew => prod >> (8 << eew) })(out_eew)(63,0)
  val lo = VecInit.tabulate(4)({ eew => prod((8 << eew)-1,0)})(out_eew)(63,0)
  val madd = Mux(ctrl.bool(MULSub), ~lo, lo) + ctrl.bool(MULSub) + Mux(ctrl.bool(MULSwapVdV2), in_vs2, in_vd)
  val rounding_incr = VecInit.tabulate(4)({ eew => RoundingIncrement(io.pipe(depth-1).bits.vxrm, prod((8 << eew)-1,0)) })(out_eew)
  val smul = VecInit.tabulate(4)({ eew => prod >> ((8 << eew) - 1) })(out_eew) + Cat(0.U(1.W), rounding_incr).asSInt
  val smul_clip_neg = VecInit.tabulate(4)({ eew => (-1 << ((8 << eew)-1)).S })(out_eew)
  val smul_clip_pos = VecInit.tabulate(4)({ eew => ((1 << ((8 << eew)-1)) - 1).S })(out_eew)
  val smul_clip_hi = smul > smul_clip_pos
  val smul_clip_lo = smul < smul_clip_neg
  val smul_clipped = Mux(smul_clip_hi, smul_clip_pos, 0.S) | Mux(smul_clip_lo, smul_clip_neg, 0.S) | Mux(!smul_clip_hi && !smul_clip_lo, smul, 0.S)
  val smul_sat = smul_clip_hi || smul_clip_lo
  val out = Mux(ctrl.bool(MULAccumulate), madd, 0.U) | Mux(ctrl_smul, smul_clipped.asUInt, 0.U) | Mux(!ctrl.bool(MULAccumulate) && !ctrl_smul, Mux(ctrl.bool(MULHi), hi, lo), 0.U)

  val pipe_out = out(63,0)
  val pipe_vxsat = smul_sat && ctrl_smul

  val wdata = VecInit.tabulate(4)({ eew => Fill(dLenB >> eew, pipe_out((8<<eew)-1,0)) })(io.pipe(depth-1).bits.vd_eew)
  io.pipe0_stall     := false.B
  io.write.valid     := io.pipe(depth-1).valid
  io.write.bits.eg   := io.pipe(depth-1).bits.wvd_eg
  io.write.bits.data := wdata
  io.write.bits.mask := FillInterleaved(8, io.pipe(depth-1).bits.wmask)

  io.set_vxsat := io.pipe(depth-1).valid && pipe_vxsat
  io.scalar_write.valid := false.B
  io.scalar_write.bits := DontCare
}
