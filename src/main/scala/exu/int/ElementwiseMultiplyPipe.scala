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
  val supported_insns = IntegerMultiplyFactory(depth, false).insns

  io.iss.ready := new VectorDecoder(io.iss.op.funct3, io.iss.op.funct6, 0.U, 0.U, supported_insns, Nil).matched
  io.set_vxsat := false.B
  io.set_fflags.valid := false.B
  io.set_fflags.bits := DontCare

  val ctrl = new VectorDecoder(io.pipe(0).bits.funct3, io.pipe(0).bits.funct6, 0.U, 0.U, supported_insns, Seq(
    MULHi, MULSign1, MULSign2, MULSwapVdV2, MULAccumulate, MULSub))

  val in_eew = io.pipe(0).bits.rvs1_eew
  val eidx = io.pipe(0).bits.eidx

  val in_vs1 = Mux(ctrl.bool(MULSign1), sextElem(io.pipe(0).bits.rvs1_elem, in_eew), io.pipe(0).bits.rvs1_elem)
  val in_vs2 = Mux(ctrl.bool(MULSign2), sextElem(io.pipe(0).bits.rvs2_elem, in_eew), io.pipe(0).bits.rvs2_elem)
  val in_vd  = io.pipe(0).bits.rvd_elem

  val prod = in_vs1.asSInt * Mux(ctrl.bool(MULSwapVdV2), in_vd, in_vs2).asSInt
  ///////////////// pipe
  val prod_pipe = Pipe(io.pipe(0).valid, prod, depth-2).bits
  val in_vs2_pipe = Pipe(io.pipe(0).valid, in_vs2, depth-2).bits
  val in_vd_pipe = Pipe(io.pipe(0).valid, in_vd, depth-2).bits
  val ctrl_MULSub = Pipe(io.pipe(0).valid, ctrl.bool(MULSub), depth-2).bits
  val ctrl_MULSwapVdV2 = Pipe(io.pipe(0).valid, ctrl.bool(MULSwapVdV2), depth-2).bits
  val ctrl_MULAccumulate = Pipe(io.pipe(0).valid, ctrl.bool(MULAccumulate), depth-2).bits
  val ctrl_MULHi = Pipe(io.pipe(0).valid, ctrl.bool(MULHi), depth-2).bits 
  val ctrl_smul = io.pipe(depth-2).bits.isOpi
  val out_eew = io.pipe(depth-2).bits.vd_eew

  val hi = VecInit.tabulate(4)({ eew => prod_pipe >> (8 << eew) })(out_eew)(63,0)
  val lo = VecInit.tabulate(4)({ eew => prod_pipe((8 << eew)-1,0)})(out_eew)(63,0)
  val madd = Mux(ctrl_MULSub, ~lo, lo) + ctrl_MULSub + Mux(ctrl_MULSwapVdV2, in_vs2_pipe, in_vd_pipe)
  val rounding_incr = VecInit.tabulate(4)({ eew => RoundingIncrement(io.pipe(depth-2).bits.vxrm, prod_pipe((8 << eew)-1,0)) })(out_eew)
  val smul = VecInit.tabulate(4)({ eew => prod_pipe >> ((8 << eew) - 1) })(out_eew) + Cat(0.U(1.W), rounding_incr).asSInt
  val smul_clip_neg = VecInit.tabulate(4)({ eew => (-1 << ((8 << eew)-1)).S })(out_eew)
  val smul_clip_pos = VecInit.tabulate(4)({ eew => ((1 << ((8 << eew)-1)) - 1).S })(out_eew)
  val smul_clip_hi = smul > smul_clip_pos
  val smul_clip_lo = smul < smul_clip_neg
  val smul_clipped = Mux(smul_clip_hi, smul_clip_pos, 0.S) | Mux(smul_clip_lo, smul_clip_neg, 0.S) | Mux(!smul_clip_hi && !smul_clip_lo, smul, 0.S)
  val smul_sat = smul_clip_hi || smul_clip_lo
  val out = Mux(ctrl_MULAccumulate, madd, 0.U) | Mux(ctrl_smul, smul_clipped.asUInt, 0.U) | Mux(!ctrl_MULAccumulate && !ctrl_smul, Mux(ctrl_MULHi, hi, lo), 0.U)

  ///////////////// pipe
  val pipe_out = Pipe(io.pipe(depth-2).valid, out(63,0), 1).bits
  val pipe_vxsat = Pipe(io.pipe(depth-2).valid, smul_sat && ctrl_smul, 1).bits
  val wdata = VecInit.tabulate(4)({ eew => Fill(dLenB >> eew, pipe_out((8<<eew)-1,0)) })(io.pipe(depth-1).bits.vd_eew)
  io.pipe0_stall     := false.B
  io.write.valid     := io.pipe(depth-1).valid
  io.write.bits.eg   := io.pipe(depth-1).bits.wvd_eg
  io.write.bits.data := wdata
  io.write.bits.mask := FillInterleaved(8, io.pipe(depth-1).bits.wmask)

  io.set_vxsat := io.pipe(depth-1).valid && pipe_vxsat && (io.pipe(depth-1).bits.wmask =/= 0.U)
  io.scalar_write.valid := false.B
  io.scalar_write.bits := DontCare
}
