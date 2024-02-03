package saturn.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import saturn.common._
import saturn.insns._

class BitwisePipe(implicit p: Parameters) extends PipelinedFunctionalUnit(1)(p) {
  val supported_insns = Seq(
    AND.VV, AND.VX, AND.VI, OR.VV, OR.VX, OR.VI, XOR.VV, XOR.VX, XOR.VI,
    MANDNOT.VV, MAND.VV, MOR.VV, MXOR.VV, MORNOT.VV, MNAND.VV, MNOR.VV, MXNOR.VV,
    REDAND.VV, REDOR.VV, REDXOR.VV
  )

  val ctrl = new VectorDecoder(io.pipe(0).bits.funct3, io.pipe(0).bits.funct6, 0.U, 0.U, supported_insns,
    Seq(BWAnd, BWOr, BWXor, BWInvOut, BWInv1))
  io.iss.ready := new VectorDecoder(io.iss.op.funct3, io.iss.op.funct6, 0.U, 0.U, supported_insns, Nil).matched

  val in1 = Mux(ctrl.bool(BWInv1), ~io.pipe(0).bits.rvs1_data, io.pipe(0).bits.rvs1_data)
  val in2 = io.pipe(0).bits.rvs2_data
  val op = Mux1H(Seq(
    (ctrl.bool(BWAnd), (in1 & in2)),
    (ctrl.bool(BWOr) , (in1 | in2)),
    (ctrl.bool(BWXor), (in1 ^ in2))
  ))
  val out = Mux(ctrl.bool(BWInvOut), ~op, op)

  io.pipe0_stall     := false.B
  io.write.valid := io.pipe(0).valid
  io.write.bits.eg := io.pipe(0).bits.wvd_eg
  io.write.bits.mask := Mux(io.pipe(0).bits.isOpm && !io.pipe(0).bits.acc,
    io.pipe(0).bits.full_tail_mask,
    FillInterleaved(8, io.pipe(0).bits.wmask))
  io.write.bits.data := out

  io.set_vxsat := false.B
  io.set_fflags.valid := false.B
  io.set_fflags.bits := DontCare
  io.scalar_write.valid := false.B
  io.scalar_write.bits := DontCare
}
