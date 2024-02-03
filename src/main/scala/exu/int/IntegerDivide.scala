package saturn.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import saturn.common._
import saturn.insns._

class IterativeIntegerDivider(implicit p: Parameters) extends IterativeFunctionalUnit()(p) {
  val supported_insns = Seq(
    DIVU.VV, DIVU.VX,
    DIV.VV, DIV.VX,
    REMU.VV, REMU.VX,
    REM.VV, REM.VX
  ).map(_.elementWise)

  val div = Module(new MulDiv(MulDivParams(mulUnroll = 0), 64, 1))
  io.iss.ready := new VectorDecoder(io.iss.op.funct3, io.iss.op.funct6, 0.U, 0.U, supported_insns, Nil).matched && div.io.req.ready && (!valid || last)

  io.set_vxsat := false.B
  io.set_fflags.valid := false.B
  io.set_fflags.bits := DontCare

  div.io.req.valid := io.iss.valid && io.iss.ready

  lazy val aluFn = new ALUFN
  val ctrl_fn = VecInit(Seq(aluFn.FN_DIVU, aluFn.FN_DIV, aluFn.FN_REMU, aluFn.FN_REM))(io.iss.op.funct6(1,0))
  val ctrl_signed = io.iss.op.funct6(0)

  div.io.req.bits.fn := ctrl_fn
  div.io.req.bits.in1 := Mux(ctrl_signed,
    sextElem(io.iss.op.rvs2_elem, io.iss.op.rvs2_eew),
    io.iss.op.rvs2_elem)
  div.io.req.bits.in2 := Mux(ctrl_signed,
    sextElem(io.iss.op.rvs1_elem, io.iss.op.rvs1_eew),
    io.iss.op.rvs1_elem)
  div.io.req.bits.dw  := DW_64
  div.io.req.bits.tag := DontCare

  div.io.kill := false.B

  io.hazard.valid       := valid
  io.hazard.bits.vat    := op.vat
  io.hazard.bits.eg     := op.wvd_eg

  val wdata = VecInit.tabulate(4)({ eew => Fill(dLenB >> eew, div.io.resp.bits.data((8<<eew)-1,0)) })(op.rvs1_eew)
  div.io.resp.ready  := io.write.ready
  io.write.valid     := div.io.resp.valid
  io.write.bits.eg   := op.wvd_eg
  io.write.bits.mask := FillInterleaved(8, op.wmask)
  io.write.bits.data := wdata

  io.scalar_write.valid := false.B
  io.scalar_write.bits := DontCare

  last := io.write.fire()

  io.acc := false.B
  io.tail := false.B
}
