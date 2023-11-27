package vector.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import vector.common._

class IterativeIntegerDivider(implicit p: Parameters) extends IterativeFunctionalUnit()(p) {
  lazy val aluFn = new ALUFN
  lazy val opcodes = Seq(
    OPMFunct6.divu,
    OPMFunct6.div ,
    OPMFunct6.remu,
    OPMFunct6.rem ,
  )

  lazy val div = Module(new MulDiv(MulDivParams(mulUnroll = 0), 64, 1))

  override def accepts(f3: UInt, f6: UInt): Bool = VecDecode(f3, f6, opcodes) && div.io.req.ready

  io.iss.sub_dlen := log2Ceil(dLenB).U - io.iss.op.rvs1_eew

  div.io.req.valid := io.iss.valid && io.iss.ready

  val ctrl_fn = VecInit(Seq(aluFn.FN_DIVU, aluFn.FN_DIV, aluFn.FN_REMU, aluFn.FN_REM))(io.iss.op.funct6(1,0))
  val ctrl_signed = io.iss.op.funct6(0)

  div.io.req.bits.fn := ctrl_fn
  div.io.req.bits.in1 := extract(io.iss.op.rvs2_data, ctrl_signed, io.iss.op.rvs1_eew, io.iss.op.eidx).asUInt
  div.io.req.bits.in2 := extract(io.iss.op.rvs1_data, ctrl_signed, io.iss.op.rvs1_eew, io.iss.op.eidx).asUInt
  div.io.req.bits.dw  := DW_64
  div.io.req.bits.tag := DontCare

  div.io.kill := false.B

  io.hazard.valid       := valid
  io.hazard.bits.vat    := op.vat
  io.hazard.bits.eg     := op.wvd_eg
  io.hazard.bits.widen2 := false.B

  val wdata = VecInit.tabulate(4)({ eew => Fill(dLenB >> eew, div.io.resp.bits.data((8<<eew)-1,0)) })(op.rvs1_eew)
  div.io.resp.ready  := io.write.ready
  io.write.valid     := div.io.resp.valid
  io.write.bits.eg   := op.wvd_eg
  io.write.bits.mask := FillInterleaved(8, op.wmask)
  io.write.bits.data := wdata

  last := io.write.fire()
}
