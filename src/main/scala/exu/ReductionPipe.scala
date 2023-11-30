package vector.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import vector.common._

class IntegerReductionPipe(implicit p: Parameters) extends IterativeFunctionalUnit()(p) {
  lazy val opcodes = Seq(
    OPMFunct6.redsum,
  )

  override def accepts(f3: UInt, f6: UInt): Bool = VecDecode(f3, f6, opcodes)

  io.iss.sub_dlen := log2Ceil(dLenB).U - io.iss.op.rvs1_eew
  io.set_vxsat := false.B

  // Capture vs1[0] and wvd_eg
  val vs1_0 = Reg(UInt(64.W))
  val wvd_eg = Reg(UInt(log2Ceil(egsTotal).W))

  val in_eew = io.iss.op.rvs2_eew
  val out_eew = io.iss.op.vd_eew
  val eidx = io.iss.op.eidx

  val vs2_i_extract = extract(io.iss.op.rvs2_data, false.B, in_eew, eidx)(63,0)
  val vs2_i_mask = io.iss.op.wmask((eidx << in_eew) % dLenB.U)
  val vs2_i = Mux(io.iss.op.vm || vs2_i_mask, vs2_i_extract, 0.U)

  val active = RegInit(false.B)
  when (active && io.write.fire()) {
    active := false.B
  } .elsewhen (!active && io.iss.valid) {
    active := true.B
  }
  
  io.hazard.valid := active
  io.hazard.bits.vat := op.vat
  io.hazard.bits.eg := wvd_eg
  io.hazard.bits.widen2 := false.B

  when (io.iss.valid && !active) {
    vs1_0 := extract(io.iss.op.rvs1_data, false.B, in_eew, 0.U)(63,0)
    wvd_eg := io.iss.op.wvd_eg
  }

  // Accumulator register
  val red_accum = RegInit(0.S(64.W))

  when (io.iss.valid && active) {
    red_accum := red_accum + vs2_i.asSInt
  } .elsewhen (io.iss.valid && !active) {
    red_accum := vs2_i.asSInt
  }

  val mask_select = Seq(FillInterleaved(8, "b00000001".U),
                        FillInterleaved(8, "b00000011".U),
                        FillInterleaved(8, "b00001111".U),
                        FillInterleaved(8, "b11111111".U))

  val wdata = Wire(SInt(64.W)) 
  wdata := red_accum + vs1_0.asSInt

  io.write.valid := active && op.last
  io.write.bits.eg := wvd_eg
  io.write.bits.data := Cat(0.U((dLen-32).W), wdata)
  io.write.bits.mask := mask_select(out_eew) 

  last := true.B

  io.iss.ready := accepts(io.iss.op.funct3, io.iss.op.funct6) && !(active && op.last)
  io.vat.valid := io.write.fire()
}
