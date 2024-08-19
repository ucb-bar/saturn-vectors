package saturn.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import saturn.common._
import saturn.insns._

case object PermuteUnitFactory extends FunctionalUnitFactory {
  def insns = Seq(
    SLIDEUP.VI, SLIDEUP.VX, SLIDEDOWN.VI, SLIDEDOWN.VX,
    SLIDE1UP.VX, SLIDE1DOWN.VX, FSLIDE1UP.VF, FSLIDE1DOWN.VF,
    RGATHER_VV, RGATHER_VI, RGATHER_VX,
    RGATHEREI16, COMPRESS.VV,
    MVNRR
  ).map(_.pipelined(1))

  def generate(implicit p: Parameters) = new PermuteUnit()(p)
}

class PermuteUnit(implicit p: Parameters) extends PipelinedFunctionalUnit(1)(p) {
  val supported_insns = PermuteUnitFactory.insns

  io.iss.ready := new VectorDecoder(io.iss.op.funct3, io.iss.op.funct6, io.iss.op.rs1, io.iss.op.rs2,
    supported_insns, Nil).matched

  val wvd_reg = Reg(UInt(5.W))
  val result_reg = Reg(UInt(64.W))

  val mvnrr = io.pipe(0).bits.funct3 === OPIVV && io.pipe(0).bits.opif6 === OPIFunct6.mvnrr
  val compress = io.pipe(0).bits.opmf6 === OPMFunct6.compress
  val rgatherei16 = io.pipe(0).bits.funct3 === OPIVV && io.pipe(0).bits.opif6 === OPIFunct6.rgatherei16
  val rgather = io.pipe(0).bits.opif6 === OPIFunct6.rgather || rgatherei16


  val index_eew = Mux(rgatherei16, 1.U, io.pipe(0).bits.rvs2_eew)
  val elem_eidx = Mux(rgather, io.pipe(0).bits.rvs1_data, io.pipe(0).bits.eidx)
  val elem = VecInit.tabulate(4)({sew => if (sew == 3 && dLenB == 8) {
    io.pipe(0).bits.rvs2_data
  } else {
    io.pipe(0).bits.rvs2_data.asTypeOf(Vec(dLenB >> sew, UInt((8 << sew).W)))(elem_eidx)
  }})(io.pipe(0).bits.rvs2_eew)
  val rgather_elem = Mux(io.pipe(0).bits.head || io.pipe(0).bits.funct3 === OPIVV, elem, result_reg)
  val splat = dLenSplat(Mux(compress, elem, rgather_elem), io.pipe(0).bits.rvs2_eew)

  val compress_wvd = Mux(io.pipe(0).bits.head, io.pipe(0).bits.wvd_eg >> log2Ceil(egsPerVReg), wvd_reg)
  val compress_bit = (io.pipe(0).bits.rvs1_data >> io.pipe(0).bits.eidx(log2Ceil(dLen)-1,0))(0)
  val compress_eidx = Mux(io.pipe(0).bits.head, 0.U, result_reg)(log2Ceil(maxVLMax),0)

  when (io.pipe(0).valid && io.pipe(0).bits.head && rgather) {
    result_reg := elem
  }
  when (io.pipe(0).valid && io.pipe(0).bits.head) {
    wvd_reg := io.pipe(0).bits.wvd_eg >> log2Ceil(egsPerVReg)
  }
  when (io.pipe(0).valid && compress) {
    result_reg := (compress_eidx + compress_bit)(log2Ceil(maxVLMax),0)
  }

  val shifted_mask_eidx = Mux(compress, compress_eidx, io.pipe(0).bits.vl - 1.U)
  val shifted_mask = VecInit.tabulate(4)({sew => if (sew == 3 && dLenB == 8) { ~(0.U(8.W)) } else {
    FillInterleaved(1 << sew, UIntToOH(shifted_mask_eidx(dLenOffBits-sew-1,0)))
  }})(io.pipe(0).bits.rvs2_eew)


  val slide_up = !io.pipe(0).bits.funct6(0)
  val slide1 = !io.pipe(0).bits.isOpi
  val slide1up_mask = eewByteMask(io.pipe(0).bits.rvs2_eew)
  val slide1_mask = Mux(slide_up,
    Mux(io.pipe(0).bits.head, slide1up_mask, 0.U),
    Mux(io.pipe(0).bits.tail, shifted_mask, 0.U))
  val use_rvs1_mask = FillInterleaved(8, Mux(slide1, slide1_mask, 0.U).pad(dLenB))

  val wmask = Mux(mvnrr, ~(0.U(dLenB.W)),
    Mux(compress, Mux(compress_bit, shifted_mask, 0.U), io.pipe(0).bits.wmask))

  io.scalar_write.valid := false.B
  io.scalar_write.bits := DontCare
  io.set_vxsat := false.B
  io.set_fflags.valid := false.B
  io.set_fflags.bits := DontCare

  io.write.valid := io.pipe(0).valid && (!compress || compress_bit)
  io.write.bits.eg := Mux(compress,
    getEgId(compress_wvd, compress_eidx, io.pipe(0).bits.rvs2_eew, false.B),
    io.pipe(0).bits.wvd_eg)
  io.write.bits.mask := FillInterleaved(8, wmask)
  io.write.bits.data := Mux(rgather || compress,
    splat,
    (io.pipe(0).bits.rvs2_data & ~use_rvs1_mask) | (io.pipe(0).bits.rvs1_data & use_rvs1_mask))
}
