package vector.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import vector.common._
import vector.insns._

class PermuteUnit(implicit p: Parameters) extends PipelinedFunctionalUnit(1)(p) {
  val supported_insns = Seq(
    SLIDEUP.VI, SLIDEUP.VX, SLIDEDOWN.VI, SLIDEDOWN.VX,
    SLIDE1UP.VX, SLIDE1DOWN.VX, FSLIDE1UP.VF, FSLIDE1DOWN.VF
  )

  io.iss.sub_dlen := 0.U
  io.iss.ready := new VectorDecoder(io.iss.op.funct3, io.iss.op.funct6, io.iss.op.rs1, io.iss.op.rs2,
    supported_insns, Nil).matched

  val slide_up = !io.pipe(0).bits.funct6(0)
  val slide1 = !io.pipe(0).bits.isOpi
  val slide1down_mask = VecInit.tabulate(4)({sew => if (sew == 3 && dLenB == 8) { ~(0.U(8.W)) } else {
    FillInterleaved(1 << sew, UIntToOH(io.pipe(0).bits.vl(dLenOffBits-sew-1,0)-1.U))
  }})(io.pipe(0).bits.rvs2_eew)
  val slide1up_mask = eewByteMask(io.pipe(0).bits.rvs2_eew)
  val slide1_mask = Mux(slide_up,
    Mux(io.pipe(0).bits.head, slide1up_mask, 0.U),
    Mux(io.pipe(0).bits.tail, slide1down_mask, 0.U))
  val use_rvs1_mask = FillInterleaved(8, Mux(slide1, slide1_mask, 0.U).pad(dLenB))


  io.scalar_write.valid := false.B
  io.scalar_write.bits := DontCare
  io.set_vxsat := false.B
  io.set_fflags.valid := false.B
  io.set_fflags.bits := DontCare
  io.write.valid := io.pipe(0).valid
  io.write.bits.eg := io.pipe(0).bits.wvd_eg
  io.write.bits.mask := FillInterleaved(8, io.pipe(0).bits.wmask)
  io.write.bits.data := (io.pipe(0).bits.rvs2_data & ~use_rvs1_mask) | (io.pipe(0).bits.rvs1_data & use_rvs1_mask)
}
