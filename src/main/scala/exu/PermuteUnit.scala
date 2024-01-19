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
    SLIDEUP.VI, SLIDEUP.VX, SLIDEDOWN.VI, SLIDEDOWN.VX
  )

  io.iss.sub_dlen := 0.U
  io.iss.ready := new VectorDecoder(io.iss.op.funct3, io.iss.op.funct6, io.iss.op.rs1, io.iss.op.rs2,
    supported_insns, Nil).matched

  io.scalar_write.valid := false.B
  io.scalar_write.bits := DontCare
  io.set_vxsat := false.B
  io.set_fflags.valid := false.B
  io.set_fflags.bits := DontCare
  io.write.valid := io.pipe(0).valid
  io.write.bits.eg := io.pipe(0).bits.wvd_eg
  io.write.bits.mask := FillInterleaved(8, io.pipe(0).bits.wmask)
  io.write.bits.data := io.pipe(0).bits.rvs2_data
}
