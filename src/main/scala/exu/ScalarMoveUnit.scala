package vector.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import vector.common._

class ScalarMoveUnit(implicit p: Parameters) extends IterativeFunctionalUnit()(p) {
  val wb_busy = RegInit(false.B)
  val wb_data = Reg(UInt((1+log2Ceil(dLen)).W))
  val found_first = Reg(Bool())

  def accepts(op: VectorMicroOp): Bool = op.isOpm && OPMFunct6(op.funct6) === OPMFunct6.wrxunary0 && !wb_busy

  io.iss.ready := accepts(io.iss.op) && (!valid || last)

  io.iss.sub_dlen := false.B
  io.set_vxsat := false.B
  io.set_fflags.valid := false.B
  io.set_fflags.bits := DontCare

  val opmvv = !op.funct3(2)
  val opmvx = !opmvv

  val tail_mask = ~(0.U(dLen.W)) >> (0.U(log2Ceil(dLen).W) - op.vl(log2Ceil(dLen)-1,0))
  val elems = (op.rvs2_data &
    Mux(op.vm, ~(0.U(dLen.W)), op.rvm_data) &
    Mux(op.tail, tail_mask, ~(0.U(dLen.W)))
  )
  val popc = PopCount(elems)
  val ff = PriorityEncoder(elems)

  when (io.iss.valid && io.iss.ready) {
    wb_data := 0.U
    found_first := false.B
  }

  when (valid && opmvv && !wb_busy) {
    when (op.rs1 === 16.U) { // popc
      wb_data := wb_data + popc
    } .elsewhen (op.rs1 === 17.U) { // first
      when (!found_first && elems =/= 0.U) {
        found_first := true.B
        wb_data := op.eidx + ff
      }
    }
    when (op.tail) { wb_busy := true.B }
  }

  last := Mux(opmvv, !op.tail || io.scalar_write.fire, io.write.ready)

  io.scalar_write.valid := wb_busy && op.tail && valid && opmvv
  io.scalar_write.bits.data := Mux(op.rs1(0) && !found_first, ~(0.U(64.W)), wb_data)
  io.scalar_write.bits.rd := op.rd
  io.scalar_write.bits.fp := false.B

  io.write.valid     := valid && opmvx
  io.write.bits.eg   := op.wvd_eg
  io.write.bits.mask := eewBitMask(op.vd_eew)
  io.write.bits.data := op.rvs1_data(63,0)

  io.hazard.valid := valid && opmvx
  io.hazard.bits.vat := op.vat
  io.hazard.bits.eg := op.wvd_eg

  when (io.scalar_write.fire) { wb_busy := false.B }
}
