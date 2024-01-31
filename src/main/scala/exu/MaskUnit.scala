package saturn.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import saturn.common._
import saturn.insns._

class MaskUnit(implicit p: Parameters) extends PipelinedFunctionalUnit(1)(p) {
  val supported_insns = Seq(MV_S_X, MV_X_S, POPC, FIRST, FMV_S_F, FMV_F_S, MSBF, MSOF, MSIF, IOTA, ID)

  val scalar_wb_busy = RegInit(false.B)
  val scalar_wb_data = Reg(UInt(64.W))
  val scalar_wb_rd = Reg(UInt(5.W))
  val scalar_wb_fp = Reg(Bool())
  val scalar_wb_size = Reg(UInt(2.W))
  val found_first = Reg(Bool())

  def accepts(op: ExecuteMicroOp): Bool = (op.opff6.isOneOf(OPFFunct6.wrfunary0) || op.opmf6.isOneOf(OPMFunct6.wrxunary0, OPMFunct6.munary0)) && !scalar_wb_busy

  io.iss.ready := new VectorDecoder(io.iss.op.funct3, io.iss.op.funct6, io.iss.op.rs1, io.iss.op.rs2, supported_insns, Nil).matched && !scalar_wb_busy

  io.iss.sub_dlen := 0.U
  io.set_vxsat := false.B
  io.set_fflags.valid := false.B
  io.set_fflags.bits := DontCare

  val op = io.pipe(0).bits
  val opmvv = op.funct3 === OPMVV
  val opmvx = op.funct3 === OPMVX
  val opfvv = op.funct3 === OPFVV
  val opfvf = op.funct3 === OPFVF

  val wxunary0 = opmvv && !op.funct6(2)
  val rxunary0 = opmvx
  val wfunary0 = opfvv
  val rfunary0 = opfvf
  val munary0  = opmvv && op.funct6(2)

  val set_before = op.rs1.isOneOf(1.U, 3.U)
  val set_first = op.rs1.isOneOf(2.U, 3.U)

  val elems = (op.rvs2_data & op.rvm_data & op.full_tail_mask)
  val popc = PopCount(elems)
  val ff = PriorityEncoder(elems)
  val ff_oh = PriorityEncoderOH(elems)
  val bf = ~((0 until dLen).map { i => (elems << i)(dLen-1,0) }.reduce(_|_))
  val nonzero = elems =/= 0.U
  val first_here = !found_first && nonzero
  val before = Mux(found_first, 0.U, Mux(nonzero, bf, ~(0.U(dLen.W))))
  val first = Mux(first_here, ff_oh, 0.U)
  val set = Mux(set_before, before, 0.U) | Mux(set_first, first, 0.U)
  val sign = VecInit.tabulate(4)({sew => op.rvs2_data((8 << sew)-1)})(op.rvs2_eew)
  val eew_mask = eewBitMask(op.rvs2_eew).pad(64)
  val elem = (op.rvs2_data & eew_mask) | (Fill(64, sign && op.isOpm) & ~eew_mask)

  val iota_dlenb = VecInit.tabulate(4)({sew =>
    val grouped = Mux(op.rs1(0), ~(0.U(dLen.W)), elems).asTypeOf(Vec(8 << sew, UInt((dLenB >> sew).W)))
    grouped(op.eidx(log2Ceil(dLen)-1,log2Ceil(dLenB) - sew))
  })(op.rvd_eew)
  val iota_sums = (0 until dLenB).map { i =>
    (PopCount(iota_dlenb & ((1<<i)-1).U) +& scalar_wb_data)(log2Ceil(maxVLMax),0)
  }
  val iota_out = VecInit.tabulate(4)({sew =>
    val out = Wire(Vec(dLenB >> sew, UInt((8<<sew).W)))
    out := iota_sums.take(dLenB >> sew)
    out.asUInt
  })(op.vd_eew)

  when (io.iss.valid && io.iss.ready && io.iss.op.head) {
    scalar_wb_data := 0.U
    found_first := false.B
  }

  when (io.pipe(0).valid) {
    scalar_wb_rd := io.pipe(0).bits.rd
    scalar_wb_size := io.pipe(0).bits.rvs2_eew
    when (first_here) { found_first := true.B }
    when (wxunary0) {
      when (op.rs1 === 16.U) { // popc
        scalar_wb_data := (scalar_wb_data + popc)(log2Ceil(maxVLMax),0)
      } .elsewhen (op.rs1 === 17.U) { // first
        when (first_here) {
          scalar_wb_data := op.eidx + ff
        } .otherwise {
          scalar_wb_data := ~(0.U(64.W))
        }
      } .otherwise { // mv
        scalar_wb_data := elem
      }
    }
    when (wfunary0) { // fmv
      scalar_wb_data := elem
    }
    when (munary0) {
      val mask = VecInit.tabulate(4)({sew => ~(0.U((dLenB >> sew).W))})(op.vd_eew)
      val incr = PopCount(iota_dlenb & mask)
      scalar_wb_data := (scalar_wb_data + incr)(log2Ceil(maxVLMax),0)
    }
    when (op.tail) {
      scalar_wb_busy := wxunary0 || wfunary0
      scalar_wb_fp := wfunary0
    }
  }

  io.scalar_write.valid := scalar_wb_busy
  io.scalar_write.bits.data := scalar_wb_data
  io.scalar_write.bits.rd := scalar_wb_rd
  io.scalar_write.bits.fp := scalar_wb_fp
  io.scalar_write.bits.size := scalar_wb_size

  io.pipe0_stall     := false.B
  io.write.valid     := io.pipe(0).valid && (rxunary0 || rfunary0 || munary0)
  io.write.bits.eg   := op.wvd_eg
  io.write.bits.mask := Mux1H(Seq(
    (rxunary0 || rfunary0 , eewBitMask(op.vd_eew)),
    (munary0 && op.rs1(4) , FillInterleaved(8, op.wmask)),
    (munary0 && !op.rs1(4), op.full_tail_mask & op.rvm_data)
  ))
  io.write.bits.data := Mux1H(Seq(
    (rxunary0 || rfunary0 , op.rvs1_data(63,0)),
    (munary0 && op.rs1(4) , iota_out),
    (munary0 && !op.rs1(4), set)
  ))

  when (io.scalar_write.fire) { scalar_wb_busy := false.B }
}
