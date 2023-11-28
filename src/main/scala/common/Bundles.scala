package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

class VectorMemMacroOp(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val vat = UInt(vParams.vatSz.W)
  val phys = Bool()

  val base_addr = UInt(xLen.W)
  val stride    = UInt(xLen.W)

  val vstart = UInt(log2Ceil(maxVLMax).W)
  val vl = UInt((1+log2Ceil(maxVLMax)).W)

  val mop = UInt(2.W)
  val vm = Bool()
  val nf = UInt(3.W)

  val idx_size = UInt(2.W)
  val elem_size = UInt(2.W)
  val whole_reg = Bool()
  val store = Bool()

  def seg_nf = Mux(whole_reg, 0.U, nf)
  def wr_nf = Mux(whole_reg, nf, 0.U)
}


class VectorIssueInst(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val bits = UInt(32.W)
  val vconfig = new VConfig
  val vstart = UInt(log2Ceil(maxVLMax).W)
  val rs1_data = UInt(xLen.W)
  val rs2_data = UInt(xLen.W)
  val vat = UInt(vParams.vatSz.W)
  val phys = Bool()
  val vxrm = UInt(2.W)

  def opcode = bits(6,0)
  def store = opcode(5)
  def mem_idx_size = bits(13,12)
  def mem_elem_size = Mux(mop(0), vconfig.vtype.vsew, bits(13,12))
  def mop = bits(27,26)
  def vm = bits(25)
  def umop = bits(24,20)
  def nf = bits(31,29)
  def wr = mop === mopUnit && umop === lumopWhole
  def seg_nf = Mux(wr, 0.U, nf)
  def wr_nf = Mux(wr, nf, 0.U)
  def pos_lmul = Mux(vconfig.vtype.vlmul_sign, 0.U, vconfig.vtype.vlmul_mag)
  def vmu = opcode.isOneOf(opcLoad, opcStore)
  def rs1 = bits(19,15)
  def rs2 = bits(24,20)
  def rd  = bits(11,7)
  def may_write_v0 = rd === 0.U && opcode =/= opcStore
  def funct3 = bits(14,12)
  def imm5 = bits(19,15)
  def funct6 = bits(31,26)

  def isOpi = funct3.isOneOf(OPIVV, OPIVI, OPIVX)
  def isOpm = funct3.isOneOf(OPMVV, OPMVX)
  def isOpf = funct3.isOneOf(OPFVV, OPFVF)

  def opmf6 = Mux(isOpm, OPMFunct6(funct6), OPMFunct6.illegal)
  def opif6 = Mux(isOpi, OPIFunct6(funct6), OPIFunct6.illegal)
  def opff6 = Mux(isOpf, OPFFunct6(funct6), OPFFunct6.illegal)
}

class VectorWrite(writeBits: Int)(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val eg = UInt(log2Ceil(32 * vLen / writeBits).W)
  val data = UInt(writeBits.W)
  val mask = UInt(writeBits.W)
}

class VectorReadIO(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val req = Decoupled(UInt(log2Ceil(egsTotal).W))
  val resp = Input(UInt(dLen.W))
}

class VectorIndexAccessIO(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val ready = Output(Bool())
  val valid = Input(Bool())
  val vrs = Input(UInt(5.W))
  val eidx = Input(UInt((1+log2Ceil(maxVLMax)).W))
  val eew = Input(UInt(2.W))
  val idx = Output(UInt(64.W))
}

class VectorMaskAccessIO(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val ready = Output(Bool())
  val valid = Input(Bool())
  val eidx = Input(UInt((1+log2Ceil(maxVLMax)).W))
  val mask = Output(Bool())
}

class MaskedByte extends Bundle {
  val data = UInt(8.W)
  val mask = Bool()
}

class VectorMicroOp(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val wvd = Bool()
  val eidx = UInt(log2Ceil(maxVLMax).W)

  val rvs1_data = UInt(dLen.W)
  val rvs2_data = UInt(dLen.W)
  val rvd_data  = UInt(dLen.W)

  val rvs1_eew = UInt(2.W)
  val rvs2_eew = UInt(2.W)
  val rvd_eew = UInt(2.W)
  val vd_eew  = UInt(2.W)

  val rmask   = UInt(dLenB.W)
  val wmask   = UInt(dLenB.W)

  val wvd_eg   = UInt(log2Ceil(egsTotal).W)
  val wvd_widen2 = Bool()

  val funct3 = UInt(3.W)
  def isOpi = funct3.isOneOf(OPIVV, OPIVI, OPIVX)
  def isOpm = funct3.isOneOf(OPMVV, OPMVX)
  def isOpf = funct3.isOneOf(OPFVV, OPFVF)

  def opmf6 = Mux(isOpm, OPMFunct6(funct6), OPMFunct6.illegal)
  def opif6 = Mux(isOpi, OPIFunct6(funct6), OPIFunct6.illegal)
  def opff6 = Mux(isOpf, OPFFunct6(funct6), OPFFunct6.illegal)

  val funct6 = UInt(6.W)
  val rs1 = UInt(5.W)
  val vm = Bool()

  val last = Bool()
  val vat = UInt(vParams.vatSz.W)

  val vxrm = UInt(2.W)
}

class PipeHazard(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val vat = UInt(vParams.vatSz.W)
  val eg = UInt(log2Ceil(egsTotal).W)
  val widen2 = Bool()
  def eg_oh = Mux(widen2, FillInterleaved(2, UIntToOH(eg >> 1) ), UIntToOH(eg))
}
