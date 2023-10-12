package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

class VectorIssueInst(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val bits = UInt(32.W)
  val vconfig = new VConfig
  val vstart = UInt(log2Ceil(maxVLMax).W)
  val rs1_data = UInt(xLen.W)
  val rs2_data = UInt(xLen.W)
  val vat = UInt(vParams.vatSz.W)
  val phys = Bool()

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
  def imm4 = bits(19,15)
  def funct6 = bits(31,26)

  def isOpi = funct3.isOneOf(OPIVV, OPIVI, OPIVX)
  def isOpm = funct3.isOneOf(OPMVV, OPMVX)
  def isOpf = funct3.isOneOf(OPFVV, OPFVF)
}

class VectorWrite(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val eg = UInt(log2Ceil(egsTotal).W)
  val data = UInt(dLen.W)
  val mask = UInt(dLenB.W)
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
