package saturn.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

class VectorMemMacroOp(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val vat = UInt(vParams.vatSz.W)

  val base_offset = UInt(pgIdxBits.W)
  val page        = UInt((paddrBits - pgIdxBits).W)
  val stride      = UInt(pgIdxBits.W)

  val segstart = UInt(3.W)
  val segend = UInt(3.W)
  val vstart = UInt(log2Ceil(maxVLMax).W)
  val vl = UInt((1+log2Ceil(maxVLMax)).W)

  val mop = UInt(2.W)
  val vm = Bool()
  val nf = UInt(3.W)

  val idx_size = UInt(2.W)
  val elem_size = UInt(2.W)
  val whole_reg = Bool()
  val store = Bool()

  def indexed = !mop.isOneOf(mopUnit, mopStrided)
  def seg_nf = Mux(whole_reg, 0.U, nf)
  def wr_nf = Mux(whole_reg, nf, 0.U)
}


class VectorIssueInst(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val pc = UInt(vaddrBitsExtended.W)
  val bits = UInt(32.W)
  val vconfig = new VConfig

  val vstart = UInt(log2Ceil(maxVLMax).W)
  val segstart = UInt(3.W)
  val segend   = UInt(3.W)
  val rs1_data = UInt(xLen.W)
  val rs2_data = UInt(xLen.W)
  val page = UInt((paddrBits - pgIdxBits).W)
  val vat = UInt(vParams.vatSz.W)
  val rm = UInt(3.W)
  val emul = UInt(2.W)

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
  def vmu = opcode.isOneOf(opcLoad, opcStore)
  def rs1 = bits(19,15)
  def rs2 = bits(24,20)
  def rd  = bits(11,7)
  def may_write_v0 = rd === 0.U && opcode =/= opcStore
  def funct3 = bits(14,12)
  def imm5 = bits(19,15)
  def imm5_sext = Cat(Fill(59, imm5(4)), imm5)
  def funct6 = bits(31,26)
  def writes_xrf = !vmu && ((funct3 === OPMVV && opmf6 === OPMFunct6.wrxunary0) || (funct3 === OPFVV && opff6 === OPFFunct6.wrfunary0))
  def writes_frf = !vmu && (funct3 === OPFVV)

  def isOpi = funct3.isOneOf(OPIVV, OPIVI, OPIVX)
  def isOpm = funct3.isOneOf(OPMVV, OPMVX)
  def isOpf = funct3.isOneOf(OPFVV, OPFVF)

  def opmf6 = Mux(isOpm, OPMFunct6(funct6), OPMFunct6.illegal)
  def opif6 = Mux(isOpi, OPIFunct6(funct6), OPIFunct6.illegal)
  def opff6 = Mux(isOpf, OPFFunct6(funct6), OPFFunct6.illegal)
}

class BackendIssueInst(implicit p: Parameters) extends VectorIssueInst()(p) {
  val reduction = Bool()      // accumulates into vd[0]
  val scalar_to_vd0 = Bool()  // mv scalar to vd[0]
  val wide_vd = Bool()        // vd reads/writes at 2xSEW
  val wide_vs2 = Bool()       // vs2 reads at 2xSEW
  val writes_mask = Bool()    // writes dest as a mask
  val reads_vs1_mask = Bool() // vs1 read as mask
  val reads_vs2_mask = Bool() // vs2 read as mask
  val rs1_is_rs2 = Bool()
  val nf_log2 = UInt(2.W)

  val renv1 = Bool()
  val renv2 = Bool()
  val renvd = Bool()
  val renvm = Bool()
  val wvd = Bool()
}

class IssueQueueInst(nSeqs: Int)(implicit p: Parameters) extends BackendIssueInst()(p) {
  val seq = UInt(nSeqs.W)
}

class VectorWrite(writeBits: Int)(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val eg = UInt(log2Ceil(32 * vLen / writeBits).W)
  def bankId = if (vrfBankBits == 0) 0.U else eg(vrfBankBits-1,0)
  val data = UInt(writeBits.W)
  val mask = UInt(writeBits.W)
}

class ScalarWrite extends Bundle {
  val data = UInt(64.W)
  val fp = Bool()
  val size = UInt(2.W)
  val rd = UInt(5.W)
}

class VectorReadReq(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val eg = UInt(log2Ceil(egsTotal).W)
  val oldest = Bool()
}

class VectorReadIO(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val req = Decoupled(new VectorReadReq)
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

class MaskedByte(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val debug_vat = UInt(vParams.vatSz.W)
  val data = UInt(8.W)
  val mask = Bool()
}

class ExecuteMicroOp(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val eidx = UInt(log2Ceil(maxVLMax).W)
  val vl = UInt((1+log2Ceil(maxVLMax)).W)

  val rvs1_data = UInt(dLen.W)
  val rvs2_data = UInt(dLen.W)
  val rvd_data  = UInt(dLen.W)
  val rvm_data  = UInt(dLen.W)

  val rvs1_elem = UInt(64.W)
  val rvs2_elem = UInt(64.W)
  val rvd_elem  = UInt(64.W)

  val rvs1_eew = UInt(2.W)
  val rvs2_eew = UInt(2.W)
  val rvd_eew = UInt(2.W)
  val vd_eew  = UInt(2.W)

  val rmask   = UInt(dLenB.W)
  val wmask   = UInt(dLenB.W)

  val full_tail_mask = UInt(dLen.W)

  val wvd_eg   = UInt(log2Ceil(egsTotal).W)

  val funct3 = UInt(3.W)
  def isOpi = funct3.isOneOf(OPIVV, OPIVI, OPIVX)
  def isOpm = funct3.isOneOf(OPMVV, OPMVX)
  def isOpf = funct3.isOneOf(OPFVV, OPFVF)

  def opmf6 = Mux(isOpm, OPMFunct6(funct6), OPMFunct6.illegal)
  def opif6 = Mux(isOpi, OPIFunct6(funct6), OPIFunct6.illegal)
  def opff6 = Mux(isOpf, OPFFunct6(funct6), OPFFunct6.illegal)

  def vd_eew8  = vd_eew === 0.U
  def vd_eew16 = vd_eew === 1.U
  def vd_eew32 = vd_eew === 2.U
  def vd_eew64 = vd_eew === 3.U

  val funct6 = UInt(6.W)
  val rs1 = UInt(5.W)
  val rs2 = UInt(5.W)
  val rd = UInt(5.W)
  val vm = Bool()

  val head = Bool()
  val tail = Bool()
  val vat = UInt(vParams.vatSz.W)
  val acc = Bool()

  val rm = UInt(3.W)
  def vxrm = rm(1,0)
  def frm = rm
}

class StoreDataMicroOp(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val stdata = UInt(dLen.W)
  val stmask = UInt(dLenB.W)
  val debug_vat = UInt(vParams.vatSz.W)
  def asMaskedBytes = {
    val bytes = Wire(Vec(dLenB, new MaskedByte))
    for (i <- 0 until dLenB) {
      bytes(i).data := stdata(((i+1)*8)-1,i*8)
      bytes(i).mask := stmask(i)
      bytes(i).debug_vat := debug_vat
    }
    bytes
  }
}

class LoadRespMicroOp(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val wvd_eg = UInt(log2Ceil(egsTotal).W)
  val wmask = UInt(dLenB.W)
  val tail = Bool()
  val vat = UInt(vParams.vatSz.W)
}

class PermuteMicroOp(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val rvs2_data = UInt(dLen.W)
  val eidx = UInt(log2Ceil(maxVLMax).W)
  val rvs2_eew = UInt(2.W)
  val rvm_data = UInt(dLen.W)
  val vmu = Bool()
  val vl = UInt((1+log2Ceil(maxVLMax)).W)
  val tail = Bool()
}

class PipeHazard(pipe_depth: Int)(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val vat = UInt(vParams.vatSz.W)
  val latency = UInt(log2Ceil(pipe_depth).W)  
  val eg = UInt(log2Ceil(egsTotal).W)
  def eg_oh = UIntToOH(eg)
}

class SequencerHazard(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val vat = UInt(vParams.vatSz.W)
  val rintent = UInt(egsTotal.W)
  val wintent = UInt(egsTotal.W)
}


class InstructionHazard(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val vat = UInt(vParams.vatSz.W)
  val rintent = UInt(32.W)
  val wintent = UInt(32.W)
}
