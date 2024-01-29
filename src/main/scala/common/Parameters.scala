package saturn.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

object VectorParams {
  def minParams = VectorParams(useScalarFPUFMAPipe = true)
  def refParams = VectorParams(vlissqEntries = 3, vsissqEntries = 3, vxissqEntries = 3, vatSz = 5, useSegmentedIMul = true, doubleBufferSegments = true)

}

case class VectorParams(
  // In-order dispatch Queue
  vdqEntries: Int = 4,

  // Load store instruction queues (in VLSU)
  vliqEntries: Int = 4,
  vsiqEntries: Int = 4,

  // Load store in-flight queues (in VLSU)
  vlifqEntries: Int = 8,
  vsifqEntries: Int = 8,

  // Load/store/execute/permute/maskindex issue queues
  vlissqEntries: Int = 0,
  vsissqEntries: Int = 0,
  vxissqEntries: Int = 0,
  vpissqEntries: Int = 0,

  dLen: Int = 64,
  vatSz: Int = 3,

  useSegmentedIMul: Boolean = false,
  useScalarFPUFMAPipe: Boolean = false,
  fmaPipeDepth: Int = 3,

  doubleBufferSegments: Boolean = false,
)

case object VectorParamsKey extends Field[VectorParams]

trait HasVectorParams extends HasVectorConsts { this: HasCoreParameters =>
  implicit val p: Parameters
  def vParams: VectorParams = p(VectorParamsKey)
  def dLen = vParams.dLen
  def dLenB = dLen / 8
  def dLenOffBits = log2Ceil(dLenB)
  def dmemTagBits = log2Ceil(vParams.vlifqEntries.max(vParams.vsifqEntries))
  def egsPerVReg = vLen / dLen
  def egsTotal = (vLen / dLen) * 32

  def getEgId(vreg: UInt, eidx: UInt, eew: UInt, bitwise: Bool): UInt = {
    val base = vreg << log2Ceil(egsPerVReg)
    val off = eidx >> Mux(bitwise, log2Ceil(dLen).U, (log2Ceil(dLenB).U - eew))
    base +& off
  }
  def getByteId(vreg: UInt, eidx: UInt, eew: UInt): UInt = {
    Cat(getEgId(vreg, eidx, eew, false.B), (eidx << eew)(log2Ceil(dLenB)-1,0))
  }

  def eewByteMask(eew: UInt) = (0 until (1+log2Ceil(eLen/8))).map { e =>
    Mux(e.U === eew, ((1 << (1 << e)) - 1).U, 0.U)
  }.reduce(_|_)((eLen/8)-1,0)
  def eewBitMask(eew: UInt) = FillInterleaved(8, eewByteMask(eew))


  def cqOlder(i0: UInt, i1: UInt, tail: UInt) = (i0 < i1) ^ (i0 < tail) ^ (i1 < tail)
  def dLenSplat(in: UInt, eew: UInt) = {
    val v = Wire(UInt(64.W))
    v := in
    Mux1H(UIntToOH(eew), (0 until 4).map { i => Fill(dLenB >> i, v((8<<i)-1,0)) })
  }

  def maxPosUInt(sew: Int) = Cat(0.U, ~(0.U(((8 << sew)-1).W)))
  def minNegUInt(sew: Int) = Cat(1.U,   0.U(((8 << sew)-1).W))
  def maxPosSInt(sew: Int) = ((1 << ((8 << sew)-1))-1).S
  def minNegSInt(sew: Int) = (-1 << ((8 << sew)-1)).S
  def maxPosFPUInt(sew: Int) = {
    val expBits = Seq(4, 5, 8, 11)(sew)
    val fracBits = (8 << sew) - expBits - 1
    Cat(0.U, ~(0.U(expBits.W)), 0.U(fracBits.W))
  }
  def minNegFPUInt(sew: Int) = {
    val expBits = Seq(4, 5, 8, 11)(sew)
    val fracBits = (8 << sew) - expBits - 1
    Cat(1.U, ~(0.U(expBits.W)), 0.U(fracBits.W))
  }
  def get_arch_mask(reg: UInt, emul: UInt) = VecInit.tabulate(4)({ lmul =>
    FillInterleaved(1 << lmul, UIntToOH(reg >> lmul)((32>>lmul)-1,0))
  })(emul)
  def log2_up(f: UInt, max: Int) = VecInit.tabulate(max)({nf => log2Ceil(nf+1).U})(f)
}
