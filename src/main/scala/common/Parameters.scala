package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

case class VectorParams(
  vdqEntries: Int = 4,

  vliqEntries: Int = 4,
  vsiqEntries: Int = 4,

  vlifqEntries: Int = 4,
  vsifqEntries: Int = 4,

  dLen: Int = 64,
  vatSz: Int = 3)

case object VectorParamsKey extends Field[VectorParams]

trait HasVectorParams extends VectorConsts { this: HasCoreParameters =>
  implicit val p: Parameters
  def vParams: VectorParams = p(VectorParamsKey)
  def dLen = vParams.dLen
  def dLenB = dLen / 8
  def dLenOffBits = log2Ceil(dLenB)

  def egsPerVReg = vLen / dLen
  def egsTotal = (vLen / dLen) * 32

  def getEgId(vreg: UInt, eidx: UInt, eew: UInt): UInt = {
    val base = vreg << log2Ceil(egsPerVReg)
    val off = eidx >> (log2Ceil(dLenB).U - eew)
    base + off
  }
  def getByteId(vreg: UInt, eidx: UInt, eew: UInt): UInt = {
    Cat(getEgId(vreg, eidx, eew), (eidx << eew)(log2Ceil(dLenB)-1,0))
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
}
