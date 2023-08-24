package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

case class VectorParams(
  vdqEntries: Int = 4,
  vlaqEntries: Int = 4,
  vsaqEntries: Int = 4,
  vmaqEntries: Int = 4,
  vsoqEntries: Int = 4,
  vatSz: Int = 3)

case object VectorParamsKey extends Field[VectorParams]

trait HasVectorParams extends VectorConsts { this: HasCoreParameters =>
  implicit val p: Parameters
  def vParams: VectorParams = p(VectorParamsKey)
  def dLen = vMemDataBits
  def dLenB = dLen / 8
  def dLenOffBits = log2Ceil(dLenB)

  def vmaqSz = log2Ceil(vParams.vmaqEntries)

  def egsPerVReg = vLen / dLen
  def egsTotal = (vLen / dLen) * 32

  def getEgId(vreg: UInt, eidx: UInt, eew: UInt): UInt = {
    val base = vreg << log2Ceil(egsPerVReg)
    val off = eidx >> (log2Ceil(dLenB).U - eew)
    base + off
  }

  def cqOlder(i0: UInt, i1: UInt, tail: UInt) = (i0 < i1) ^ (i0 < tail) ^ (i1 < tail)
}
