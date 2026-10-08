package saturn.exu

import chisel3._
import hardfloat.RawFloat

// IEEE P3109 block scale factors (P3109 5). The scale format, Binary8p1uf, is
// unsigned with precision 1: code 0 is zero, 255 is NaN, and code c is
// 2^(c - 128). Applying or removing a scale therefore only moves the exponent.
object P3109Scale {
  val Bias = 128
  val NaNCode = 255

  def isNaN(scale: UInt)  = scale === NaNCode.U
  def isZero(scale: UInt) = scale === 0.U
  def exponent(scale: UInt): SInt = (0.U(1.W) ## scale).asSInt - Bias.S

  // omegaBlockDecode (5.4.1): Multiply(scale, element), so Inf x 0 is NaN
  def decode(in: RawFloat, scale: UInt): RawFloat = {
    val nanOut = in.isNaN || isNaN(scale) || (in.isInf && isZero(scale))
    val zeroOut = in.isZero || isZero(scale)
    Mux(nanOut, makeNaN(in), Mux(zeroOut, makeZero(in), shiftExp(in, exponent(scale))))
  }

  // omegaBlockProject (5.4.2): Divide(element, scale), except that a zero
  // scale gives zero for every element but NaN, infinities included
  def project(in: RawFloat, scale: UInt): RawFloat = {
    val nanOut = in.isNaN || isNaN(scale)
    val zeroOut = isZero(scale)
    Mux(nanOut, makeNaN(in), Mux(zeroOut, makeZero(in), shiftExp(in, -exponent(scale))))
  }

  // The callers pass BF16 raw floats, whose sExp field holds any element's
  // exponent moved by any scale (decode: 111..511, project: -4..511), so the
  // sum always fits. Scale codes 0 and 255 never reach here, nor do NaNs.
  def shiftExp(in: RawFloat, delta: SInt): RawFloat = {
    val out = WireInit(in)
    out.sExp := (in.sExp +& delta)(in.sExp.getWidth - 1, 0).asSInt
    out
  }

  def makeNaN(in: RawFloat): RawFloat = {
    val out = WireInit(in)
    out.isNaN := true.B
    out.isInf := false.B
    out.isZero := false.B
    out
  }

  def makeZero(in: RawFloat): RawFloat = {
    val out = WireInit(in)
    out.isNaN := false.B
    out.isInf := false.B
    out.isZero := true.B
    out
  }
}
