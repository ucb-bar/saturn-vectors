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

  // omegaBlockDecode (5.4.1): Multiply(scale, element), so Inf x 0 is NaN.
  // in's exponent field must have room for the shift.
  def decode(in: RawFloat, scale: UInt): RawFloat = {
    val nanOut = in.isNaN || isNaN(scale) || (in.isInf && isZero(scale))
    val zeroOut = !nanOut && (in.isZero || isZero(scale))
    Mux(nanOut, makeNaN(in), Mux(zeroOut, makeZero(in), shiftExp(in, exponent(scale))))
  }

  // omegaBlockProject (5.4.2): Divide(element, scale), except that a zero
  // scale gives zero for every element, infinities included
  def project(in: RawFloat, scale: UInt): RawFloat = {
    val nanOut = in.isNaN || isNaN(scale)
    val zeroOut = !nanOut && isZero(scale)
    Mux(nanOut, makeNaN(in), Mux(zeroOut, makeZero(in), shiftExp(in, -exponent(scale))))
  }

  // Saturates instead of wrapping: past either end the following rounder
  // overflows or underflows the same way
  def shiftExp(in: RawFloat, delta: SInt): RawFloat = {
    val w = in.sExp.getWidth
    val wide = in.sExp +& delta
    val hi = ((BigInt(1) << (w - 1)) - 1).S(w.W)
    val lo = (-(BigInt(1) << (w - 1))).S(w.W)
    val out = WireInit(in)
    out.sExp := Mux(wide > hi, hi, Mux(wide < lo, lo, wide(w - 1, 0).asSInt))
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
