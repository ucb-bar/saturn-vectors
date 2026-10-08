package saturn.exu

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile._
import saturn.common._

// Rounds a hardfloat RawFloat to an 8-bit IEEE P3109 code, binary8p4 or
// binary8p3 as selected by altfmt. Section numbers refer to IEEE P3109
// Interim Report v4.0.3.
//
// The rounding core (adjustedSig .. common_inexact) is hardfloat's
// RoundAnyRawFNToRecFN with outSigWidth = 4 and the same signal names, except:
//  - the round mask is moved by delta to the selected format's subnormal
//    threshold, and widened by precShift, the significand bits it lacks;
//  - tininess is judged at the selected format's last kept bit;
//  - overflow is judged against the format's largest finite value, as P3109
//    has finite values where IEEE has its Inf/NaN exponent;
//  - round-to-odd overflows to Inf (NaN in the finite domain), 4.7.5.
// The output is assembled directly as a P3109 code: one NaN (0x80), no -0, and
// Inf at 0x7F/0xFF in the extended domain only.

object P3109Rounder {
  val outSigWidth = 4      // binary8p4's precision
  val expBias = 1 << 8     // hardfloat's sExp offset for an 8-bit exponent, so BF16 re-biases by zero

  // The mask decoder covers binary8p4's subnormal range; binary8p3 reaches it through delta
  val maskBottom = expBias + 1 - (1 << (7 - outSigWidth))   // binary8p4's minNorm
  val maskTop = maskBottom - outSigWidth - 1
  val maskExpWidth = log2Ceil(maskBottom + 1)
}

// Per-format constants, with exponents biased by P3109Rounder.expBias
case class P3109FormatInfo(precision: Int, finite: Boolean) {
  import P3109Rounder.{expBias, maskBottom, outSigWidth}

  val fracBits   = precision - 1
  val expBits    = 8 - precision
  val bias       = 1 << (7 - precision)
  val minNorm    = expBias + 1 - bias
  val minNonzero = minNorm - fracBits
  val emax       = expBias + ((1 << expBits) - 1 - bias)
  val maxFrac    = (1 << fracBits) - (if (finite) 1 else 2) // 0x7F is Inf in the extended domain
  val delta      = maskBottom - minNorm
  val precShift  = outSigWidth - precision

  require(precision >= 2 && precision <= outSigWidth)
}

// sigMSBitAlwaysZero: the significand is below 2, as for a value read from a
// register. An FMA result can reach 4.
class P3109Rounder(inExpWidth: Int, inSigWidth: Int, formats: P3109Formats,
                   sigMSBitAlwaysZero: Boolean) extends RawModule {
  import P3109Rounder._

  override def desiredName = s"P3109Rounder_ie${inExpWidth}_is${inSigWidth}"

  val io = IO(new Bundle {
    val in             = Input(new hardfloat.RawFloat(inExpWidth, inSigWidth))
    val altfmt         = Input(Bool()) // 0: binary8p4, 1: binary8p3
    val roundingMode   = Input(UInt(3.W))
    val sat            = Input(Bool()) // SatFinite instead of SatNone
    val invalidExc     = Input(Bool())
    val out            = Output(UInt(8.W))
    val exceptionFlags = Output(UInt(5.W))
  })

  val p4 = P3109FormatInfo(4, formats.p4Finite)
  val p3 = P3109FormatInfo(3, formats.p3Finite)

  val fracBits   = Mux(io.altfmt, p3.fracBits.U,   p4.fracBits.U)
  val precShift  = Mux(io.altfmt, p3.precShift.U,  p4.precShift.U)
  val delta      = Mux(io.altfmt, p3.delta.S,      p4.delta.S)
  val minNorm    = Mux(io.altfmt, p3.minNorm.S,    p4.minNorm.S)
  val minNonzero = Mux(io.altfmt, p3.minNonzero.S, p4.minNonzero.S)
  val emax       = Mux(io.altfmt, p3.emax.S,       p4.emax.S)
  val maxFrac    = Mux(io.altfmt, p3.maxFrac.U,    p4.maxFrac.U)
  val finite     = Mux(io.altfmt, p3.finite.B,     p4.finite.B)

  val roundingMode_near_even   = io.roundingMode === hardfloat.consts.round_near_even
  val roundingMode_near_maxMag = io.roundingMode === hardfloat.consts.round_near_maxMag
  val roundingMode_odd         = io.roundingMode === hardfloat.consts.round_odd
  val roundMagUp =
    (io.roundingMode === hardfloat.consts.round_min && io.in.sign) ||
    (io.roundingMode === hardfloat.consts.round_max && !io.in.sign)

  val sAdjustedExp = io.in.sExp +& (expBias - (1 << inExpWidth)).S

  // [top] [hidden] [outSigWidth-1 fraction] [guard] [sticky]
  val adjustedSig = if (inSigWidth <= outSigWidth + 2) {
    io.in.sig << (outSigWidth - inSigWidth + 2)
  } else {
    io.in.sig(inSigWidth, inSigWidth - outSigWidth - 1) ##
      io.in.sig(inSigWidth - outSigWidth - 2, 0).orR
  }

  val doShiftSigDown1 =
    if (sigMSBitAlwaysZero) false.B else adjustedSig(outSigWidth + 2)

  // Round mask: hardfloat's subnormal widening, moved by delta, then the bits
  // the format lacks. precShift is a shift, not an OR, so the two lengths add.
  val maskExp = sAdjustedExp + delta
  val maskExpClamped = Mux(maskExp < maskTop.S, maskTop.U,
                       Mux(maskExp > maskBottom.S, maskBottom.U,
                           maskExp.asUInt))
  val subnormalBits = hardfloat.lowMask(
    maskExpClamped(maskExpWidth - 1, 0), maskTop, maskBottom)
  val maskBody = (((subnormalBits | doShiftSigDown1) << precShift) |
                  ((1.U << precShift) - 1.U))(outSigWidth, 0)
  val roundMask = maskBody ## 3.U(2.W)

  val shiftedRoundMask = 0.U(1.W) ## (roundMask >> 1)
  val roundPosMask     = ~shiftedRoundMask & roundMask
  val roundPosBit      = (adjustedSig & roundPosMask).orR
  val anyRoundExtra    = (adjustedSig & shiftedRoundMask).orR
  val anyRound         = roundPosBit || anyRoundExtra

  val roundIncr =
    ((roundingMode_near_even || roundingMode_near_maxMag) && roundPosBit) || (roundMagUp && anyRound)

  val roundedSig = Mux(roundIncr,
    (((adjustedSig | roundMask) >> 2) +& 1.U) &
      ~Mux(roundingMode_near_even && roundPosBit && !anyRoundExtra, roundMask >> 1, 0.U((outSigWidth + 2).W)),
    (adjustedSig & ~roundMask) >> 2 |
      Mux(roundingMode_odd && anyRound, roundPosMask >> 1, 0.U))

  val sRoundedExp = sAdjustedExp +& (roundedSig >> outSigWidth).asUInt.zext

  // binary8p4's fraction field; binary8p3's low bit is always masked off
  val common_fractOut = Mux(doShiftSigDown1, roundedSig(outSigWidth - 1, 1), roundedSig(outSigWidth - 2, 0))
  val frac = common_fractOut >> precShift

  // Against the format's largest finite value, not IEEE's Inf exponent
  val common_overflow = (sRoundedExp > emax) ||
                        ((sRoundedExp === emax) && (frac > maxFrac))
  val common_totalUnderflow = sRoundedExp < minNonzero

  // Tininess after rounding, at the format's last kept bit (precShift higher
  // than hardfloat's position for binary8p3)
  val lsb = doShiftSigDown1.asUInt +& precShift
  val roundCarry = Mux(doShiftSigDown1, roundedSig(outSigWidth + 1), roundedSig(outSigWidth))
  val unboundedRange_roundPosBit = (adjustedSig >> (lsb +& 1.U))(0)
  val unboundedRange_anyRound = (adjustedSig & ((1.U << (lsb +& 2.U)) - 1.U)).orR
  val unboundedRange_roundIncr =
    ((roundingMode_near_even || roundingMode_near_maxMag) && unboundedRange_roundPosBit) ||
      (roundMagUp && unboundedRange_anyRound)
  // hardfloat also requires sAdjustedExp <= minNorm, because its mask exponent is
  // truncated and can alias; maskExpClamped cannot, so the mask bit implies it
  val common_underflow = common_totalUnderflow ||
    (anyRound && (roundMask >> (lsb +& 2.U))(0) &&
      !(!(roundMask >> (lsb +& 3.U))(0) &&
        roundCarry && roundPosBit && unboundedRange_roundIncr))

  val common_inexact = common_totalUnderflow || anyRound

  val isNaNOut   = io.invalidExc || io.in.isNaN
  val commonCase = !isNaNOut && !io.in.isInf && !io.in.isZero
  val overflow   = commonCase && common_overflow
  val underflow  = commonCase && common_underflow
  val inexact    = overflow || (commonCase && common_inexact)

  // Under SatNone only the directed modes towards zero stop at the largest
  // finite value; round-to-odd goes to Inf/NaN too (P3109 4.7.5)
  val overflow_roundMagUp =
    roundingMode_near_even || roundingMode_near_maxMag || roundMagUp || roundingMode_odd
  // Read only where commonCase && common_totalUnderflow holds (the output Mux)
  val pegMinNonzeroMagOut = roundMagUp || roundingMode_odd

  val signBit        = io.in.sign ## 0.U(7.W)
  val nanCode        = "h80".U(8.W)
  val maxFiniteCode  = signBit | "h7E".U | finite // 0x7F is Inf in the extended domain
  val infCode        = Mux(finite, nanCode, signBit | "h7F".U)

  // The mask already rounded a subnormal onto its grid, so this shift is exact.
  // Not total underflow, so the shift is at most fracBits and the field is nonzero.
  val subnormalShift = (minNorm - sRoundedExp)(1, 0)   // at most precision - 1
  val sigWithHidden  = ((1.U(7.W) << fracBits) | frac)(6, 0)
  val subnormalField = (sigWithHidden >> subnormalShift)(6, 0)

  val normalExpField = (sRoundedExp - minNorm + 1.S).asUInt
  val normalCode     = signBit | ((normalExpField << fracBits) | frac)(6, 0)

  io.out := Mux(isNaNOut, nanCode,
            Mux(io.in.isInf, Mux(io.sat, maxFiniteCode, infCode),
            Mux(io.in.isZero, 0.U,
            Mux(overflow, Mux(io.sat || !overflow_roundMagUp, maxFiniteCode, infCode),
            Mux(common_totalUnderflow, Mux(pegMinNonzeroMagOut, signBit | 1.U, 0.U),
            Mux(sRoundedExp < minNorm, signBit | subnormalField, normalCode))))))

  io.exceptionFlags := io.invalidExc ## false.B ## overflow ## underflow ## inexact
}

// The FMA's unrounded result to a P3109 code and flags (cf. rawUnroundedToFp8).
// RVV has no saturating multiply-add, so sat is off.
object rawUnroundedToP3109 {

  def apply(unroundedType: FType, unroundedIn: hardfloat.RawFloat, unroundedInvalidExc: Bool,
            altfmt: Bool, roundingMode: Bits, formats: P3109Formats): (UInt, UInt) = {
    val rounder = Module(new P3109Rounder(unroundedType.exp, unroundedType.sig + 2, formats,
                                          sigMSBitAlwaysZero = false))
    rounder.io.in := unroundedIn
    rounder.io.altfmt := altfmt
    rounder.io.roundingMode := roundingMode
    rounder.io.sat := false.B
    rounder.io.invalidExc := unroundedInvalidExc
    (rounder.io.out, rounder.io.exceptionFlags)
  }
}
