package saturn.exu

import chisel3._
import chisel3.util._
import hardfloat._
import hardfloat.consts._
import scala.math._

class RecFNToINDynamic(expWidth: Int, sigWidth: Int, intWidths: Seq[Int]) extends chisel3.Module
{
  val maxIntWidth = intWidths.max
  override def desiredName = s"RecFNToINDynamic_e${expWidth}_s${sigWidth}"
  val io = IO(new Bundle {
    val in = Input(Bits((expWidth + sigWidth + 1).W))
    val outW = Input(UInt(intWidths.size.W))
    val roundingMode = Input(UInt(3.W))
    val signedOut = Input(Bool())
    val out = Output(Bits(maxIntWidth.W))
    val intExceptionFlags = Output(Bits(3.W))
  })

  //------------------------------------------------------------------------
  //------------------------------------------------------------------------
  val rawIn = rawFloatFromRecFN(expWidth, sigWidth, io.in)

  val magGeOne = rawIn.sExp(expWidth)
  val posExp = rawIn.sExp(expWidth - 1, 0)
  val magJustBelowOne = !magGeOne && posExp.andR

  //------------------------------------------------------------------------
  //------------------------------------------------------------------------
  val roundingMode_near_even   = (io.roundingMode === round_near_even)
  val roundingMode_minMag      = (io.roundingMode === round_minMag)
  val roundingMode_min         = (io.roundingMode === round_min)
  val roundingMode_max         = (io.roundingMode === round_max)
  val roundingMode_near_maxMag = (io.roundingMode === round_near_maxMag)
  val roundingMode_odd         = (io.roundingMode === round_odd)

  /*------------------------------------------------------------------------
  | Assuming the input floating-point value is not a NaN, its magnitude is
  | at least 1, and it is not obviously so large as to lead to overflow,
  | convert its significand to fixed-point (i.e., with the binary point in a
    | fixed location).  For a non-NaN input with a magnitude less than 1, this
  | expression contrives to ensure that the integer bits of 'alignedSig'
  | will all be zeros.
    *------------------------------------------------------------------------*/
  val shiftedSig =
    (magGeOne ## rawIn.sig(sigWidth - 2, 0))<<
  Mux(magGeOne,
    Mux1H(io.outW, intWidths.map(intWidth => rawIn.sExp(min(expWidth - 2, log2Up(intWidth) - 1), 0))),
    0.U
  )
  val alignedSig =
    (shiftedSig>>(sigWidth - 2)) ## shiftedSig(sigWidth - 3, 0).orR
  val unroundedInts = intWidths.map(intWidth => 0.U(intWidth.W) | alignedSig>>2)

  val common_inexact = Mux(magGeOne, alignedSig(1, 0).orR, !rawIn.isZero)
  val roundIncr_near_even =
    (magGeOne       && (alignedSig(2, 1).andR || alignedSig(1, 0).andR)) ||
  (magJustBelowOne && alignedSig(1, 0).orR)
  val roundIncr_near_maxMag = (magGeOne && alignedSig(1)) || magJustBelowOne
  val roundIncr =
    (roundingMode_near_even   && roundIncr_near_even  ) ||
  (roundingMode_near_maxMag && roundIncr_near_maxMag) ||
  ((roundingMode_min || roundingMode_odd) &&
    (rawIn.sign && common_inexact)) ||
  (roundingMode_max && (!rawIn.sign && common_inexact))
  val complUnroundedInt = Mux(rawIn.sign,
    Mux1H(io.outW, unroundedInts.map(unroundedInt => ~unroundedInt)),
    Mux1H(io.outW, unroundedInts))
  val roundedInt =
    Mux(roundIncr ^ rawIn.sign,
      complUnroundedInt + 1.U,
      complUnroundedInt
    ) | (roundingMode_odd && common_inexact)

  val magGeOne_atOverflowEdge = Mux1H(io.outW, intWidths.map(intWidth => (posExp === (intWidth - 1).U)))
  //*** CHANGE TO TAKE BITS FROM THE ORIGINAL 'rawIn.sig' INSTEAD OF FROM
  //***  'unroundedInt'?:
  val roundCarryBut2 = Mux1H(io.outW,
    intWidths.zip(unroundedInts).map { case (intWidth, unroundedInt) => unroundedInt(intWidth - 3, 0).andR }
  ) && roundIncr
  val posExpEquals = Mux1H(io.outW, intWidths.map(intWidth => posExp === (intWidth - 2).U))
  val common_overflow =
    Mux(magGeOne,
      Mux1H(io.outW, intWidths.map(intWidth => (posExp >= intWidth.U))) ||
        Mux(io.signedOut,
          Mux(rawIn.sign,
            magGeOne_atOverflowEdge &&
              (Mux1H(io.outW, intWidths.zip(unroundedInts).map {
                case (intWidth, unroundedInt) => unroundedInt(intWidth - 2, 0)
              }).orR || roundIncr),
            magGeOne_atOverflowEdge ||
              (posExpEquals && roundCarryBut2)
          ),
          rawIn.sign ||
            (magGeOne_atOverflowEdge &&
              Mux1H(io.outW, intWidths.zip(unroundedInts).map {
                case (intWidth, unroundedInt) => unroundedInt(intWidth - 2)
              }) && roundCarryBut2)
        ),
      !io.signedOut && rawIn.sign && roundIncr
    )

  //------------------------------------------------------------------------
  //------------------------------------------------------------------------
  val invalidExc = rawIn.isNaN || rawIn.isInf
  val overflow = !invalidExc && common_overflow
  val inexact  = !invalidExc && !common_overflow && common_inexact

  val excSign = !rawIn.isNaN && rawIn.sign
  val excOut =
    Mux((io.signedOut === excSign),
      Mux1H(io.outW, intWidths.map(intWidth => (BigInt(1)<<(intWidth - 1)).U)),
      0.U
    ) |
  Mux(!excSign, Mux1H(io.outW, intWidths.map(intWidth => ((BigInt(1)<<(intWidth - 1)) - 1).U)), 0.U)

  io.out := Mux(invalidExc || common_overflow, excOut, roundedInt)
  io.intExceptionFlags := invalidExc ## overflow ## inexact
}

