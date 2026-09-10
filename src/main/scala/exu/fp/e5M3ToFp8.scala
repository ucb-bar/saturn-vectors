package saturn.exu

import chisel3._
import freechips.rocketchip.tile._

object rawUnroundedToFp8 {

	def apply(unroundedType: FType, unroundedIn: hardfloat.RawFloat, unroundedInvalidExc: Bool, altfmt: Bool, roundingMode: Bits, saturate: Bool) = {
		val e5m2Narrower = Module(new hardfloat.RoundAnyRawFNToRecFN(unroundedType.exp, unroundedType.sig + 2, MXFType.E5M2.exp, MXFType.E5M2.sig, 0))
		e5m2Narrower.io.in := unroundedIn
		e5m2Narrower.io.roundingMode := roundingMode
		e5m2Narrower.io.detectTininess := hardfloat.consts.tininess_afterRounding
		e5m2Narrower.io.invalidExc := unroundedInvalidExc
		e5m2Narrower.io.infiniteExc := false.B

		val e5m3Narrower = Module(new hardfloat.RoundAnyRawFNToRecFN(unroundedType.exp, unroundedType.sig + 2, MXFType.E5M3.exp, MXFType.E5M3.sig, 0))
		e5m3Narrower.io.in := unroundedIn
		e5m3Narrower.io.roundingMode := roundingMode
		e5m3Narrower.io.detectTininess := hardfloat.consts.tininess_afterRounding
		e5m3Narrower.io.invalidExc := unroundedInvalidExc
		e5m3Narrower.io.infiniteExc := false.B

		val e4m3Narrower = Module(new hardfloat.RoundAnyRawFNToRecFN(unroundedType.exp, unroundedType.sig + 2, MXFType.E4M3.exp, MXFType.E4M3.sig, 0))
		e4m3Narrower.io.in := unroundedIn
		e4m3Narrower.io.roundingMode := roundingMode
		e4m3Narrower.io.detectTininess := hardfloat.consts.tininess_afterRounding
		e4m3Narrower.io.invalidExc := unroundedInvalidExc
		e4m3Narrower.io.infiniteExc := false.B

		val outBits = Wire(UInt(8.W))
		val exceptionFlags = Wire(UInt(5.W))
		outBits := DontCare
		exceptionFlags := DontCare
		
		val e5m2Ieee = MXFType.E5M2.ieee(e5m2Narrower.io.out)
		val e5m3Ieee = MXFType.E5M3.ieee(e5m3Narrower.io.out)
		val e4m3Ieee = MXFType.E4M3.ieee(e4m3Narrower.io.out)
		dontTouch(e5m3Ieee)
		dontTouch(e4m3Ieee)

		when (altfmt) { // E5M2
			outBits := saturateE5M2(e5m2Ieee, saturate)
			exceptionFlags := e5m2Narrower.io.exceptionFlags
		} .otherwise { // E4M3
			outBits := assembleOFPE4M3(e5m3Ieee, e4m3Ieee, saturate, roundingMode, e5m3Narrower.io.exceptionFlags(2))
			exceptionFlags := 0.U(5.W)
		}

		(outBits, exceptionFlags)
	}
}

object saturateE5M2 {

	def apply(in: UInt, saturate: Bool) = {
		val sign = in(7)
		val rest = in(6, 0)
		Mux(saturate && rest === "b1111100".U(7.W), // Saturate Inf
			sign ## "b1111011".U(7.W),
			Mux(rest(6, 2) === "b11111".U(5.W) && rest(1, 0) =/= "b00".U(2.W), // Use canonical NaN
				"h7F".U(8.W),
				in
			)
		)
	}
}

object assembleOFPE4M3 {
	
	def apply(ieeeE5M3: UInt, ieeeE4M3: UInt, saturate: Bool, roundingMode: Bits, e5m3Overflow: Bool) = {
		val sign = ieeeE4M3(7)
		val expE4M3 = ieeeE4M3(6, 3)
		val sigE4M3 = ieeeE4M3(2, 0)
		val expE5M3 = ieeeE5M3(7, 3)
		val sigE5M3 = ieeeE5M3(2, 0)
		// E5M3 is the same precision as E4M3 with a wider exponent, so it carries the
		// correctly-rounded result for the whole of OFP8 E4M3's range, including the top
		// binade that IEEE E4M3 has already collapsed to Inf/NaN.
		val isNaN = expE5M3 === "b11111".U(5.W) && sigE5M3 =/= "b000".U(3.W) // a real NaN, not a large finite
		val topBinade = expE5M3 === "b10111".U(5.W) // unbiased exp 8: OFP8 E4M3's top binade, [256, 512)
		val aboveTop = expE5M3(4, 3) === "b11".U(2.W) // unbiased exp >= 9, or Inf: past E4M3's range entirely
		// 1.111 x 2^8 = 480 is the NaN code point in OFP8 E4M3, so it overflows too.
		val overflows = aboveTop || (topBinade && sigE5M3 === "b111".U(3.W))
		
		val roundMagUp = (roundingMode === hardfloat.consts.round_min && sign) ||
			(roundingMode === hardfloat.consts.round_max && !sign)
		val overflowToSpecial = roundingMode === hardfloat.consts.round_near_even ||
			roundingMode === hardfloat.consts.round_near_maxMag || roundMagUp
		// That clause governs an out-of-range *finite* result. An operand that was
		// already infinite converts to infinity -- here NaN, E4M3 having none --
		// whatever the rounding mode. The E5M3 result is infinite in both cases, and
		// only the rounder's overflow flag separates them: hardfloat computes it as
		// `commonCase && common_overflow`, and commonCase excludes infinite operands.
		val e5m3IsInf = expE5M3 === "b11111".U(5.W) && sigE5M3 === "b000".U(3.W)
		val infiniteOperand = e5m3IsInf && !e5m3Overflow
		val clampByRounding = !infiniteOperand && !overflowToSpecial
		val outValue = Mux(isNaN,
			"h7F".U(8.W),
			Mux(overflows,
				Mux(saturate || clampByRounding, // clamp to maxFinite = 448
					sign ## "b1111110".U(7.W),
					"h7F".U(8.W) // otherwise NaN, as OFP8 E4M3 has no infinity
				),
				Mux(topBinade, // representable, but only the E5M3 rounder got it right
					sign ## "b1111".U(4.W) ## sigE5M3,
					ieeeE4M3
				)
			)
		)
		outValue
	}
}