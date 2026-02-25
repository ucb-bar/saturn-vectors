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
			outBits := assembleOFPE4M3(e5m3Ieee, e4m3Ieee, saturate)
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
	
	def apply(ieeeE5M3: UInt, ieeeE4M3: UInt, saturate: Bool) = {
		val sign = ieeeE4M3(7)
		val expE4M3 = ieeeE4M3(6, 3)
		val sigE4M3 = ieeeE4M3(2, 0)
		val expE5M3 = ieeeE5M3(7, 3)
		val sigE5M3 = ieeeE5M3(2, 0)
		val special = expE5M3(4, 3) === "b11".U(2.W) // Values that overflow to infinity or NaN for OFP8 E4M3
		val overflow = expE5M3 === "b10111".U(5.W) // Values that overflow to infinity for IEEE E4M3 but not OFP8 E4M3
		val possibleInf = expE5M3(4, 3) === "b11".U(2.W) || sigE5M3 === "b111".U(3.W) // True inf check for values in the IEEE E4M3 inf range
		val outValue = Mux(special || overflow, // Possible NaN or Inf
			Mux(special && sigE5M3 =/= "b000".U(3.W), // NaN
				"h7F".U(8.W),
				Mux(possibleInf, // Inf
					Mux(saturate, // Saturate Inf
						sign ## "b1111110".U(7.W),
						"h7F".U(8.W)
					),
					sign ## "b1111".U(4.W) ## sigE5M3
				)
			),
			ieeeE4M3
		)
		outValue
	}
}