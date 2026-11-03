package saturn.exu

import chisel3._
import chisel3.util._

object fp8ToE5M3 {

	def apply(in: Bits, altfmt: Bool) = {
		Mux(altfmt, { // E5M2
			in ## 0.U(1.W)
		}, { // E4M3
			val sign = in(7)
			val exp = in(6, 3)
			val sig = in(2, 0)
			val isNaN = exp.andR && sig.andR
			val isSubnormal = !exp.orR
			val subnormShift = PriorityEncoder(Reverse(sig))
			val subnormSig = ((sig << 1.U) << subnormShift)(2, 0)
			val subnormExp = 8.U(5.W) - subnormShift
			Mux(isNaN,
				sign ## "b11111100".U(8.W), // NaN
				Mux(isSubnormal,
					Mux(sig.orR,
						sign ## subnormExp ## subnormSig, // Subnormal
						sign ## "b00000000".U(8.W) // Zero
					),
					sign ## ((0.U(1.W) ## exp) + 8.U) ## sig // Normal
				)
			)
		})
	}
}