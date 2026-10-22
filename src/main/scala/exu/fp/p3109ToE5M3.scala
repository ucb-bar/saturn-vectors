package saturn.exu

import chisel3._
import chisel3.util._
import saturn.common._

// P3109 binary8p4 (altfmt = 0) or binary8p3 (altfmt = 1) to IEEE E5M3, which
// holds every value of both formats exactly
object p3109ToE5M3 {

  def apply(in: Bits, altfmt: Bool, formats: P3109Formats): UInt = {
    val sign = in(7)
    val isZero = in === "h00".U
    val isNaN = in === "h80".U
    val isInf = in(6, 0) === "h7F".U && Mux(altfmt, (!formats.p3Finite).B, (!formats.p4Finite).B)

    // binary8p4, bias 8
    val exp4 = in(6, 3)
    val sig4 = in(2, 0)
    val subnormShift4 = PriorityEncoder(Reverse(sig4))
    val p4 = Mux(exp4 === 0.U,
      sign ## (7.U(5.W) - subnormShift4) ## ((sig4 << 1.U) << subnormShift4)(2, 0), // Subnormal
      sign ## ((0.U(1.W) ## exp4) + 7.U) ## sig4) // Normal

    // binary8p3, bias 16: exponent fields 0 and 1 land on E5M3 subnormals
    val exp3 = in(6, 2)
    val sig3 = in(1, 0)
    val p3 = Mux(exp3(4, 1) === 0.U,
      sign ## 0.U(5.W) ## exp3(0) ## sig3,
      sign ## (exp3 - 1.U) ## sig3 ## 0.U(1.W))

    Mux(isNaN, "b0_11111_100".U(9.W),
      Mux(isInf, sign ## "b11111_000".U(8.W),
        Mux(isZero, 0.U(9.W),
          Mux(altfmt, p3, p4))))
  }
}
