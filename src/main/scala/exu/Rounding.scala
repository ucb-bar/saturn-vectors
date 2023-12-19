package vector.exu

import chisel3._
import chisel3.util._

object RoundingIncrement {
  def apply(vxrm: UInt, bits: UInt): UInt = {
    val rm = vxrm(1,0)
    val w = bits.getWidth
    require(w >= 2)
    val d = w - 1
    val out = Wire(UInt(1.W))
    when (rm === 0.U) {        // rnu
      out := bits(d-1)
    } .elsewhen (rm === 1.U) { // rne
      out := bits(d-1) && ((if (w == 2) false.B else (bits(d-2,0) =/= 0.U)) || bits(d))
    } .elsewhen (rm === 2.U) { // rdn
      out := 0.U
    } .otherwise { // rod
      out := !bits(d) && (bits(d-1,0) =/= 0.U)
    }
    out
  }
}
