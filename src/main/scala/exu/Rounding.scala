package vector.exu

import chisel3._
import chisel3.util._

object RoundingIncrement {

  def apply(vxrm: UInt, v_d: Bool, v_d1: Bool, v_d20: Option[UInt]): Bool = MuxLookup(vxrm(1,0), false.B)(Seq(
    (0.U -> (v_d1)),
    (1.U -> (v_d1 && (v_d20.map(_ =/= 0.U).getOrElse(false.B) || v_d))),
    (2.U -> (false.B)),
    (3.U -> (!v_d && Cat(v_d1, v_d20.getOrElse(false.B)) =/= 0.U))
  ))

  def apply(vxrm: UInt, bits: UInt): UInt = {
    val w = bits.getWidth
    val d = w - 1
    require(w >= 2)
    apply(vxrm, bits(d), bits(d-1), if (w > 2) Some(bits(d-2,0)) else None)
  }
}
