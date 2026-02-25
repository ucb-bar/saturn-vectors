package saturn.exu

import chisel3._
import freechips.rocketchip.tile._

object MXFType {
  val BF16 = new FType(8, 8)
  val E5M3 = new FType(5, 4)
  val E4M3 = new FType(4, 4)
  val E5M2 = new FType(5, 3)
}
