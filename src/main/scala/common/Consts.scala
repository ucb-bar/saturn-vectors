package vector.common

import chisel3._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._

object OPIFunct6 extends ChiselEnum {
  val add = Value
  val _ = Value
  val sub, rsub, minu, min, maxu, max = Value
  val _ = Value
  val vand, vor, vxor, vrgather = Value
  val _ = Value
  val slideup, slidedown = Value
  def rgatherei16 = slideup

  val vadc = Value
  val _ = Value
  val madc, sbc, msbc = Value
  val _, _, _ = Value
  val merge, mseq, msne, msltu, mslt, msleu, msle, msgtu, msgt = Value
  val saddu, sadd, ssubu, ssub = Value
  val _ = Value
  val sll = Value
  val _ = Value
  val smul, srl, sra, srrl, srra, nsrl, nsra, nclipu, nclip = Value
  val wredsumu, wredsum = Value
}


trait VectorConsts {
  def mopUnit      = 0.U(2.W)
  def mopUnordered = 1.U(2.W)
  def mopStrided   = 2.U(2.W)
  def mopOrdered   = 3.U(2.W)

  def lumopUnit  = "b00000".U
  def lumopWhole = "b01000".U
  def lumopMask  = "b01011".U
  def lumopFF    = "b10000".U

  def sumopUnit  = "b00000".U
  def sumopWhole = "b01000".U
  def sumopMask  = "b01011".U

  def opcLoad   = "b0000111".U
  def opcStore  = "b0100111".U
  def opcVector = "b1010111".U

  def execRegular      = 1.U(3.W)
  def execElementOrder = 2.U(3.W)

  def OPIVV = 0.U
  def OPFVV = 1.U
  def OPMVV = 2.U
  def OPIVI = 3.U
  def OPIVX = 4.U
  def OPFVF = 5.U
  def OPMVX = 6.U
  def OPCFG = 7.U

}
