package vector.common

import chisel3._
import chisel3.util._
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

object OPMFunct6 extends ChiselEnum {
  val redsum, redand, redor, redxor, redminu, redmin, redmaxu, redmax, aaddu, aadd, asubu, asub = Value
  val _, _ = Value
  val slide1up, slide1down = Value

  val wrxunary0 = Value
  val _ = Value
  val xunary0 = Value
  val _ = Value
  val munary0 = Value
  val _, _ = Value
  val compress, mandnot, mand, mor, mxor, mornot, mnand, mnor, mxnor = Value

  val divu, div, remu, rem, mulhu, mul, mulhsu, mulh = Value
  val _ = Value
  val madd = Value
  val _ = Value
  val nmsub = Value
  val _ = Value
  val macc = Value
  val _ = Value
  val nmsac = Value

  val waddu, wadd, wsubu, wsub, wadduw, waddw, wsubuw, wsubw, wmulu = Value
  val _ = Value
  val wmulsu, wmul, wmaccu, wmacc, wmaccus, wmaccsu = Value
}

object OPFFunct6 extends ChiselEnum {

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

  def OPIVV = 0.U
  def OPFVV = 1.U
  def OPMVV = 2.U
  def OPIVI = 3.U
  def OPIVX = 4.U
  def OPFVF = 5.U
  def OPMVX = 6.U
  def OPCFG = 7.U
}

object VecDecode extends VectorConsts {
  def apply(funct3: UInt, funct6: UInt,
    trues: Seq[EnumType],
    falses: Seq[EnumType]): Bool = {
    def vToUInt(vs: Seq[EnumType]) = vs.map {
      case v: OPIFunct6.Type => Seq(OPIVV, OPIVI, OPIVX).map { f3 => ((f3.litValue << 6) + v.litValue).U(9.W) }
      case v: OPMFunct6.Type => Seq(OPMVV, OPMVX       ).map { f3 => ((f3.litValue << 6) + v.litValue).U(9.W) }
      case v: OPFFunct6.Type => Seq(OPFVV, OPFVF       ).map { f3 => ((f3.litValue << 6) + v.litValue).U(9.W) }
    }.flatten
    DecodeLogic(Cat(funct3(2,0), funct6(5,0)),
      vToUInt(trues),
      vToUInt(falses))
  }
  def apply(inst: VectorIssueInst, trues: Seq[EnumType], falses: Seq[EnumType]): Bool = apply(
    inst.funct3, inst.funct6, trues, falses)
}

