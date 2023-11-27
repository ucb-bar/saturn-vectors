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
  val and, or, xor, rgather = Value
  val _ = Value
  val slideup, slidedown = Value
  def rgatherei16 = slideup

  val adc = Value
  val madc, sbc, msbc = Value
  val _, _, _ = Value
  val merge, mseq, msne, msltu, mslt, msleu, msle, msgtu, msgt = Value

  val saddu, sadd, ssubu, ssub = Value
  val _ = Value
  val sll = Value
  val _ = Value
  val smul, srl, sra, srrl, srra, nsrl, nsra, nclipu, nclip = Value
  val wredsumu, wredsum = Value

  val illegal = Value
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

  val illegal = Value
}

object OPFFunct6 extends ChiselEnum {
  val vfadd, vfredusum, vfsub, vfredosum, vfmin, vfredmin, vfmax, vfredmax, vfsgnj, vfsgnjn, vfsgnjx = Value
  val _, _, _ = Value
  val vfslide1up, vfslide1down = Value
  val _, _, _, _, _, _, _ = Value
  val vfmerge, vmfeq, vfmfle = Value
  val _ = Value
  val vmflt, vmfne, vmfgt = Value
  val _ = Value
  val vmfge, vfdiv, vfrdiv = Value
  val _, _ = Value
  val vfmul = Value
  val _, _ = Value
  val vfrsub = Value
  val vfmadd, vfnmadd, vfmsub, vfnmsub, vfmacc, vfnmacc, vfmsac, vfnmsac, vfwadd, vwfredusum, vfwsub, vfwredosum = Value
  val illegal = Value
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

  def X = BitPat("b?")
  def N = BitPat("b0")
  def Y = BitPat("b1")

}

object VecDecode extends VectorConsts {
  def apply(funct3: UInt, funct6: UInt, default: Seq[BitPat], table: Seq[(EnumType, Seq[BitPat])]): Seq[UInt] = {
    def enumToUInt(e: EnumType): Seq[UInt] = e match {
      case v: OPIFunct6.Type => Seq(OPIVV, OPIVI, OPIVX).map { f3 => ((f3.litValue << 6) + v.litValue).U(9.W) }
      case v: OPMFunct6.Type => Seq(OPMVV, OPMVX       ).map { f3 => ((f3.litValue << 6) + v.litValue).U(9.W) }
      case v: OPFFunct6.Type => Seq(OPFVV, OPFVF       ).map { f3 => ((f3.litValue << 6) + v.litValue).U(9.W) }
    }
    DecodeLogic(Cat(funct3(2,0), funct6(5,0)),
      default,
      table.map(e => enumToUInt(e._1).map(u => (BitPat(u), e._2))).flatten)
  }


  def applyBools(funct3: UInt, funct6: UInt, default: Seq[BitPat], table: Seq[(EnumType, Seq[BitPat])]): Seq[Bool] = apply(
    funct3, funct6, default, table).map(_(0))

  def apply(funct3: UInt, funct6: UInt, trues: Seq[EnumType], falses: Seq[EnumType]): Bool = applyBools(
    funct3, funct6, Seq(BitPat.dontCare(1)), trues.map(e => (e, Seq(BitPat(true.B)))) ++ falses.map(e => (e, Seq(BitPat(false.B)))))(0)
  def apply(funct3: UInt, funct6: UInt, trues: Seq[EnumType]): Bool = applyBools(
    funct3, funct6, Seq(BitPat(false.B)), trues.map(e => (e, Seq(BitPat(true.B)))))(0)
}

