package saturn.insns

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.rocket._
import freechips.rocketchip.rocket.constants._
import freechips.rocketchip.util._
import saturn.common.{OPIFunct6, OPMFunct6, OPFFunct6, VectorConsts}

class OPIVVInstruction(base: OPIInstruction) extends VectorInstruction {
  val props = base.props ++ Seq(F3(VectorConsts.OPIVV), ReadsVS1.Y)
}
class OPIVXInstruction(base: OPIInstruction) extends VectorInstruction {
  val props = base.props ++ Seq(F3(VectorConsts.OPIVX))
}
class OPIVIInstruction(base: OPIInstruction) extends VectorInstruction {
  val props = base.props ++ Seq(F3(VectorConsts.OPIVI))
}
class OPMVVInstruction(base: OPMInstruction) extends VectorInstruction {
  val props = base.props ++ Seq(F3(VectorConsts.OPMVV), ReadsVS1.Y)
}
class OPMVXInstruction(base: OPMInstruction) extends VectorInstruction {
  val props = base.props ++ Seq(F3(VectorConsts.OPMVX))
}
class OPFVVInstruction(base: OPFInstruction) extends VectorInstruction {
  val props = base.props ++ Seq(F3(VectorConsts.OPFVV), ReadsVS1.Y)
}
class OPFVFInstruction(base: OPFInstruction) extends VectorInstruction {
  val props = base.props ++ Seq(F3(VectorConsts.OPFVF))
}

trait OPIInstruction extends VectorInstruction {
  def VV = new OPIVVInstruction(this)
  def VX = new OPIVXInstruction(this)
  def VI = new OPIVIInstruction(this)
}

trait OPMInstruction extends VectorInstruction {
  def VV = new OPMVVInstruction(this)
  def VX = new OPMVXInstruction(this)
}

trait OPFInstruction extends VectorInstruction {
  def VV = new OPFVVInstruction(this)
  def VF = new OPFVFInstruction(this)
}

object ADD       extends OPIInstruction    { val props = Seq(F6(OPIFunct6.add)     , DoSub.N, Averaging.N, CarryIn.N) }
object SUB       extends OPIInstruction    { val props = Seq(F6(OPIFunct6.sub)     , DoSub.Y, Averaging.N, CarryIn.N) }
object RSUB      extends OPIInstruction    { val props = Seq(F6(OPIFunct6.rsub)    , DoSub.Y, Averaging.N, CarryIn.N, Swap12.Y) }
object WADDU     extends OPMInstruction    { val props = Seq(F6(OPMFunct6.waddu)   , DoSub.N, Averaging.N, CarryIn.N, WideningSext.N, Wide2VD.Y) }
object WADD      extends OPMInstruction    { val props = Seq(F6(OPMFunct6.wadd)    , DoSub.N, Averaging.N, CarryIn.N, WideningSext.Y, Wide2VD.Y) }
object WSUBU     extends OPMInstruction    { val props = Seq(F6(OPMFunct6.wsubu)   , DoSub.Y, Averaging.N, CarryIn.N, WideningSext.N, Wide2VD.Y) }
object WSUB      extends OPMInstruction    { val props = Seq(F6(OPMFunct6.wsub)    , DoSub.Y, Averaging.N, CarryIn.N, WideningSext.Y, Wide2VD.Y) }
object WADDUW    extends OPMInstruction    { val props = Seq(F6(OPMFunct6.wadduw)  , DoSub.N, Averaging.N, CarryIn.N, WideningSext.N, Wide2VD.Y, Wide2VS2.Y) }
object WADDW     extends OPMInstruction    { val props = Seq(F6(OPMFunct6.waddw)   , DoSub.N, Averaging.N, CarryIn.N, WideningSext.Y, Wide2VD.Y, Wide2VS2.Y) }
object WSUBUW    extends OPMInstruction    { val props = Seq(F6(OPMFunct6.wsubuw)  , DoSub.Y, Averaging.N, CarryIn.N, WideningSext.N, Wide2VD.Y, Wide2VS2.Y) }
object WSUBW     extends OPMInstruction    { val props = Seq(F6(OPMFunct6.wsubw)   , DoSub.Y, Averaging.N, CarryIn.N, WideningSext.Y, Wide2VD.Y, Wide2VS2.Y) }
object ADC       extends OPIInstruction    { val props = Seq(F6(OPIFunct6.adc)     , DoSub.N, Averaging.N, CarryIn.Y, AlwaysCarryIn.Y, SetsWMask.N) }
object MADC      extends OPIInstruction    { val props = Seq(F6(OPIFunct6.madc)    , DoSub.N, Averaging.N, CarryIn.Y, AlwaysCarryIn.N, SetsWMask.N, WritesAsMask.Y) }
object SBC       extends OPIInstruction    { val props = Seq(F6(OPIFunct6.sbc)     , DoSub.Y, Averaging.N, CarryIn.Y, AlwaysCarryIn.Y, SetsWMask.N) }
object MSBC      extends OPIInstruction    { val props = Seq(F6(OPIFunct6.msbc)    , DoSub.Y, Averaging.N, CarryIn.Y, AlwaysCarryIn.N, SetsWMask.N, WritesAsMask.Y) }
object NEXT      extends OPMInstruction    { val props = Seq(F6(OPMFunct6.xunary0) , RS1(BitPat("b00???")), UsesNarrowingSext.Y) }
object SLL       extends OPIInstruction    { val props = Seq(F6(OPIFunct6.sll)     , UsesShift.Y, ShiftsLeft.Y, ScalingShift.N) }
object SRA       extends OPIInstruction    { val props = Seq(F6(OPIFunct6.sra)     , UsesShift.Y, ShiftsLeft.N, ScalingShift.N) }
object SRL       extends OPIInstruction    { val props = Seq(F6(OPIFunct6.srl)     , UsesShift.Y, ShiftsLeft.N, ScalingShift.N, ZextImm5.Y) }
object NSRA      extends OPIInstruction    { val props = Seq(F6(OPIFunct6.nsra)    , UsesShift.Y, ShiftsLeft.N, ScalingShift.N, WideningSext.N, Wide2VS2.Y, ZextImm5.Y) }
object NSRL      extends OPIInstruction    { val props = Seq(F6(OPIFunct6.nsrl)    , UsesShift.Y, ShiftsLeft.N, ScalingShift.N, WideningSext.N, Wide2VS2.Y, ZextImm5.Y) }
object MSEQ      extends OPIInstruction    { val props = Seq(F6(OPIFunct6.mseq)    , WritesAsMask.Y, UsesCmp.Y, CmpLess.N) }
object MSNE      extends OPIInstruction    { val props = Seq(F6(OPIFunct6.msne)    , WritesAsMask.Y, UsesCmp.Y, CmpLess.N) }
object MSLTU     extends OPIInstruction    { val props = Seq(F6(OPIFunct6.msltu)   , WritesAsMask.Y, UsesCmp.Y, CmpLess.Y) }
object MSLT      extends OPIInstruction    { val props = Seq(F6(OPIFunct6.mslt)    , WritesAsMask.Y, UsesCmp.Y, CmpLess.Y) }
object MSLEU     extends OPIInstruction    { val props = Seq(F6(OPIFunct6.msleu)   , WritesAsMask.Y, UsesCmp.Y, CmpLess.Y) }
object MSLE      extends OPIInstruction    { val props = Seq(F6(OPIFunct6.msle)    , WritesAsMask.Y, UsesCmp.Y, CmpLess.Y) }
object MSGTU     extends OPIInstruction    { val props = Seq(F6(OPIFunct6.msgtu)   , WritesAsMask.Y, UsesCmp.Y, CmpLess.Y, Swap12.Y) }
object MSGT      extends OPIInstruction    { val props = Seq(F6(OPIFunct6.msgt)    , WritesAsMask.Y, UsesCmp.Y, CmpLess.Y, Swap12.Y) }
object MINU      extends OPIInstruction    { val props = Seq(F6(OPIFunct6.minu)    , UsesMinMax.Y, CmpLess.Y) }
object MIN       extends OPIInstruction    { val props = Seq(F6(OPIFunct6.min)     , UsesMinMax.Y, CmpLess.Y) }
object MAXU      extends OPIInstruction    { val props = Seq(F6(OPIFunct6.maxu)    , UsesMinMax.Y, CmpLess.Y, Swap12.Y) }
object MAX       extends OPIInstruction    { val props = Seq(F6(OPIFunct6.max)     , UsesMinMax.Y, CmpLess.Y, Swap12.Y) }
object MERGE     extends OPIInstruction    { val props = Seq(F6(OPIFunct6.merge)   , AlwaysReadsVM.Y, UsesMerge.Y, SetsWMask.N) }
object SADDU     extends OPIInstruction    { val props = Seq(F6(OPIFunct6.saddu)   , DoSub.N, Averaging.N, CarryIn.N, UsesSat.Y) }
object SADD      extends OPIInstruction    { val props = Seq(F6(OPIFunct6.sadd)    , DoSub.N, Averaging.N, CarryIn.N, UsesSat.Y) }
object SSUBU     extends OPIInstruction    { val props = Seq(F6(OPIFunct6.ssubu)   , DoSub.Y, Averaging.N, CarryIn.N, UsesSat.Y) }
object SSUB      extends OPIInstruction    { val props = Seq(F6(OPIFunct6.ssub)    , DoSub.Y, Averaging.N, CarryIn.N, UsesSat.Y) }
object AADDU     extends OPMInstruction    { val props = Seq(F6(OPMFunct6.aaddu)   , DoSub.N, Averaging.Y, CarryIn.N) }
object AADD      extends OPMInstruction    { val props = Seq(F6(OPMFunct6.aadd)    , DoSub.N, Averaging.Y, CarryIn.N) }
object ASUBU     extends OPMInstruction    { val props = Seq(F6(OPMFunct6.asubu)   , DoSub.Y, Averaging.Y, CarryIn.N) }
object ASUB      extends OPMInstruction    { val props = Seq(F6(OPMFunct6.asub)    , DoSub.Y, Averaging.Y, CarryIn.N) }
object SSRL      extends OPIInstruction    { val props = Seq(F6(OPIFunct6.ssrl)    , UsesShift.Y, ShiftsLeft.N, ScalingShift.Y, ZextImm5.Y) }
object SSRA      extends OPIInstruction    { val props = Seq(F6(OPIFunct6.ssra)    , UsesShift.Y, ShiftsLeft.N, ScalingShift.Y) }
object NCLIPU    extends OPIInstruction    { val props = Seq(F6(OPIFunct6.nclipu)  , UsesShift.Y, ShiftsLeft.N, ScalingShift.Y, Wide2VS2.Y, ZextImm5.Y) }
object NCLIP     extends OPIInstruction    { val props = Seq(F6(OPIFunct6.nclip)   , UsesShift.Y, ShiftsLeft.N, ScalingShift.Y, Wide2VS2.Y, ZextImm5.Y) }
object REDSUM    extends OPMInstruction    { val props = Seq(F6(OPMFunct6.redsum)  , Reduction.Y, AccInitZeros.Y, DoSub.N, Averaging.N, CarryIn.N) }
object WREDSUM   extends OPIInstruction    { val props = Seq(F6(OPIFunct6.wredsum) , Reduction.Y, AccInitZeros.Y, DoSub.N, Averaging.N, CarryIn.N, WideningSext.Y, Wide2VD.Y) }
object WREDSUMU  extends OPIInstruction    { val props = Seq(F6(OPIFunct6.wredsumu), Reduction.Y, AccInitZeros.Y, DoSub.N, Averaging.N, CarryIn.N, WideningSext.N, Wide2VD.Y) }
object REDMINU   extends OPMInstruction    { val props = Seq(F6(OPMFunct6.redminu) , Reduction.Y, AccInitOnes.Y , UsesMinMax.Y, CmpLess.Y) }
object REDMIN    extends OPMInstruction    { val props = Seq(F6(OPMFunct6.redmin)  , Reduction.Y, AccInitPos.Y  , UsesMinMax.Y, CmpLess.Y) }
object REDMAXU   extends OPMInstruction    { val props = Seq(F6(OPMFunct6.redmaxu) , Reduction.Y, AccInitZeros.Y, UsesMinMax.Y, CmpLess.Y, Swap12.Y) }
object REDMAX    extends OPMInstruction    { val props = Seq(F6(OPMFunct6.redmax)  , Reduction.Y, AccInitNeg.Y  , UsesMinMax.Y, CmpLess.Y, Swap12.Y) }
object FMERGE    extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fmerge)  , AlwaysReadsVM.Y, UsesMerge.Y, SetsWMask.N) }


object AND       extends OPIInstruction    { val props = Seq(F6(OPIFunct6.and)     , BWAnd.Y) }
object OR        extends OPIInstruction    { val props = Seq(F6(OPIFunct6.or)      , BWOr.Y) }
object XOR       extends OPIInstruction    { val props = Seq(F6(OPIFunct6.xor)     , BWXor.Y) }
object MANDNOT   extends OPMInstruction    { val props = Seq(F6(OPMFunct6.mandnot) , WritesAsMask.Y, ReadsVS1AsMask.Y, ReadsVS2AsMask.Y, BWAnd.Y, BWInv1.Y) }
object MAND      extends OPMInstruction    { val props = Seq(F6(OPMFunct6.mand)    , WritesAsMask.Y, ReadsVS1AsMask.Y, ReadsVS2AsMask.Y, BWAnd.Y) }
object MOR       extends OPMInstruction    { val props = Seq(F6(OPMFunct6.mor)     , WritesAsMask.Y, ReadsVS1AsMask.Y, ReadsVS2AsMask.Y, BWOr.Y) }
object MXOR      extends OPMInstruction    { val props = Seq(F6(OPMFunct6.mxor)    , WritesAsMask.Y, ReadsVS1AsMask.Y, ReadsVS2AsMask.Y, BWXor.Y) }
object MORNOT    extends OPMInstruction    { val props = Seq(F6(OPMFunct6.mornot)  , WritesAsMask.Y, ReadsVS1AsMask.Y, ReadsVS2AsMask.Y, BWOr.Y, BWInv1.Y) }
object MNAND     extends OPMInstruction    { val props = Seq(F6(OPMFunct6.mnand)   , WritesAsMask.Y, ReadsVS1AsMask.Y, ReadsVS2AsMask.Y, BWAnd.Y, BWInvOut.Y) }
object MNOR      extends OPMInstruction    { val props = Seq(F6(OPMFunct6.mnor)    , WritesAsMask.Y, ReadsVS1AsMask.Y, ReadsVS2AsMask.Y, BWOr.Y, BWInvOut.Y) }
object MXNOR     extends OPMInstruction    { val props = Seq(F6(OPMFunct6.mxnor)   , WritesAsMask.Y, ReadsVS1AsMask.Y, ReadsVS2AsMask.Y, BWXor.Y, BWInvOut.Y) }
object REDAND    extends OPMInstruction    { val props = Seq(F6(OPMFunct6.redand)  , Reduction.Y, AccInitOnes.Y , BWAnd.Y) }
object REDOR     extends OPMInstruction    { val props = Seq(F6(OPMFunct6.redor)   , Reduction.Y, AccInitZeros.Y, BWOr.Y) }
object REDXOR    extends OPMInstruction    { val props = Seq(F6(OPMFunct6.redxor)  , Reduction.Y, AccInitZeros.Y, BWXor.Y) }

object MUL       extends OPMInstruction    { val props = Seq(F6(OPMFunct6.mul)     , MULHi.N) }
object MULH      extends OPMInstruction    { val props = Seq(F6(OPMFunct6.mulh)    , MULHi.Y, MULSign1.Y, MULSign2.Y) }
object MULHU     extends OPMInstruction    { val props = Seq(F6(OPMFunct6.mulhu)   , MULHi.Y, MULSign1.N, MULSign2.N) }
object MULHSU    extends OPMInstruction    { val props = Seq(F6(OPMFunct6.mulhsu)  , MULHi.Y, MULSign1.N, MULSign2.Y) }
object WMUL      extends OPMInstruction    { val props = Seq(F6(OPMFunct6.wmul)    , MULHi.N, MULSign1.Y, MULSign2.Y, Wide2VD.Y) }
object WMULU     extends OPMInstruction    { val props = Seq(F6(OPMFunct6.wmulu)   , MULHi.N, MULSign1.N, MULSign2.N, Wide2VD.Y) }
object WMULSU    extends OPMInstruction    { val props = Seq(F6(OPMFunct6.wmulsu)  , MULHi.N, MULSign1.N, MULSign2.Y, Wide2VD.Y) }
object MACC      extends OPMInstruction    { val props = Seq(F6(OPMFunct6.macc)    , MULHi.N, ReadsVD.Y, MULAccumulate.Y, MULSub.N) }
object NMSAC     extends OPMInstruction    { val props = Seq(F6(OPMFunct6.nmsac)   , MULHi.N, ReadsVD.Y, MULAccumulate.Y, MULSub.Y) }
object MADD      extends OPMInstruction    { val props = Seq(F6(OPMFunct6.madd)    , MULHi.N, ReadsVD.Y, MULAccumulate.Y, MULSub.N, MULSwapVdV2.Y) }
object NMSUB     extends OPMInstruction    { val props = Seq(F6(OPMFunct6.nmsub)   , MULHi.N, ReadsVD.Y, MULAccumulate.Y, MULSub.Y, MULSwapVdV2.Y) }
object WMACC     extends OPMInstruction    { val props = Seq(F6(OPMFunct6.wmacc)   , MULHi.N, ReadsVD.Y, MULSign1.Y, MULSign2.Y, MULAccumulate.Y, MULSub.N, Wide2VD.Y) }
object WMACCU    extends OPMInstruction    { val props = Seq(F6(OPMFunct6.wmaccu)  , MULHi.N, ReadsVD.Y, MULSign1.N, MULSign2.N, MULAccumulate.Y, MULSub.N, Wide2VD.Y) }
object WMACCUS   extends OPMInstruction    { val props = Seq(F6(OPMFunct6.wmaccus) , MULHi.N, ReadsVD.Y, MULSign1.N, MULSign2.Y, MULAccumulate.Y, MULSub.N, Wide2VD.Y) }
object WMACCSU   extends OPMInstruction    { val props = Seq(F6(OPMFunct6.wmaccsu) , MULHi.N, ReadsVD.Y, MULSign1.Y, MULSign2.N, MULAccumulate.Y, MULSub.N, Wide2VD.Y) }
object SMUL      extends OPIInstruction    { val props = Seq(F6(OPIFunct6.smul)    , MULHi.N, MULSign1.Y, MULSign2.Y, MULSub.N) }

object DIV       extends OPMInstruction    { val props = Seq(F6(OPMFunct6.div)) }
object DIVU      extends OPMInstruction    { val props = Seq(F6(OPMFunct6.divu)) }
object REM       extends OPMInstruction    { val props = Seq(F6(OPMFunct6.rem)) }
object REMU      extends OPMInstruction    { val props = Seq(F6(OPMFunct6.remu)) }

object MV_S_X    extends VectorInstruction { val props = Seq(F6(OPMFunct6.wrxunary0), F3(VectorConsts.OPMVX), RS2( 0.U(5.W)), ReadsVS2.N, VMBitReadsVM.N, ScalarToVD0.Y) }
object MV_X_S    extends VectorInstruction { val props = Seq(F6(OPMFunct6.wrxunary0), F3(VectorConsts.OPMVV), RS1( 0.U(5.W)), ReadsVS2.Y, VMBitReadsVM.N, WritesScalar.Y, WritesVD.N) }
object POPC      extends VectorInstruction { val props = Seq(F6(OPMFunct6.wrxunary0), F3(VectorConsts.OPMVV), RS1(16.U(5.W)), ReadsVS2.Y, VMBitReadsVM.Y, WritesScalar.Y, WritesVD.N, ReadsVS2AsMask.Y) }
object FIRST     extends VectorInstruction { val props = Seq(F6(OPMFunct6.wrxunary0), F3(VectorConsts.OPMVV), RS1(17.U(5.W)), ReadsVS2.Y, VMBitReadsVM.Y, WritesScalar.Y, WritesVD.N, ReadsVS2AsMask.Y) }
object FMV_S_F   extends VectorInstruction { val props = Seq(F6(OPFFunct6.wrfunary0), F3(VectorConsts.OPFVF), RS2( 0.U(5.W)), ReadsVS2.N, VMBitReadsVM.N, ScalarToVD0.Y) }
object FMV_F_S   extends VectorInstruction { val props = Seq(F6(OPFFunct6.wrfunary0), F3(VectorConsts.OPFVV), RS1( 0.U(5.W)), ReadsVS2.Y, VMBitReadsVM.N, WritesScalar.Y, WritesVD.N) }
object MSBF      extends VectorInstruction { val props = Seq(F6(OPMFunct6.munary0)  , F3(VectorConsts.OPMVV), RS1( 1.U(5.W)), ReadsVS2AsMask.Y, WritesAsMask.Y) }
object MSOF      extends VectorInstruction { val props = Seq(F6(OPMFunct6.munary0)  , F3(VectorConsts.OPMVV), RS1( 2.U(5.W)), ReadsVS2AsMask.Y, WritesAsMask.Y) }
object MSIF      extends VectorInstruction { val props = Seq(F6(OPMFunct6.munary0)  , F3(VectorConsts.OPMVV), RS1( 3.U(5.W)), ReadsVS2AsMask.Y, WritesAsMask.Y) }
object IOTA      extends VectorInstruction { val props = Seq(F6(OPMFunct6.munary0)  , F3(VectorConsts.OPMVV), RS1(16.U(5.W)), ReadsVS2AsMask.Y) }
object ID        extends VectorInstruction { val props = Seq(F6(OPMFunct6.munary0)  , F3(VectorConsts.OPMVV), RS1(17.U(5.W)), ReadsVS2AsMask.Y, ReadsVS2.N) }

object FADD      extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fadd)     , FPAdd.Y, FPMul.N, FPSwapVdV2.N, FPFMACmd(0.U(2.W))) }
object FSUB      extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fsub)     , FPAdd.Y, FPMul.N, FPSwapVdV2.N, FPFMACmd(1.U(2.W))) }
object FRSUB     extends OPFInstruction    { val props = Seq(F6(OPFFunct6.frsub)    , FPAdd.Y, FPMul.N, FPSwapVdV2.N, FPFMACmd(2.U(2.W))) }
object FMUL      extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fmul)     , FPAdd.N, FPMul.Y, FPSwapVdV2.N, FPFMACmd(0.U(2.W))) }
object FMACC     extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fmacc)    , FPAdd.Y, FPMul.Y, FPSwapVdV2.N, FPFMACmd(0.U(2.W)), ReadsVD.Y) }
object FNMACC    extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fnmacc)   , FPAdd.Y, FPMul.Y, FPSwapVdV2.N, FPFMACmd(3.U(2.W)), ReadsVD.Y) }
object FMSAC     extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fmsac)    , FPAdd.Y, FPMul.Y, FPSwapVdV2.N, FPFMACmd(1.U(2.W)), ReadsVD.Y) }
object FNMSAC    extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fnmsac)   , FPAdd.Y, FPMul.Y, FPSwapVdV2.N, FPFMACmd(2.U(2.W)), ReadsVD.Y) }
object FMADD     extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fmadd)    , FPAdd.Y, FPMul.Y, FPSwapVdV2.Y, FPFMACmd(0.U(2.W)), ReadsVD.Y) }
object FNMADD    extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fnmadd)   , FPAdd.Y, FPMul.Y, FPSwapVdV2.Y, FPFMACmd(3.U(2.W)), ReadsVD.Y) }
object FMSUB     extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fmsub)    , FPAdd.Y, FPMul.Y, FPSwapVdV2.Y, FPFMACmd(1.U(2.W)), ReadsVD.Y) }
object FNMSUB    extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fnmsub)   , FPAdd.Y, FPMul.Y, FPSwapVdV2.Y, FPFMACmd(2.U(2.W)), ReadsVD.Y) }
object FWADD     extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fwadd)    , FPAdd.Y, FPMul.N, FPSwapVdV2.N, FPFMACmd(0.U(2.W)), Wide2VD.Y) }
object FWSUB     extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fwsub)    , FPAdd.Y, FPMul.N, FPSwapVdV2.N, FPFMACmd(1.U(2.W)), Wide2VD.Y) }
object FWADDW    extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fwaddw)   , FPAdd.Y, FPMul.N, FPSwapVdV2.N, FPFMACmd(0.U(2.W)), Wide2VD.Y, Wide2VS2.Y) }
object FWSUBW    extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fwsubw)   , FPAdd.Y, FPMul.N, FPSwapVdV2.N, FPFMACmd(1.U(2.W)), Wide2VD.Y, Wide2VS2.Y) }
object FWMUL     extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fwmul)    , FPAdd.N, FPMul.Y, FPSwapVdV2.N, FPFMACmd(0.U(2.W)), Wide2VD.Y) }
object FWMACC    extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fwmacc)   , FPAdd.Y, FPMul.Y, FPSwapVdV2.N, FPFMACmd(0.U(2.W)), Wide2VD.Y, ReadsVD.Y) }
object FWNMACC   extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fwnmacc)  , FPAdd.Y, FPMul.Y, FPSwapVdV2.N, FPFMACmd(3.U(2.W)), Wide2VD.Y, ReadsVD.Y) }
object FWMSAC    extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fwmsac)   , FPAdd.Y, FPMul.Y, FPSwapVdV2.N, FPFMACmd(1.U(2.W)), Wide2VD.Y, ReadsVD.Y) }
object FWNMSAC   extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fwnmsac)  , FPAdd.Y, FPMul.Y, FPSwapVdV2.N, FPFMACmd(2.U(2.W)), Wide2VD.Y, ReadsVD.Y) }
object FREDOSUM  extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fredosum) , FPAdd.Y, FPMul.N, FPSwapVdV2.N, FPFMACmd(0.U(2.W)), Reduction.Y, AccInitZeros.Y, Elementwise.Y) }
object FREDUSUM  extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fredusum) , FPAdd.Y, FPMul.N, FPSwapVdV2.N, FPFMACmd(0.U(2.W)), Reduction.Y, AccInitZeros.Y) }
object FWREDOSUM extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fwredosum), FPAdd.Y, FPMul.N, FPSwapVdV2.N, FPFMACmd(0.U(2.W)), Wide2VD.Y, Reduction.Y, AccInitZeros.Y, Elementwise.Y) }
object FWREDUSUM extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fwredusum), FPAdd.Y, FPMul.N, FPSwapVdV2.N, FPFMACmd(0.U(2.W)), Wide2VD.Y, Reduction.Y, AccInitZeros.Y) }

object FDIV      extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fdiv)     , FPSwapVdV2.N, FPAdd.N, FPMul.N) }
object FRDIV     extends OPFInstruction    { val props = Seq(F6(OPFFunct6.frdiv)    , FPSwapVdV2.Y, FPAdd.N, FPMul.N) }
object FSQRT_V   extends VectorInstruction { val props = Seq(F6(OPFFunct6.funary1)  , F3(VectorConsts.OPFVV), RS1( 0.U(5.W)), FPSwapVdV2.N, FPAdd.N, FPMul.N) }
object FRSQRT7_V extends VectorInstruction { val props = Seq(F6(OPFFunct6.funary1)  , F3(VectorConsts.OPFVV), RS1( 4.U(5.W)), FPSwapVdV2.N) }
object FREC7_V   extends VectorInstruction { val props = Seq(F6(OPFFunct6.funary1)  , F3(VectorConsts.OPFVV), RS1( 5.U(5.W)), FPSwapVdV2.N) }
object FCLASS_V  extends VectorInstruction { val props = Seq(F6(OPFFunct6.funary1)  , F3(VectorConsts.OPFVV), RS1(16.U(5.W)), FPSwapVdV2.N, FPAdd.N, FPMul.N, FPSpecRM(1.U(3.W))) }

object FMIN      extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fmin)     , FPComp.Y, FPCompMin.Y, FPAdd.N, FPMul.N, FPSpecRM(0.U(3.W))) }
object FMAX      extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fmax)     , FPComp.Y, FPCompMin.N, FPAdd.N, FPMul.N, FPSpecRM(1.U(3.W))) }
object FSGNJ     extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fsgnj)    , FPSgnj.Y, FPAdd.N, FPMul.N, FPSpecRM(0.U(3.W))) }
object FSGNJN    extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fsgnjn)   , FPSgnj.Y, FPAdd.N, FPMul.N, FPSpecRM(1.U(2.W))) }
object FSGNJX    extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fsgnjx)   , FPSgnj.Y, FPAdd.N, FPMul.N, FPSpecRM(2.U(3.W))) }
object MFEQ      extends OPFInstruction    { val props = Seq(F6(OPFFunct6.mfeq)     , WritesAsMask.Y, FPMEQ.Y, FPMNE.N, FPMLT.N, FPMGT.N, FPAdd.N, FPMul.N, FPSpecRM(2.U(3.W))) }
object MFNE      extends OPFInstruction    { val props = Seq(F6(OPFFunct6.mfne)     , WritesAsMask.Y, FPMEQ.N, FPMNE.Y, FPMLT.N, FPMGT.N, FPAdd.N, FPMul.N, FPSpecRM(2.U(3.W))) }
object MFLT      extends OPFInstruction    { val props = Seq(F6(OPFFunct6.mflt)     , WritesAsMask.Y, FPMEQ.N, FPMNE.N, FPMLT.Y, FPMGT.N, FPAdd.N, FPMul.N, FPSpecRM(1.U(3.W))) }
object MFLE      extends OPFInstruction    { val props = Seq(F6(OPFFunct6.mfle)     , WritesAsMask.Y, FPMEQ.Y, FPMNE.N, FPMLT.Y, FPMGT.N, FPAdd.N, FPMul.N, FPSpecRM(0.U(3.W))) }
object MFGT      extends OPFInstruction    { val props = Seq(F6(OPFFunct6.mfgt)     , WritesAsMask.Y, FPMEQ.N, FPMNE.N, FPMLT.N, FPMGT.Y, FPAdd.N, FPMul.N, FPSpecRM(0.U(3.W))) }
object MFGE      extends OPFInstruction    { val props = Seq(F6(OPFFunct6.mfge)     , WritesAsMask.Y, FPMEQ.Y, FPMNE.N, FPMLT.N, FPMGT.Y, FPAdd.N, FPMul.N, FPSpecRM(1.U(3.W))) }
object FREDMIN   extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fredmin)  , FPComp.Y, FPCompMin.Y, Reduction.Y, FPAdd.N, FPMul.N, FPSpecRM(0.U(3.W))) }
object FREDMAX   extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fredmax)  , FPComp.Y, FPCompMin.N, Reduction.Y, FPAdd.N, FPMul.N, FPSpecRM(1.U(3.W))) }

object FCVT_SGL  extends VectorInstruction { val props = Seq(F6(OPFFunct6.funary0), F3(VectorConsts.OPFVV), RS1(BitPat("b00???")), FPAdd.N, FPMul.N) }
object FCVT_WID  extends VectorInstruction { val props = Seq(F6(OPFFunct6.funary0), F3(VectorConsts.OPFVV), RS1(BitPat("b01???")), Wide2VD.Y, FPAdd.N, FPMul.N) }
object FCVT_NRW  extends VectorInstruction { val props = Seq(F6(OPFFunct6.funary0), F3(VectorConsts.OPFVV), RS1(BitPat("b10???")), Wide2VD.N, Wide2VS2.Y, FPAdd.N, FPMul.N) }

object SLIDEUP     extends OPIInstruction    { val props = Seq(F6(OPIFunct6.slideup)    , UsesGatherUnit.Y, ReadsVS2.N, Slide.Y) }
object SLIDEDOWN   extends OPIInstruction    { val props = Seq(F6(OPIFunct6.slidedown)  , UsesGatherUnit.Y, ReadsVS2.N, Slide.Y) }
object SLIDE1UP    extends OPMInstruction    { val props = Seq(F6(OPMFunct6.slide1up)   , UsesGatherUnit.Y, ReadsVS2.N, Slide.Y) }
object SLIDE1DOWN  extends OPMInstruction    { val props = Seq(F6(OPMFunct6.slide1down) , UsesGatherUnit.Y, ReadsVS2.N, Slide.Y) }
object FSLIDE1UP   extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fslide1up)  , UsesGatherUnit.Y, ReadsVS2.N, Slide.Y) }
object FSLIDE1DOWN extends OPFInstruction    { val props = Seq(F6(OPFFunct6.fslide1down), UsesGatherUnit.Y, ReadsVS2.N, Slide.Y) }

object RGATHER_VX  extends VectorInstruction    { val props = Seq(F6(OPIFunct6.rgather)    , F3(VectorConsts.OPIVX)) }
object RGATHER_VI  extends VectorInstruction    { val props = Seq(F6(OPIFunct6.rgather)    , F3(VectorConsts.OPIVI)) }
object RGATHER_VV  extends VectorInstruction    { val props = Seq(F6(OPIFunct6.rgather)    , F3(VectorConsts.OPIVV), UsesGatherUnit.Y, Elementwise.Y) }
object RGATHEREI16 extends VectorInstruction    { val props = Seq(F6(OPIFunct6.rgatherei16), F3(VectorConsts.OPIVV), UsesGatherUnit.Y, Elementwise.Y) }
object COMPRESS    extends OPMInstruction       { val props = Seq(F6(OPMFunct6.compress)   , ReadsVS1AsMask.Y, Elementwise.Y) }
object MVNRR       extends VectorInstruction    { val props = Seq(F6(OPIFunct6.mvnrr)      , F3(VectorConsts.OPIVI)) }


// Zvbb instructions
object ANDN       extends OPIInstruction    { val props = Seq(F6(OPIFunct6.andn)     , BWAnd.Y, BWInv1.Y) }
object BREV8      extends OPMInstruction    { val props = Seq(F6(OPMFunct6.xunary0)  , RS1(BitPat("b01000")), UsesBitSwap.Y) }
object REV8       extends OPMInstruction    { val props = Seq(F6(OPMFunct6.xunary0)  , RS1(BitPat("b01001")), UsesBitSwap.Y) }
object BREV       extends OPMInstruction    { val props = Seq(F6(OPMFunct6.xunary0)  , RS1(BitPat("b01010")), UsesBitSwap.Y) }
object CLZ        extends OPMInstruction    { val props = Seq(F6(OPMFunct6.xunary0)  , RS1(BitPat("b01100")), UsesCountZeros.Y) }
object CTZ        extends OPMInstruction    { val props = Seq(F6(OPMFunct6.xunary0)  , RS1(BitPat("b01101")), UsesCountZeros.Y) }
object CPOP       extends OPMInstruction    { val props = Seq(F6(OPMFunct6.xunary0)  , RS1(BitPat("b01110")), UsesCountZeros.Y) }
object ROL        extends OPIInstruction    { val props = Seq(F6(OPIFunct6.rol)      , UsesShift.Y, ShiftsLeft.Y, ScalingShift.N) }
object RORI       extends OPIInstruction    { val props = Seq(F6(OPIFunct6.rol)      , UsesShift.Y, ShiftsLeft.Y, ScalingShift.N) }
object ROR        extends OPIInstruction    { val props = Seq(F6(OPIFunct6.ror)      , UsesShift.Y, ShiftsLeft.N, ScalingShift.N) }
object WSLL       extends OPIInstruction    { val props = Seq(F6(OPIFunct6.wsll)     , UsesShift.Y, ShiftsLeft.Y, ScalingShift.N, Wide2VD.Y, ZextImm5.Y) }

// Outer product instructions
object OPMACC      extends OPMInstruction    { val props = Seq(F6(OPMFunct6.opmacc)     , ReadsVS1.Y, ReadsVS2.Y, WritesVD.N) }
object OPMVIN      extends OPMInstruction    { val props = Seq(F6(OPMFunct6.opmvin)     , ReadsVS1.N, ReadsVS2.Y, WritesVD.N) }
object OPMVINBCAST extends OPMInstruction    { val props = Seq(F6(OPMFunct6.opmvinbcast), ReadsVS1.N, ReadsVS2.Y, WritesVD.N) }
object OPMVOUT     extends OPMInstruction    { val props = Seq(F6(OPMFunct6.opmvout)    , ReadsVS1.N, ReadsVS2.N, WritesVD.Y) }
