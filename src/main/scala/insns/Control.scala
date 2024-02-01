package saturn.insns

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.rocket._
import freechips.rocketchip.rocket.constants._
import freechips.rocketchip.util._

object F6            extends XDefaultInstructionField { override val width: Int = 6 }
object F3            extends XDefaultInstructionField { override val width: Int = 3 }
object RS1           extends XDefaultInstructionField { override val width: Int = 5 }
object RS2           extends XDefaultInstructionField { override val width: Int = 5 }

object AlwaysReadsVM     extends NDefaultInstructionField
object VMBitReadsVM      extends YDefaultInstructionField
object ReadsVS1          extends NDefaultInstructionField
object ReadsVS2          extends YDefaultInstructionField
object ReadsVD           extends NDefaultInstructionField
object WritesVD          extends YDefaultInstructionField
object ScalarToVD0       extends NDefaultInstructionField
object Reduction         extends NDefaultInstructionField
object Wide2VD           extends NDefaultInstructionField
object Wide2VS2          extends NDefaultInstructionField
object WritesAsMask      extends NDefaultInstructionField
object ReadsVS1AsMask    extends NDefaultInstructionField
object ReadsVS2AsMask    extends NDefaultInstructionField
object WritesScalar      extends NDefaultInstructionField
object UsesPermuteSeq    extends NDefaultInstructionField

// Execute Sequencer control
object Elementwise       extends NDefaultInstructionField
object SetsWMask         extends YDefaultInstructionField
object AccInitZeros      extends NDefaultInstructionField
object AccInitOnes       extends NDefaultInstructionField
object AccInitPos        extends NDefaultInstructionField
object AccInitNeg        extends NDefaultInstructionField

// Integer Pipe control
object Swap12            extends NDefaultInstructionField
object WideningSext      extends XDefaultInstructionField

object DoSub             extends XDefaultInstructionField
object Averaging         extends XDefaultInstructionField
object CarryIn           extends XDefaultInstructionField
object AlwaysCarryIn     extends XDefaultInstructionField

object UsesShift         extends NDefaultInstructionField
object ShiftsLeft        extends XDefaultInstructionField
object ScalingShift      extends XDefaultInstructionField

object UsesCmp           extends NDefaultInstructionField
object CmpLess           extends XDefaultInstructionField

object UsesNarrowingSext extends NDefaultInstructionField

object UsesMinMax        extends NDefaultInstructionField

object UsesMerge         extends NDefaultInstructionField
object UsesSat           extends NDefaultInstructionField

// Bitwise pipe control
object BWAnd             extends NDefaultInstructionField
object BWOr              extends NDefaultInstructionField
object BWXor             extends NDefaultInstructionField
object BWInvOut          extends NDefaultInstructionField
object BWInv1            extends NDefaultInstructionField

// Multiply control
object MULHi             extends XDefaultInstructionField
object MULSign1          extends XDefaultInstructionField
object MULSign2          extends XDefaultInstructionField
object MULSwapVdV2       extends NDefaultInstructionField
object MULAccumulate     extends NDefaultInstructionField
object MULSub            extends XDefaultInstructionField

// FPFMA control
object FPAdd             extends XDefaultInstructionField
object FPMul             extends XDefaultInstructionField
object FPSwapVdV2        extends XDefaultInstructionField
object FPFMACmd          extends XDefaultInstructionField { override val width = 2 }

// FPComp control
object FPComp            extends NDefaultInstructionField
object FPCompMin         extends XDefaultInstructionField

object FPMEQ             extends XDefaultInstructionField
object FPMNE             extends XDefaultInstructionField
object FPMLT             extends XDefaultInstructionField
object FPMGT             extends XDefaultInstructionField

object FPSgnj            extends NDefaultInstructionField
object FPSpecRM          extends XDefaultInstructionField { override val width = 3 }
