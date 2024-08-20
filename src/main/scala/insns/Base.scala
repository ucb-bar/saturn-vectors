package saturn.insns

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.rocket._
import freechips.rocketchip.rocket.constants._
import freechips.rocketchip.util._

trait InstructionField {
  def default: BitPat
  def width: Int
  def Y = InstructionProperty(this, true.B)
  def N = InstructionProperty(this, false.B)
  def dontCare: BitPat = BitPat.dontCare(width)
  def apply(v: UInt) = InstructionProperty(this, v.pad(width)(width-1,0))
  def apply(v: BitPat) = InstructionProperty(this, v)
  def apply(e: EnumType) = InstructionProperty(this, e.litValue.U(width.W))
}

trait NDefaultInstructionField extends InstructionField {
  val default: BitPat = false.B
  val width: Int = 1
}

trait YDefaultInstructionField extends InstructionField {
  val default: BitPat = true.B
  val width: Int = 1
}

trait XDefaultInstructionField extends InstructionField {
  def default: BitPat = BitPat.dontCare(width)
  val width: Int = 1
}

case class InstructionProperty(val field: InstructionField, val value: BitPat)

trait VectorInstruction {
  val props: Seq[InstructionProperty]
  def lookup(field: InstructionField) = {
    val matches = props.collect { case InstructionProperty(`field`, value) => value }
    if (matches.size > 0) {
      require(matches.toSet.size <= 1, s"Field lookup for $field returned multiple results")
      matches(0)
    } else {
      field.default
    }
  }
  def append(add_props: InstructionProperty*) = new AppendedVectorInstruction(props, add_props)
  def elementWise: VectorInstruction = append(Elementwise.Y)
  def pipelined(depth: Int): VectorInstruction = { require(depth >= 1); append(PipelinedExecution.Y, PipelineStagesMinus1((depth-1).U)) }
  def iterative: VectorInstruction = append(PipelinedExecution.N)
}

class AppendedVectorInstruction(
  base_props: Seq[InstructionProperty],
  add_props: Seq[InstructionProperty]) extends VectorInstruction {
  val props = base_props ++ add_props
}
