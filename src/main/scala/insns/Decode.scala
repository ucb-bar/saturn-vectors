package saturn.insns

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._

import org.chipsalliance.cde.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.rocket._
import freechips.rocketchip.rocket.constants._
import freechips.rocketchip.util._

trait HasVectorDecoderSignals {
  def funct6: UInt
  def funct3: UInt
  def rs1: UInt
  def rs2: UInt
  def sew: UInt
}

class VectorDecodedControl(insns: Seq[VectorInstruction], fields: Seq[InstructionField]) extends Bundle {
  val matched = Bool()
  val signals = MixedVec(fields.map(f => UInt(f.width.W)))

  def uint(field: InstructionField): UInt = {
    val index = fields.indexOf(field)
    require (index >= 0, s"Field $field not decoded here")
    signals(index)
  }

  def bool(field: InstructionField): Bool = {
    require (field.width == 1)
    uint(field)(0)
  }

  def decode(bundle: HasVectorDecoderSignals): VectorDecodedControl = decode(
    bundle.rs1, bundle.rs2, bundle.funct3, bundle.funct6, bundle.sew)

  def decode(rs1: UInt, rs2: UInt, funct3: UInt, funct6: UInt, sew: UInt): VectorDecodedControl = {
    val decoder = new VectorDecoder(rs1, rs2, funct3, funct6, sew, insns, fields)

    matched := decoder.matched
    fields.zipWithIndex.foreach { case (f, i) =>
      signals(i) := decoder.uint(f)
    }
    this
  }
}

class VectorDecoder(
  rs1: UInt, rs2: UInt, funct3: UInt, funct6: UInt, sew: UInt,
  insns: Seq[VectorInstruction],
  fields: Seq[InstructionField]) {

  def this(bundle: HasVectorDecoderSignals, insns: Seq[VectorInstruction], fields: Seq[InstructionField]) = {
    this(bundle.rs1, bundle.rs2, bundle.funct3, bundle.funct6, bundle.sew,
      insns, fields)
  }

  val index = Cat(rs1(4,0), rs2(4,0), funct3(2,0), funct6(5,0), sew(1,0))
  val lookups = insns.map { i => i.lookup(RS1) ## i.lookup(RS2) ## i.lookup(F3) ## i.lookup(F6) ## i.lookup(SEW) }
  val duplicates = lookups.diff(lookups.distinct).distinct
  val table = insns.map { i => fields.map(f => i.lookup(f)) :+ BitPat(true.B) }

  val elementsGrouped = table.transpose
  val defaults = fields.map(_.dontCare) :+ BitPat(false.B)
  val elementWidths = elementsGrouped.zip(defaults).map { case (elts, default) =>
    require(elts.forall(_.getWidth == default.getWidth))
    default.getWidth
  }
  val resultWidth = elementWidths.sum
  val elementIndices = elementWidths.scan(resultWidth-1) { case (l,r) => l - r }
  val truthTable = TruthTable(lookups.zip(table).map { case (l,r) => (l, r.reduce(_ ## _)) }, defaults.reduce(_ ## _))
  val decode = chisel3.util.experimental.decode.decoder(index, truthTable)
  val decoded = elementIndices.zip(elementIndices.tail).map { case (msb, lsb) => decode(msb, lsb+1) }.toSeq


  def uint(field: InstructionField): UInt = {
    val index = fields.indexOf(field)
    require(index >= 0, s"Field $field not found in this decoder")
    decoded(index)
  }
  def bool(field: InstructionField): Bool = {
    require(field.width == 1)
    uint(field)(0)
  }
  def matched: Bool = decoded.last(0)
}
