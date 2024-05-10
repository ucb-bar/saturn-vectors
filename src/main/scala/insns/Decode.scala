package saturn.insns

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._

import org.chipsalliance.cde.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.rocket._
import freechips.rocketchip.rocket.constants._
import freechips.rocketchip.util._


class VectorDecoder(
  funct3: UInt, funct6: UInt, rs1: UInt, rs2: UInt,
  insns: Seq[VectorInstruction],
  fields: Seq[InstructionField]) {

  val index = Cat(rs1(4,0), rs2(4,0), funct3(2,0), funct6(5,0))
  val lookups = insns.map { i => i.lookup(RS1) ## i.lookup(RS2) ## i.lookup(F3) ## i.lookup(F6) }
  val duplicates = lookups.diff(lookups.distinct).distinct
  //require(duplicates.size == 0, s"Found duplicates $duplicates")
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
