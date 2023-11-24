package vector.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import vector.common._

abstract class FunctionalUnitIO(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val iss = new Bundle {
    val funct3 = Input(UInt(3.W))
    val funct6 = Input(UInt(6.W))
    val ready = Output(Bool())
  }
}

class PipelinedFunctionalUnitIO(depth: Int)(implicit p: Parameters) extends FunctionalUnitIO {
  val writes = Vec(2, Valid(new VectorWrite))
  val pipe = Input(Vec(depth, Valid(new VectorMicroOp)))
}

class IterativeFunctionalUnitIO(implicit p: Parameters) extends FunctionalUnitIO {
  val iss_op = Input(Valid(new VectorMicroOp))

  val write = Decoupled(new VectorWrite)
  val vat = Output(Valid(UInt(vParams.vatSz.W)))
  val hazard = Output(Valid(new PipeHazard))

  val busy = Output(Bool())
}

abstract class FunctionalUnit(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io: FunctionalUnitIO

  def accepts(f3: UInt, f6: UInt): Bool
}

abstract class PipelinedFunctionalUnit(val depth: Int)(implicit p: Parameters) extends FunctionalUnit()(p) {
  val io = IO(new PipelinedFunctionalUnitIO(depth))

  require (depth > 0)
  io.iss.ready := accepts(io.iss.funct3, io.iss.funct6)
}
abstract class IterativeFunctionalUnit(implicit p: Parameters) extends FunctionalUnit()(p) {
  val io = IO(new IterativeFunctionalUnitIO)

  val valid = RegInit(false.B)
  val op = Reg(new VectorMicroOp)
  val last = Wire(Bool())

  io.iss.ready := accepts(io.iss.funct3, io.iss.funct6) && (!valid || last)
  io.vat.valid := valid && op.last
  io.vat.bits  := op.vat
  io.busy := valid

  when (io.iss_op.valid && io.iss.ready) {
    valid := true.B
    op := io.iss_op.bits
  } .elsewhen (last) {
    valid := false.B
  }
}
