package vector.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import vector.common._
import vector.insns.{VectorInstruction}

abstract class FunctionalUnitIO(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val iss = new Bundle {
    val valid = Input(Bool())
    val op = Input(new ExecuteMicroOp)
    val sub_dlen = Output(UInt(log2Ceil(dLenB).W))
    val ready = Output(Bool())
  }

  val scalar_write = Decoupled(new ScalarWrite)
  val set_vxsat = Output(Bool())
  val set_fflags = Output(Valid(UInt(5.W)))
}

class PipelinedFunctionalUnitIO(depth: Int)(implicit p: Parameters) extends FunctionalUnitIO {
  val write = Valid(new VectorWrite(dLen))
  val pipe = Input(Vec(depth, Valid(new ExecuteMicroOp)))
}

class IterativeFunctionalUnitIO(implicit p: Parameters) extends FunctionalUnitIO {
  val write = Decoupled(new VectorWrite(dLen))
  val vat = Output(Valid(UInt(vParams.vatSz.W)))
  val hazard = Output(Valid(new PipeHazard))

  val busy = Output(Bool())

  val fp_req = Decoupled(new FPInput()) 
  val fp_resp = Flipped(Decoupled(new FPResult()))
}

abstract class FunctionalUnit(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io: FunctionalUnitIO
  val supported_insns: Seq[VectorInstruction]

  def extract(in: UInt, sext: Bool, in_eew: UInt, eidx: UInt): SInt = {
    val bytes = in.asTypeOf(Vec(dLenB, UInt(8.W)))
    VecInit.tabulate(4) { eew =>
      val elem = if (dLen == 64 && eew == 3) {
        in
      } else {
        VecInit(bytes.grouped(1 << eew).map(g => VecInit(g).asUInt).toSeq)(eidx(log2Ceil(dLenB)-1-eew,0))
      }
      val hi = sext && elem((8 << eew)-1)
      Cat(hi, elem((8 << eew)-1,0)).asSInt
    }(in_eew)
  }
}

abstract class PipelinedFunctionalUnit(val depth: Int)(implicit p: Parameters) extends FunctionalUnit()(p) {
  val io = IO(new PipelinedFunctionalUnitIO(depth))

  require (depth > 0)
}
abstract class IterativeFunctionalUnit(implicit p: Parameters) extends FunctionalUnit()(p) {
  val io = IO(new IterativeFunctionalUnitIO)

  val valid = RegInit(false.B)
  val op = Reg(new ExecuteMicroOp)
  val last = Wire(Bool())

  io.vat.valid := valid && op.tail
  io.vat.bits  := op.vat
  io.busy := valid

  when (io.iss.valid && io.iss.ready) {
    valid := true.B
    op := io.iss.op
  } .elsewhen (last) {
    valid := false.B
  }
}
