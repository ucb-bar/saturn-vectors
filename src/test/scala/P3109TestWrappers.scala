package saturn.exu

import chisel3._
import freechips.rocketchip.tile.FType
import saturn.common._

// Standalone wrappers around P3109Rounder for the Verilator checks in models/
// (see models/run_rounder_check.sh, which emits them)

// The conversion unit's use: a BF16 pattern in
class P3109ConvWrapper(formats: P3109Formats, name: String) extends RawModule {
  override def desiredName = name
  val io = IO(new Bundle {
    val in             = Input(UInt(16.W))
    val altfmt         = Input(Bool())
    val roundingMode   = Input(UInt(3.W))
    val sat            = Input(Bool())
    val out            = Output(UInt(8.W))
    val exceptionFlags = Output(UInt(5.W))
  })
  val raw = hardfloat.rawFloatFromFN(8, 8, io.in)
  val rounder = Module(new P3109Rounder(8, 8, formats, sigMSBitAlwaysZero = true))
  rounder.io.in := raw
  rounder.io.altfmt := io.altfmt
  rounder.io.roundingMode := io.roundingMode
  rounder.io.sat := io.sat
  rounder.io.invalidExc := hardfloat.isSigNaNRawFloat(raw)
  io.out := rounder.io.out
  io.exceptionFlags := rounder.io.exceptionFlags
}

// The FMA's use, through rawUnroundedToP3109: the unrounded result of one core
// type, driven field by field since its significand can be in [2,4)
class P3109FmaRoundWrapper(core: FType, formats: P3109Formats, name: String) extends RawModule {
  override def desiredName = name
  val io = IO(new Bundle {
    val isNaN          = Input(Bool())
    val isInf          = Input(Bool())
    val isZero         = Input(Bool())
    val sign           = Input(Bool())
    val sExp           = Input(SInt((core.exp + 2).W))
    val sig            = Input(UInt((core.sig + 3).W))
    val altfmt         = Input(Bool())
    val roundingMode   = Input(UInt(3.W))
    val out            = Output(UInt(8.W))
    val exceptionFlags = Output(UInt(5.W))
  })
  val raw = Wire(new hardfloat.RawFloat(core.exp, core.sig + 2))
  raw.isNaN := io.isNaN
  raw.isInf := io.isInf
  raw.isZero := io.isZero
  raw.sign := io.sign
  raw.sExp := io.sExp
  raw.sig := io.sig
  val (out, flags) = rawUnroundedToP3109(core, raw, false.B, io.altfmt, io.roundingMode, formats)
  io.out := out
  io.exceptionFlags := flags
}

object P3109TestWrappers extends App {
  val dir = if (args.nonEmpty) args(0) else "."
  val opts = Array("-disable-all-randomization", "-strip-debug-info")

  def emit(name: String, gen: String => RawModule): Unit = {
    val sv = circt.stage.ChiselStage.emitSystemVerilog(gen(name), firtoolOpts = opts)
    java.nio.file.Files.write(java.nio.file.Paths.get(s"$dir/$name.sv"), sv.getBytes)
  }

  val domains = Seq(
    "Ext" -> P3109Formats(),
    "Fin" -> P3109Formats(p4 = P3109Domain.Finite, p3 = P3109Domain.Finite))
  // Every core type an 8-bit FMA lane can run on (ftype_used_for in FPFMAPipe)
  val fmaCores = Seq("FP64" -> FType.D, "FP32" -> FType.S, "FP16" -> FType.H,
                     "BF16" -> MXFType.BF16, "E5M3" -> MXFType.E5M3)

  for ((dom, formats) <- domains) {
    emit(s"P3109Conv$dom", new P3109ConvWrapper(formats, _))
    for ((coreName, core) <- fmaCores)
      emit(s"P3109FmaRound$coreName$dom", new P3109FmaRoundWrapper(core, formats, _))
  }
}
