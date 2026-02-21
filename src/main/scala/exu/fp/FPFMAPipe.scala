package saturn.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import saturn.common._
import saturn.insns._

class FMAPipeIO(implicit p: Parameters) extends Bundle {
  val valid = Input(Bool())
  val frm = Input(UInt(3.W))
  val addsub = Input(Bool())
  val mul = Input(Bool())
  val op = Input(UInt(2.W))
  val a_eew = Input(UInt(2.W))
  val b_eew = Input(UInt(2.W))
  val c_eew = Input(UInt(2.W))
  val out_eew = Input(UInt(2.W))
  val widen = Input(Bool())
  val altfmt = Input(Bool())
  val a = Input(UInt(64.W))
  val b = Input(UInt(64.W))
  val c = Input(UInt(64.W))

  val out = Output(UInt(64.W))
  val exc = Output(Vec(8, UInt(5.W)))
}

abstract class FMAPipe()(implicit p: Parameters) extends FPUModule()(p) {
  val io = IO(new FMAPipeIO)
}

class TandemFMAPipe(depth: Int, buildFP64: Boolean, mxFPFMA: Boolean)(implicit p: Parameters) extends FMAPipe()(p) {
  require (depth >= 4)
  require (!mxFPFMA) // Not supported for non-segmented FPFMA

  val out_eew_pipe = Pipe(io.valid, io.out_eew, depth-1)
  val frm_pipe = Pipe(io.valid, io.frm, depth-1)

  val da = io.a.asTypeOf(Vec(1, UInt(64.W))).map(f => FType.D.recode(f))
  val db = io.b.asTypeOf(Vec(1, UInt(64.W))).map(f => FType.D.recode(f))
  val dc = io.c.asTypeOf(Vec(1, UInt(64.W))).map(f => FType.D.recode(f))
  val sa = io.a.asTypeOf(Vec(2, UInt(32.W))).map(f => FType.S.recode(f))
  val sb = io.b.asTypeOf(Vec(2, UInt(32.W))).map(f => FType.S.recode(f))
  val sc = io.c.asTypeOf(Vec(2, UInt(32.W))).map(f => FType.S.recode(f))
  val ha = io.a.asTypeOf(Vec(4, UInt(16.W))).map(f => FType.H.recode(f))
  val hb = io.b.asTypeOf(Vec(4, UInt(16.W))).map(f => FType.H.recode(f))
  val hc = io.c.asTypeOf(Vec(4, UInt(16.W))).map(f => FType.H.recode(f))

  def widen(in: UInt, inT: FType, outT: FType, active: Bool): UInt = {
    val widen = Module(new hardfloat.RecFNToRecFN(inT.exp, inT.sig, outT.exp, outT.sig))
    widen.io.in := Mux(active, in, 0.U)
    widen.io.roundingMode := io.frm
    widen.io.detectTininess := hardfloat.consts.tininess_afterRounding
    widen.io.out
  }

  val dfma_valid = io.valid && io.out_eew === 3.U
  val sfma_valid = io.valid && io.out_eew === 2.U
  val hfma_valid = io.valid && io.out_eew === 1.U

  val swa = Seq(widen(sa(0), FType.S, FType.D, io.out_eew === 3.U && io.a_eew === 2.U))
  val swb = Seq(widen(sb(0), FType.S, FType.D, io.out_eew === 3.U && io.b_eew === 2.U))
  val swc = Seq(widen(sc(0), FType.S, FType.D, io.out_eew === 3.U && io.c_eew === 2.U))
  val hwa = Seq(
    widen(ha(0), FType.H, FType.S, io.out_eew === 2.U && io.a_eew === 1.U),
    widen(ha(2), FType.H, FType.S, io.out_eew === 2.U && io.a_eew === 1.U))
  val hwb = Seq(
    widen(hb(0), FType.H, FType.S, io.out_eew === 2.U && io.b_eew === 1.U),
    widen(hb(2), FType.H, FType.S, io.out_eew === 2.U && io.b_eew === 1.U))
  val hwc = Seq(
    widen(hc(0), FType.H, FType.S, io.out_eew === 2.U && io.c_eew === 1.U),
    widen(hc(2), FType.H, FType.S, io.out_eew === 2.U && io.c_eew === 1.U))

  val da_in = da.zip(swa).map(t => Mux(io.out_eew =/= io.a_eew, t._2, t._1))
  val db_in = db.zip(swb).map(t => Mux(io.out_eew =/= io.b_eew, t._2, t._1))
  val dc_in = dc.zip(swc).map(t => Mux(io.out_eew =/= io.c_eew, t._2, t._1))

  val sa_in = sa.zip(hwa).map(t => Mux(io.out_eew =/= io.a_eew, t._2, t._1))
  val sb_in = sb.zip(hwb).map(t => Mux(io.out_eew =/= io.b_eew, t._2, t._1))
  val sc_in = sc.zip(hwc).map(t => Mux(io.out_eew =/= io.c_eew, t._2, t._1))

  val ha_in = ha
  val hb_in = hb
  val hc_in = hc

  val s1_op = RegEnable(io.op, io.valid)
  val s1_frm = RegEnable(io.frm, io.valid)

  io.out := DontCare
  io.exc := DontCare

  (buildFP64.option((dfma_valid, FType.D, da_in, db_in, dc_in)) ++ Seq(
    (sfma_valid, FType.S, sa_in, sb_in, sc_in),
    (hfma_valid, FType.H, ha_in, hb_in, hc_in)
  )).foreach { case (fma_valid, ftype, a, b, c) => {
    val n = 64 / ftype.ieeeWidth
    val s1_valid = RegNext(fma_valid, false.B)
    val res = (0 until n).map { i =>
      val fma = Module(new MulAddRecFNPipe(depth-2, ftype.exp, ftype.sig))
      fma.io.validin      := s1_valid
      fma.io.op           := Mux(s1_valid, s1_op, 0.U)
      fma.io.roundingMode := Mux(s1_valid, s1_frm, 0.U)
      fma.io.detectTininess := hardfloat.consts.tininess_afterRounding
      fma.io.a := RegEnable(a(i), fma_valid)
      fma.io.b := RegEnable(Mux(io.addsub, 1.U << (ftype.ieeeWidth - 1), b(i)), fma_valid)
      fma.io.c := RegEnable(Mux(io.mul, (a(i) ^ b(i)) & (1.U << ftype.ieeeWidth), c(i)), fma_valid)

      val out = Pipe(fma.io.validout, ftype.ieee(fma.io.out), depth-4).bits
      val exc = Pipe(fma.io.validout, fma.io.exceptionFlags, depth-4).bits
      (out, Seq.fill(ftype.ieeeWidth / 8)(exc))
    }
    when (out_eew_pipe.bits === log2Ceil(ftype.ieeeWidth >> 3).U) {
      io.out := res.map(_._1).asUInt
      io.exc := res.map(_._2).flatten
    }
  }}
}

class MulAddRecFNPipeUnrounded(latency: Int, expWidth: Int, sigWidth: Int) extends Module {
  override def desiredName = s"MulAddRecFNPipeUnrounded_l${latency}_e${expWidth}_s${sigWidth}"
  require(latency<=2)

  val io = IO(new Bundle {
    val validin = Input(Bool())
    val op = Input(Bits(2.W))
    val a = Input(Bits((expWidth + sigWidth + 1).W))
    val b = Input(Bits((expWidth + sigWidth + 1).W))
    val c = Input(Bits((expWidth + sigWidth + 1).W))
    val roundingMode   = Input(UInt(3.W))
    val detectTininess = Input(UInt(1.W))
    val out = Output(new hardfloat.RawFloat(expWidth, sigWidth + 2))
    val invalidExc = Output(Bool())
    val validout = Output(Bool())
  })

  val mulAddRecFNToRaw_preMul = Module(new hardfloat.MulAddRecFNToRaw_preMul(expWidth, sigWidth))
  val mulAddRecFNToRaw_postMul = Module(new hardfloat.MulAddRecFNToRaw_postMul(expWidth, sigWidth))

  mulAddRecFNToRaw_preMul.io.op := io.op
  mulAddRecFNToRaw_preMul.io.a  := io.a
  mulAddRecFNToRaw_preMul.io.b  := io.b
  mulAddRecFNToRaw_preMul.io.c  := io.c

  val mulAddResult = (mulAddRecFNToRaw_preMul.io.mulAddA * mulAddRecFNToRaw_preMul.io.mulAddB) +& mulAddRecFNToRaw_preMul.io.mulAddC

  val valid_stage0 = Wire(Bool())

  val postmul_regs = if(latency>0) 1 else 0
  mulAddRecFNToRaw_postMul.io.fromPreMul   := Pipe(io.validin, mulAddRecFNToRaw_preMul.io.toPostMul, postmul_regs).bits
  mulAddRecFNToRaw_postMul.io.mulAddResult := Pipe(io.validin, mulAddResult, postmul_regs).bits
  mulAddRecFNToRaw_postMul.io.roundingMode := Pipe(io.validin, io.roundingMode, postmul_regs).bits
  valid_stage0                             := Pipe(io.validin, false.B, postmul_regs).valid

  val round_regs = if(latency==2) 1 else 0
  io.validout       := Pipe(valid_stage0, false.B, round_regs).valid
  io.out            := Pipe(valid_stage0, mulAddRecFNToRaw_postMul.io.rawOut, round_regs).bits
  io.invalidExc     := Pipe(valid_stage0, mulAddRecFNToRaw_postMul.io.invalidExc, round_regs).bits
}

class SegmentedFMAPipe(depth: Int, buildFP64: Boolean, mxFPFMA: Boolean)(implicit p: Parameters) extends FMAPipe()(p) {
  require (depth >= 4)

  val out_eew_pipe = Pipe(io.valid, io.out_eew, depth-1)
  val out_altfmt = Mux(io.widen, io.out_eew === 1.U, io.altfmt)
  val out_altfmt_pipe = Pipe(io.valid, out_altfmt, depth-1)
  val frm_pipe = Pipe(io.valid, io.frm, depth-1)

  val a_altfmt = Mux(io.out_eew === io.a_eew, out_altfmt, io.altfmt)
  val b_altfmt = Mux(io.out_eew === io.b_eew, out_altfmt, io.altfmt)
  val c_altfmt = Mux(io.out_eew === io.c_eew, out_altfmt, io.altfmt)

  val da = io.a.asTypeOf(Vec(1, UInt(64.W))).map(f => FType.D.recode(f))
  val db = io.b.asTypeOf(Vec(1, UInt(64.W))).map(f => FType.D.recode(f))
  val dc = io.c.asTypeOf(Vec(1, UInt(64.W))).map(f => FType.D.recode(f))
  val sa = io.a.asTypeOf(Vec(2, UInt(32.W))).map(f => FType.S.recode(f))
  val sb = io.b.asTypeOf(Vec(2, UInt(32.W))).map(f => FType.S.recode(f))
  val sc = io.c.asTypeOf(Vec(2, UInt(32.W))).map(f => FType.S.recode(f))
  val ha = io.a.asTypeOf(Vec(4, UInt(16.W))).map(f => FType.H.recode(f))
  val hb = io.b.asTypeOf(Vec(4, UInt(16.W))).map(f => FType.H.recode(f))
  val hc = io.c.asTypeOf(Vec(4, UInt(16.W))).map(f => FType.H.recode(f))
  val bf16a = io.a.asTypeOf(Vec(4, UInt(16.W))).map(f => FType.BF16.recode(f))
  val bf16b = io.b.asTypeOf(Vec(4, UInt(16.W))).map(f => FType.BF16.recode(f))
  val bf16c = io.c.asTypeOf(Vec(4, UInt(16.W))).map(f => FType.BF16.recode(f))
  val f8a = io.a.asTypeOf(Vec(8, UInt(8.W))).map(f => FType.E5M3.recode(fp8ToE5M3(f, io.altfmt)))
  val f8b = io.b.asTypeOf(Vec(8, UInt(8.W))).map(f => FType.E5M3.recode(fp8ToE5M3(f, io.altfmt)))
  val f8c = io.c.asTypeOf(Vec(8, UInt(8.W))).map(f => FType.E5M3.recode(fp8ToE5M3(f, io.altfmt)))

  def widen(in: UInt, inT: FType, outT: FType, active: Bool): UInt = {
    val widen = Module(new hardfloat.RecFNToRecFN(inT.exp, inT.sig, outT.exp, outT.sig))
    widen.io.in := Mux(active, in, 0.U)
    widen.io.roundingMode := io.frm
    widen.io.detectTininess := hardfloat.consts.tininess_afterRounding
    widen.io.out
  }

  val dfma_valid = io.valid && io.out_eew === 3.U
  val sfma_valid = io.valid && io.out_eew === 2.U
  val hfma_valid = io.valid && io.out_eew === 1.U && !out_altfmt
  val bf16fma_valid = io.valid && io.out_eew === 1.U && out_altfmt
  val f8fma_valid = io.valid && io.out_eew === 0.U

  val s1_op = RegEnable(io.op, io.valid)
  val s1_frm = RegEnable(io.frm, io.valid)
  val s1_out_altfmt = RegEnable(out_altfmt, io.valid)

  io.out := DontCare
  io.exc := DontCare

  val valid_signals = Map(
    FType.D -> dfma_valid,
    FType.S -> sfma_valid,
    FType.H -> hfma_valid,
    FType.BF16 -> bf16fma_valid,
    FType.E5M3 -> f8fma_valid
  )

  val recoded_in = Map(
    FType.D -> (da, db, dc),
    FType.S -> (sa, sb, sc),
    FType.H -> (ha, hb, hc),
    FType.BF16 -> (bf16a, bf16b, bf16c),
    FType.E5M3 -> (f8a, f8b, f8c)
  )

  val ftype_conditions = Map( // eew, ignore altfmt, altfmt
    FType.D -> (3.U, false.B, false.B),
    FType.S -> (2.U, false.B, false.B),
    FType.H -> (1.U, false.B, false.B),
    FType.BF16 -> (1.U, false.B, true.B),
    FType.E5M3 -> (0.U, true.B, false.B)
  )

  val ftype_used_for = Map(
    FType.D -> { if (mxFPFMA) Seq(FType.D, FType.S, FType.H, FType.BF16, FType.E5M3) else Seq(FType.D, FType.S, FType.H) },
    FType.S -> { if (mxFPFMA) Seq(FType.S, FType.H, FType.BF16, FType.E5M3) else Seq(FType.S, FType.H) },
    FType.H -> { if (mxFPFMA) Seq(FType.H, FType.E5M3) else Seq(FType.H) },
    FType.BF16 -> Seq(FType.BF16, FType.E5M3),
    FType.E5M3 -> Seq(FType.E5M3)
  )

  val fma_types = if (mxFPFMA) Seq( // Larger ones need to be spaced out correctly to easily select the right indeces when widening
    (if (buildFP64) FType.D else FType.S),
    FType.H,
    FType.BF16,
    FType.E5M3,
    FType.S,
    FType.H,
    FType.BF16,
    FType.E5M3
  ) else Seq(
    (if (buildFP64) FType.D else FType.S),
    FType.H,
    FType.S,
    FType.H
  )

  val ftype_exc_lanes = Map(
    FType.D -> 8,
    FType.S -> 4,
    FType.H -> 2,
    FType.BF16 -> 2,
    FType.E5M3 -> 1
  )

  val out = Map(
    FType.D -> Wire(Vec(1, UInt(64.W))),
    FType.S -> Wire(Vec(2, UInt(32.W))),
    FType.H -> Wire(Vec(4, UInt(16.W))),
    FType.BF16 -> Wire(Vec(4, UInt(16.W))),
    FType.E5M3 -> Wire(Vec(8, UInt(8.W)))
  )

  val exc = Map(
    FType.D -> Wire(Vec(8, UInt(5.W))),
    FType.S -> Wire(Vec(8, UInt(5.W))),
    FType.H -> Wire(Vec(8, UInt(5.W))),
    FType.BF16 -> Wire(Vec(8, UInt(5.W))),
    FType.E5M3 -> Wire(Vec(8, UInt(5.W)))
  )

  out.foreach{ case (key, value) => value := DontCare }
  exc.foreach{ case (key, value) => value := DontCare }

  val out_select = ftype_conditions.map { case (key, cond) =>
    key -> (out_eew_pipe.bits === cond._1 && (cond._2 || out_altfmt_pipe.bits === cond._3))
  }

  fma_types.foldLeft(Map(
    FType.D -> 0, FType.S -> 0, FType.H -> 0, FType.BF16 -> 0, FType.E5M3 -> 0
  )) { (counts, fma_type) => {
    val usedFor = ftype_used_for(fma_type)
    val fma_valid = usedFor.map(valid_signals(_)).foldLeft(0.U)(_|_).asBool
    val s1_valid = RegNext(fma_valid, false.B)
    val fma = Module(new MulAddRecFNPipeUnrounded(depth-2, fma_type.exp, fma_type.sig))
    fma.io.validin      := s1_valid
    fma.io.op           := Mux(s1_valid, s1_op, 0.U)
    fma.io.roundingMode := Mux(s1_valid, s1_frm, 0.U)
    fma.io.detectTininess := hardfloat.consts.tininess_afterRounding

    val a = Wire(UInt(fma_type.recodedWidth.W))
    val b = Wire(UInt(fma_type.recodedWidth.W))
    val c = Wire(UInt(fma_type.recodedWidth.W))

    a := DontCare
    b := DontCare
    c := DontCare

    usedFor.foreach { data_type =>
      val cond = ftype_conditions(data_type)
      val index = counts(data_type)

      val select_a = io.a_eew === cond._1 && (cond._2 || a_altfmt === cond._3)
      val in_a = recoded_in(data_type)._1(index)
      val wide_a = if (data_type != fma_type) {
        widen(in_a, data_type, fma_type, select_a)
      } else {
        in_a
      }
      when (select_a) {
        a := wide_a
      }

      val select_b = io.b_eew === cond._1 && (cond._2 || b_altfmt === cond._3)
      val in_b = recoded_in(data_type)._2(index)
      val wide_b = if (data_type != fma_type) {
        widen(in_b, data_type, fma_type, select_b)
      } else {
        in_b
      }
      when (select_b) {
        b := wide_b
      }

      val select_c = io.c_eew === cond._1 && (cond._2 || c_altfmt === cond._3)
      val in_c = recoded_in(data_type)._3(index)
      val wide_c = if (data_type != fma_type) {
        widen(in_c, data_type, fma_type, select_c)
      } else {
        in_c
      }
      when (select_c) {
        c := wide_c
      }

      val select_out = out_select(data_type)
      if (data_type == FType.E5M3){
        val (out_bits, exc_flags) = rawUnroundedToFp8(fma_type, fma.io.out, fma.io.invalidExc, out_altfmt_pipe.bits, frm_pipe.bits, false.B)

        when (select_out) {
          out(data_type)(index) := Pipe(fma.io.validout, out_bits, depth-4).bits
          exc(data_type).slice(index, index + ftype_exc_lanes(data_type)).foreach { _ := Pipe(fma.io.validout, exc_flags, depth-4).bits }
        }
      }
      else {
        val narrower = Module(new hardfloat.RoundAnyRawFNToRecFN(fma_type.exp, fma_type.sig + 2, data_type.exp, data_type.sig, 0))
        narrower.io.in := fma.io.out
        narrower.io.roundingMode := frm_pipe.bits
        narrower.io.detectTininess := hardfloat.consts.tininess_afterRounding
        narrower.io.invalidExc := fma.io.invalidExc
        narrower.io.infiniteExc := false.B

        when (select_out) {
          out(data_type)(index) := Pipe(fma.io.validout, data_type.ieee(narrower.io.out), depth-4).bits
          exc(data_type).slice(index, index + ftype_exc_lanes(data_type)).foreach { _ := Pipe(fma.io.validout, narrower.io.invalidExc, depth-4).bits }
        }
      }
    }

    fma.io.a := RegEnable(a, fma_valid)
    fma.io.b := RegEnable(Mux(io.addsub, 1.U << (fma_type.ieeeWidth - 1), b), fma_valid)
    fma.io.c := RegEnable(Mux(io.mul, (a ^ b) & (1.U << fma_type.ieeeWidth), c), fma_valid)
    counts.map { case (key, value) => (key, if (usedFor.contains(key)) value + 1 else value) }
  }}

  out_select.foreach { case (ftype, sel) =>
    when (sel) {
      io.out := out(ftype).asUInt
      io.exc := exc(ftype)
    }
  }
}

trait FMAFactory extends FunctionalUnitFactory {
  def depth: Int
  def base_insns = Seq(
    FADD.VV, FADD.VF, FSUB.VV, FSUB.VF, FRSUB.VF,
    FMUL.VV, FMUL.VF,
    FMACC.VV, FMACC.VF, FNMACC.VV, FNMACC.VF,
    FMSAC.VV, FMSAC.VF, FNMSAC.VV, FNMSAC.VF,
    FMADD.VV, FMADD.VF, FNMADD.VV, FNMADD.VF,
    FMSUB.VV, FMSUB.VF, FNMSUB.VV, FNMSUB.VF,
    FWADD.VV, FWADD.VF, FWSUB.VV, FWSUB.VF,
    FWADDW.VV, FWADDW.VF, FWSUBW.VV, FWSUBW.VF,
    FWMUL.VV, FWMUL.VF,
    FWMACC.VV, FWMACC.VF, FWNMACC.VV, FWNMACC.VF,
    FWMSAC.VV, FWMSAC.VF, FWNMSAC.VV, FWNMSAC.VF,
    FREDOSUM.VV, FREDUSUM.VV, FWREDOSUM.VV, FWREDUSUM.VV
  ).map(_.pipelined(depth)).map(_.restrictSEW(0,1,2,3)).flatten
}

case class SIMDFPFMAFactory(depth: Int, elementWiseFP64: Boolean = false, segmentedFPFMA: Boolean = false, mxFPFMA: Boolean) extends FMAFactory {
  def insns = if (elementWiseFP64) {
    base_insns.map { insn =>
      if (insn.lookup(SEW).value == 3 || (insn.lookup(SEW).value == 2 && insn.lookup(Wide2VD).value == 1)) {
        insn.elementWise
      } else {
        insn
      }
    }
  } else {
    base_insns
  }
  def generate(implicit p: Parameters) = new FPFMAPipe(depth, elementWiseFP64, segmentedFPFMA, mxFPFMA)(p)
}

class FPFMAPipe(depth: Int, elementwiseFP64: Boolean, segmentedFPFMA: Boolean, mxFPFMA: Boolean)(implicit p: Parameters) extends PipelinedFunctionalUnit(depth)(p) with HasFPUParameters {
  val supported_insns = SIMDFPFMAFactory(depth, elementwiseFP64, segmentedFPFMA, mxFPFMA).insns

  io.stall := false.B
  io.set_vxsat := false.B

  val ctrl = new VectorDecoder(io.pipe(0).bits, supported_insns, Seq(
    FPAdd, FPMul, FPSwapVdV2, FPFMACmd))

  val vs1_eew = io.pipe(0).bits.rvs1_eew
  val vs2_eew = io.pipe(0).bits.rvs2_eew
  val vd_eew  = io.pipe(0).bits.vd_eew
  val ctrl_widen_vs2 = vs2_eew =/= vd_eew
  val ctrl_widen_vs1 = vs1_eew =/= vd_eew
  val wmask = io.pipe(0).bits.wmask
  val altfmt = io.pipe(0).bits.altfmt

  val nTandemFMA = dLenB / 8

  val eidx = Mux(io.pipe(0).bits.acc, 0.U, io.pipe(0).bits.eidx)
  val one_bits = Mux1H(Seq(vd_eew === 3.U, vd_eew === 2.U, vd_eew === 1.U, vd_eew === 0.U),
                       Seq("h3FF0000000000000".U, "h3F8000003F800000".U, Mux(altfmt, "h3F803F803F803F80".U, "h3C003C003C003C00".U), Mux(altfmt, "h3C3C3C3C3C3C3C3C".U, "h3838383838383838".U)))
  val fmaCmd = ctrl.uint(FPFMACmd)

  val vec_rvs1 = io.pipe(0).bits.rvs1_data.asTypeOf(Vec(nTandemFMA, UInt(64.W)))
  val vec_rvs2 = io.pipe(0).bits.rvs2_data.asTypeOf(Vec(nTandemFMA, UInt(64.W)))
  val vec_rvd = io.pipe(0).bits.rvd_data.asTypeOf(Vec(nTandemFMA, UInt(64.W)))

  val pipe_out = (0 until nTandemFMA).map { i =>
    val fma_pipe = if (segmentedFPFMA) Module(new SegmentedFMAPipe(depth, i == 0 || !elementwiseFP64, mxFPFMA)) else Module(new TandemFMAPipe(depth, i == 0 || !elementwiseFP64, mxFPFMA))
    val widening_vs1_bits = Mux(vd_eew === 3.U,
      0.U(32.W) ## extractElem(io.pipe(0).bits.rvs1_data, 2.U, eidx + i.U)(31,0),
      Mux(vd_eew === 2.U,
        Cat(
          0.U(16.W),
          extractElem(io.pipe(0).bits.rvs1_data, 1.U, eidx + (i << 1).U + 1.U)(15,0),
          0.U(16.W),
          extractElem(io.pipe(0).bits.rvs1_data, 1.U, eidx + (i << 1).U + 0.U)(15,0)
        ), 
        Cat(
          0.U(8.W),
          extractElem(io.pipe(0).bits.rvs1_data, 0.U, eidx + (i << 2).U + 3.U)(7,0),
          0.U(8.W),
          extractElem(io.pipe(0).bits.rvs1_data, 0.U, eidx + (i << 2).U + 2.U)(7,0), 
          0.U(8.W),
          extractElem(io.pipe(0).bits.rvs1_data, 0.U, eidx + (i << 2).U + 1.U)(7,0),
          0.U(8.W),
          extractElem(io.pipe(0).bits.rvs1_data, 0.U, eidx + (i << 2).U + 0.U)(7,0)
        )
      )
    )
    val vs1_bits = Mux((i == 0 && elementwiseFP64).B && vd_eew === 3.U,
      Mux(ctrl_widen_vs1, 0.U(32.W) ## io.pipe(0).bits.rvs1_elem(31,0), io.pipe(0).bits.rvs1_elem),
      Mux(ctrl_widen_vs1, widening_vs1_bits, vec_rvs1(i))
    )

    val widening_vs2_bits = Mux(vd_eew === 3.U,
      0.U(32.W) ## extractElem(io.pipe(0).bits.rvs2_data, 2.U, eidx + i.U)(31,0),
      Mux(vd_eew === 2.U,
        Cat(
          0.U(16.W),
          extractElem(io.pipe(0).bits.rvs2_data, 1.U, eidx + (i << 1).U + 1.U)(15,0),
          0.U(16.W),
          extractElem(io.pipe(0).bits.rvs2_data, 1.U, eidx + (i << 1).U + 0.U)(15,0)
        ),
        Cat(
          0.U(8.W),
          extractElem(io.pipe(0).bits.rvs2_data, 0.U, eidx + (i << 2).U + 3.U)(7,0),
          0.U(8.W),
          extractElem(io.pipe(0).bits.rvs2_data, 0.U, eidx + (i << 2).U + 2.U)(7,0), 
          0.U(8.W),
          extractElem(io.pipe(0).bits.rvs2_data, 0.U, eidx + (i << 2).U + 1.U)(7,0),
          0.U(8.W),
          extractElem(io.pipe(0).bits.rvs2_data, 0.U, eidx + (i << 2).U + 0.U)(7,0)
        )
      )
    )
    val vs2_bits = Mux((i == 0 && elementwiseFP64).B && vd_eew === 3.U,
      Mux(ctrl_widen_vs2, 0.U(32.W) ## io.pipe(0).bits.rvs2_elem(31,0), io.pipe(0).bits.rvs2_elem),
      Mux(ctrl_widen_vs2, widening_vs2_bits, vec_rvs2(i))
    )

    val vs3_bits = Mux((i == 0 && elementwiseFP64).B && vd_eew === 3.U,
      io.pipe(0).bits.rvd_elem,
      vec_rvd(i)
    )

    fma_pipe.io.addsub := ctrl.bool(FPAdd) && !ctrl.bool(FPMul)
    fma_pipe.io.mul := ctrl.bool(FPMul) && !ctrl.bool(FPAdd)
    fma_pipe.io.out_eew := vd_eew
    fma_pipe.io.widen := ctrl_widen_vs1 || ctrl_widen_vs2
    fma_pipe.io.altfmt := altfmt

    // FMA
    when (ctrl.bool(FPMul) && ctrl.bool(FPAdd)) {
      fma_pipe.io.b     := vs1_bits
      fma_pipe.io.b_eew := vs1_eew
      when (ctrl.bool(FPSwapVdV2)) {
        fma_pipe.io.a     := vs3_bits
        fma_pipe.io.a_eew := vd_eew
        fma_pipe.io.c     := vs2_bits
        fma_pipe.io.c_eew := vs2_eew
      } .otherwise {
        fma_pipe.io.a     := vs2_bits
        fma_pipe.io.a_eew := vs2_eew
        fma_pipe.io.c     := vs3_bits
        fma_pipe.io.c_eew := vd_eew
      }
    }
    // Multiply
    .elsewhen (ctrl.bool(FPMul)) {
      fma_pipe.io.a     := vs2_bits
      fma_pipe.io.a_eew := vs2_eew
      fma_pipe.io.b     := vs1_bits
      fma_pipe.io.b_eew := vs1_eew
      fma_pipe.io.c     := 0.U
      fma_pipe.io.c_eew := vs2_eew
    }
    // Add type
    .elsewhen (ctrl.bool(FPAdd)) {
      fma_pipe.io.a     := vs2_bits
      fma_pipe.io.a_eew := vs2_eew
      fma_pipe.io.b     := one_bits
      fma_pipe.io.b_eew := vd_eew
      fma_pipe.io.c     := vs1_bits
      fma_pipe.io.c_eew := vs1_eew
    } .otherwise {
      fma_pipe.io.a     := 0.U
      fma_pipe.io.a_eew := 0.U
      fma_pipe.io.b     := 0.U
      fma_pipe.io.b_eew := 0.U
      fma_pipe.io.c     := 0.U
      fma_pipe.io.c_eew := 0.U
    }


    fma_pipe.io.valid := io.pipe(0).valid
    fma_pipe.io.frm := io.pipe(0).bits.frm
    fma_pipe.io.op := fmaCmd

    fma_pipe.io
  }

  io.write.valid := io.pipe(depth-1).valid
  io.write.bits.eg := io.pipe(depth-1).bits.wvd_eg
  io.write.bits.mask := FillInterleaved(8, io.pipe(depth-1).bits.wmask)
  io.write.bits.data := pipe_out.map(_.out).asUInt

  when (elementwiseFP64.B && io.pipe(depth-1).bits.vd_eew === 3.U) {
    io.write.bits.data := Fill(dLenB >> 3, pipe_out(0).out)
  }

  io.set_fflags.valid := io.write.valid
  io.set_fflags.bits := pipe_out.map(_.exc).flatten.zipWithIndex.map {
    case (e,i) => Mux(io.pipe(depth-1).bits.wmask(i), e, 0.U)
  }.reduce(_|_)
  io.scalar_write.valid := false.B
  io.scalar_write.bits := DontCare
}
