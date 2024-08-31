package saturn.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import chisel3.util.experimental.decode._
import saturn.common._
import saturn.insns._

class VFREC7(implicit p: Parameters) extends FPUModule()(p) {
  val io = IO(new Bundle {
    val rvs2_input = Input(UInt(64.W))
    val eew = Input(UInt(2.W))
    val frm = Input(UInt(3.W))
    val out = Output(UInt(64.W))
    val exc = Output(UInt(5.W))
  })

  val table = Seq(
    127, 125, 123, 121, 119, 117, 116, 114,
    112, 110, 109, 107, 105, 104, 102, 100,
    99, 97, 96, 94, 93, 91, 90, 88,
    87, 85, 84, 83, 81, 80, 79, 77,
    76, 75, 74, 72, 71, 70, 69, 68,
    66, 65, 64, 63, 62, 61, 60, 59,
    58, 57, 56, 55, 54, 53, 52, 51,
    50, 49, 48, 47, 46, 45, 44, 43,
    42, 41, 40, 40, 39, 38, 37, 36,
    35, 35, 34, 33, 32, 31, 31, 30,
    29, 28, 28, 27, 26, 25, 25, 24,
    23, 23, 22, 21, 21, 20, 19, 19,
    18, 17, 17, 16, 15, 15, 14, 14,
    13, 12, 12, 11, 11, 10, 9, 9,
    8, 8, 7, 7, 6, 5, 5, 4,
    4, 3, 3, 2, 2, 1, 1, 0)

  def count_leading_zeros(in: UInt): UInt = {
    PriorityEncoder(Reverse(in))
  }

  val rvs2_bits = io.rvs2_input
  val fTypes = Seq(FType.H, FType.S, FType.D)

  val eew_sel = (1 to 3).map(_.U === io.eew)
  val classify = Mux1H(eew_sel, fTypes.map(f => f.classify(f.recode(rvs2_bits(f.ieeeWidth-1,0)))))

  val dz = WireInit(false.B)
  val nv = WireInit(false.B)
  val of = WireInit(false.B)
  val nx = WireInit(false.B)

  val ret = Wire(UInt(64.W))
  ret := 0.U // it should not be possible to fall into this case
  when (classify(0)) { // -inf
    ret := Mux1H(eew_sel, fTypes.map(f => 1.U ## 0.U((f.ieeeWidth-1).W)))
  } .elsewhen (classify(7)) { // +inf
    ret := 0.U
  } .elsewhen (classify(3)) { // -0
    ret := Mux1H(eew_sel, fTypes.map(f => 1.U(1.W) ## ~(0.U((f.exp).W)) ## 0.U((f.sig-1).W)))
    dz := true.B
  } .elsewhen (classify(4)) { // +0
    ret := Mux1H(eew_sel, fTypes.map(f => 0.U(1.W) ## ~(0.U((f.exp).W)) ## 0.U((f.sig-1).W)))
    dz := true.B
  } .elsewhen (classify(8)) { // sNaN
    ret := Mux1H(eew_sel, fTypes.map(f => f.ieeeQNaN))
    nv := true.B
  } .elsewhen (classify(9)) { // qNaN
    ret := Mux1H(eew_sel, fTypes.map(f => f.ieeeQNaN))
  } .otherwise {
    val sub = classify(2) || classify(5)
    val exp = Mux1H(eew_sel, fTypes.map(f => (rvs2_bits >> (f.sig - 1))(f.exp-1,0)))
    val sig = Mux1H(eew_sel, fTypes.map(f => rvs2_bits(f.sig-2,0)))
    val sign = Mux1H(eew_sel, fTypes.map(f => rvs2_bits(f.ieeeWidth-1)))

    val norm_exp = WireInit(exp)
    val norm_sig = WireInit(sig)
    val round_abnormal = WireInit(false.B)

    when (sub) {
      val leading_zeros = Mux1H(eew_sel, fTypes.map(f => count_leading_zeros(sig(f.sig-2,0))))

      val exp_new = exp - leading_zeros
      val sig_new = (sig << (leading_zeros +& 1.U)) & Mux1H(eew_sel, fTypes.map(f => ~(0.U((f.sig-1).W))))
      norm_exp := exp_new
      norm_sig := sig_new

      when (exp_new =/= 0.U && ~exp_new =/= 0.U) {
        round_abnormal := true.B
        when (io.frm === 1.U ||
          (io.frm === 2.U && !sign) ||
          (io.frm === 3.U && sign)) {
          ret := Mux1H(eew_sel, fTypes.map(f => (sign << (f.sig + f.exp - 1)) | (~(0.U(f.exp.W)) << (f.sig - 1)))) - 1.U
        } .otherwise {
          ret := Mux1H(eew_sel, fTypes.map(f => (sign << (f.sig + f.exp - 1)) | (~(0.U(f.exp.W)) << (f.sig - 1))))
        }
      }
    }

    when (!round_abnormal) {
      val idx = Mux1H(eew_sel, fTypes.map(f => norm_sig >> (f.sig - 1 - 7)))(6,0)
      val lookup = VecInit(table.map(_.U(7.W)))(idx)
      val default_out_sig = Mux1H(eew_sel, fTypes.map(f => lookup << (f.sig - 1 - 7)))
      val biases = fTypes.map(f => (1 << (f.exp - 1)) - 1)
      val default_out_exp = Mux1H(eew_sel, fTypes.zip(biases).map { case (f, b) =>
        (2 * b).U + ~norm_exp
      })

      val out_sig = WireInit(default_out_sig)
      val out_exp = WireInit(default_out_exp)

      when (default_out_exp === 0.U || (~default_out_exp === 0.U)) {
        val sig_new = (default_out_sig >> 1) | Mux1H(eew_sel, fTypes.map(f => 1.U << (f.sig - 1 - 1)))
        out_sig := sig_new
        when (~default_out_exp === 0.U) {
          out_sig := sig_new >> 1;
          out_exp := 0.U
        }
      }
      ret := Mux1H(eew_sel, fTypes.map(f => sign ## out_exp(f.exp-1,0) ## out_sig(f.sig-2,0)))
    }

    when (round_abnormal) {
      of := true.B
      nx := true.B
    }
  }

  io.out := Mux1H(eew_sel, fTypes.map(f => Fill(64 / f.ieeeWidth, ret(f.ieeeWidth-1,0))))
  io.exc := nv ## dz ## of ## false.B ## nx
}


class VFRSQRT7(implicit p: Parameters) extends FPUModule()(p) {
  val io = IO(new Bundle {
    val rvs2_input = Input(UInt(64.W))
    val eew = Input(UInt(2.W))
    val out = Output(UInt(64.W))
    val exc = Output(UInt(5.W))
  })

  val table = Seq(
    52, 51, 50, 48, 47, 46, 44, 43,
    42, 41, 40, 39, 38, 36, 35, 34,
    33, 32, 31, 30, 30, 29, 28, 27,
    26, 25, 24, 23, 23, 22, 21, 20,
    19, 19, 18, 17, 16, 16, 15, 14,
    14, 13, 12, 12, 11, 10, 10, 9,
    9, 8, 7, 7, 6, 6, 5, 4,
    4, 3, 3, 2, 2, 1, 1, 0,
    127, 125, 123, 121, 119, 118, 116, 114,
    113, 111, 109, 108, 106, 105, 103, 102,
    100, 99, 97, 96, 95, 93, 92, 91,
    90, 88, 87, 86, 85, 84, 83, 82,
    80, 79, 78, 77, 76, 75, 74, 73,
    72, 71, 70, 70, 69, 68, 67, 66,
    65, 64, 63, 63, 62, 61, 60, 59,
    59, 58, 57, 56, 56, 55, 54, 53
  )

  def count_leading_zeros(in: UInt): UInt = {
    PriorityEncoder(Reverse(in))
  }


  val rvs2_bits = io.rvs2_input
  val fTypes = Seq(FType.H, FType.S, FType.D)

  val eew_sel = (1 to 3).map(_.U === io.eew)
  val classify = Mux1H(eew_sel, fTypes.map(f => f.classify(f.recode(rvs2_bits(f.ieeeWidth-1,0)))))

  val dz = WireInit(false.B)
  val nv = WireInit(false.B)
  val of = WireInit(false.B)
  val nx = WireInit(false.B)

  val ret = Wire(UInt(64.W))
  ret := 0.U // it should not be possible to fall into this case

  when (classify(0) || classify(1) || classify(2) || classify(8)) { // -inf, -normal, -subnormal, sNaN
    nv := true.B
    ret := Mux1H(eew_sel, fTypes.map(f => f.ieeeQNaN))
  } .elsewhen (classify(9)) { // qNaN
    ret := Mux1H(eew_sel, fTypes.map(f => f.ieeeQNaN))
  } .elsewhen (classify(3)) { // -0
    ret := Mux1H(eew_sel, fTypes.map(f => 1.U(1.W) ## ~(0.U((f.exp).W)) ## 0.U((f.sig-1).W)))
    dz := true.B
  } .elsewhen (classify(4)) { // +0
    ret := Mux1H(eew_sel, fTypes.map(f => 0.U(1.W) ## ~(0.U((f.exp).W)) ## 0.U((f.sig-1).W)))
    dz := true.B
  } .elsewhen (classify(7)) { // +inf
    ret := 0.U
  } .otherwise {
    val sub = classify(5)

    val exp = Mux1H(eew_sel, fTypes.map(f => (rvs2_bits >> (f.sig - 1))(f.exp-1,0)))
    val sig = Mux1H(eew_sel, fTypes.map(f => rvs2_bits(f.sig-2,0)))
    val sign = Mux1H(eew_sel, fTypes.map(f => rvs2_bits(f.ieeeWidth-1)))

    val norm_exp = Wire(UInt((1+fTypes.map(_.exp).max).W))
    norm_exp := exp
    val norm_sig = WireInit(sig)

    when (sub) {
      val leading_zeros = Mux1H(eew_sel, fTypes.map(f => count_leading_zeros(sig(f.sig-2,0))))
      val exp_new = (0.U(1.W) ## exp) - leading_zeros
      val sig_new = (sig << (leading_zeros +& 1.U)) & Mux1H(eew_sel, fTypes.map(f => ~(0.U((f.sig-1).W))))
      norm_exp := exp_new
      norm_sig := sig_new
    }

    val idx = ((norm_exp(0) << 6) | Mux1H(eew_sel, fTypes.map(f => norm_sig >> (f.sig - 1 - 7 + 1))))(6,0)
    val lookup = VecInit(table.map(_.U(7.W)))(idx)
    val out_sig = Mux1H(eew_sel, fTypes.map(f => lookup << (f.sig - 1 - 7)))
    val biases = fTypes.map(f => (1 << (f.exp - 1)) - 1)
    val out_exp = Mux1H(eew_sel, fTypes.zip(biases).map { case (f, b) =>
      val bias3 = ((3 * b).S((f.exp + 2).W) - norm_exp.asSInt - 1.S).asUInt
      bias3 >> 1
    })

    ret := Mux1H(eew_sel, fTypes.map(f => sign ## out_exp(f.exp-1,0) ## out_sig(f.sig-2,0)))
  }
  io.out := Mux1H(eew_sel, fTypes.map(f => Fill(64 / f.ieeeWidth, ret(f.ieeeWidth-1,0))))
  io.exc := nv ## dz ## of ## false.B ## nx

}

case object FPDivSqrtFactory extends FunctionalUnitFactory {
  def insns = Seq(
    FDIV.VV, FDIV.VF,
    FRDIV.VF,
    FSQRT_V,
    FRSQRT7_V,
    FREC7_V,
    FCLASS_V
  ).map(_.elementWise.iterative)

  def generate(implicit p: Parameters) = new FPDivSqrt()(p)
}

class FPDivSqrt(implicit p: Parameters) extends IterativeFunctionalUnit()(p) with HasFPUParameters {
  val supported_insns = FPDivSqrtFactory.insns
  io.set_vxsat := false.B

  val divSqrt = Module(new hardfloat.DivSqrtRecF64)
  val divSqrt16 = Module(new hardfloat.DivSqrtRecFN_small(FType.H.exp, FType.H.sig, 0))

  val accept_inst = new VectorDecoder(
    io.iss.op.funct3, io.iss.op.funct6, io.iss.op.rs1, io.iss.op.rs2,
    supported_insns,
    Seq(FPSwapVdV2))

  val ctrl = new VectorDecoder(
    op.funct3, op.funct6, op.rs1, op.rs2,
    supported_insns,
    Seq(FPSwapVdV2))

  val ctrl_isDiv = io.iss.op.opff6.isOneOf(OPFFunct6.fdiv, OPFFunct6.frdiv)
  val divSqrt_ready = (ctrl_isDiv && divSqrt.io.inReady_div) || (!ctrl_isDiv && divSqrt.io.inReady_sqrt)
  val divSqrt16_ready = divSqrt16.io.inReady

  val div_op = op.opff6.isOneOf(OPFFunct6.fdiv, OPFFunct6.frdiv)

  val rvs2_bits = op.rvs2_elem
  val rvs1_bits = op.rvs1_elem

  divSqrt.io.detectTininess := hardfloat.consts.tininess_afterRounding
  divSqrt.io.roundingMode := op.frm
  divSqrt16.io.detectTininess := hardfloat.consts.tininess_afterRounding
  divSqrt16.io.roundingMode := op.frm

  val iss_fire_pipe = Reg(Bool())
  iss_fire_pipe := io.iss.valid

  divSqrt.io.inValid := iss_fire_pipe && !(op.rvd_eew === 1.U) && (div_op || (op.opff6 === OPFFunct6.funary1 && op.rs1 === 0.U))
  divSqrt.io.sqrtOp := !div_op
  divSqrt16.io.inValid := iss_fire_pipe && (op.rvd_eew === 1.U) && (div_op || (op.opff6 === OPFFunct6.funary1 && op.rs1 === 0.U))
  divSqrt16.io.sqrtOp := !div_op

  io.hazard.valid := valid
  io.hazard.bits.eg := op.wvd_eg
  io.hazard.bits.vat := op.vat

  when (op.rvs1_eew === 3.U) {
    divSqrt.io.a := Mux(ctrl.bool(FPSwapVdV2) && div_op, FType.D.recode(rvs1_bits), FType.D.recode(rvs2_bits))
    divSqrt.io.b := Mux(ctrl.bool(FPSwapVdV2) || !div_op, FType.D.recode(rvs2_bits), FType.D.recode(rvs1_bits))
  } .otherwise {
    val narrow_rvs2_bits = rvs2_bits(31,0)
    val narrow_rvs1_bits = rvs1_bits(31,0)
    val widen = Seq(FType.S.recode(narrow_rvs2_bits), FType.S.recode(narrow_rvs1_bits)).zip(
      Seq.fill(2)(Module(new hardfloat.RecFNToRecFN(8, 24, 11, 53)))).map { case(input, upconvert) =>
      upconvert.io.in := input
      upconvert.io.roundingMode := op.frm
      upconvert.io.detectTininess := hardfloat.consts.tininess_afterRounding
      upconvert
    }

    divSqrt.io.a := Mux(ctrl.bool(FPSwapVdV2) && div_op, widen(1).io.out, widen(0).io.out)
    divSqrt.io.b := Mux(ctrl.bool(FPSwapVdV2) || !div_op, widen(0).io.out, widen(1).io.out)
  }

  divSqrt16.io.a := Mux(ctrl.bool(FPSwapVdV2) && div_op, FType.H.recode(rvs1_bits), FType.H.recode(rvs2_bits))
  divSqrt16.io.b := Mux(ctrl.bool(FPSwapVdV2) || !div_op, FType.H.recode(rvs2_bits), FType.H.recode(rvs1_bits))

  val divSqrt_out_valid = divSqrt.io.outValid_div || divSqrt.io.outValid_sqrt
  val divSqrt16_out_valid = divSqrt16.io.outValid_div || divSqrt16.io.outValid_sqrt

  val narrow = Module(new hardfloat.RecFNToRecFN(11, 53, 8, 24))
  narrow.io.roundingMode := op.frm
  narrow.io.detectTininess := hardfloat.consts.tininess_afterRounding
  narrow.io.in := divSqrt.io.out

  val divSqrt_out = Mux(op.vd_eew === 3.U, FType.D.ieee(divSqrt.io.out), Fill(2, FType.S.ieee(narrow.io.out)))
  val divSqrt16_out = FType.H.ieee(divSqrt16.io.out)

  val divsqrt_exc = Reg(UInt(5.W))
  val divsqrt_reg = Reg(UInt(64.W))
  val divsqrt_valid = RegInit(false.B)

  when (divSqrt_out_valid) {
    divsqrt_exc := divSqrt.io.exceptionFlags
    divsqrt_reg := divSqrt_out
    divsqrt_valid := true.B
  }
  when (divSqrt16_out_valid) {
    divsqrt_exc := divSqrt16.io.exceptionFlags
    divsqrt_reg := divSqrt16_out
    divsqrt_valid := true.B
  }
  when (io.write.fire) {
    divsqrt_valid := false.B
  }

  // vfclass instruction
  val gen_vfclass = Seq(FType.H, FType.S, FType.D).zipWithIndex.map { case(fType, i) =>
    Fill(2, Cat(0.U((fType.ieeeWidth-10).W), fType.classify(fType.recode(rvs2_bits(fType.ieeeWidth-1,0)))))
  }

  val vfclass_inst = op.opff6.isOneOf(OPFFunct6.funary1) && op.rs1 === 16.U
  val vfrsqrt7_inst = op.opff6.isOneOf(OPFFunct6.funary1) && op.rs1 === 4.U
  val vfrec7_inst = op.opff6.isOneOf(OPFFunct6.funary1) && op.rs1 === 5.U

  // Reciprocal Sqrt Approximation
  val recSqrt7 = Module(new VFRSQRT7)
  recSqrt7.io.rvs2_input := Mux(valid && vfrsqrt7_inst, rvs2_bits, 0.U)
  recSqrt7.io.eew := op.rvs2_eew

  // Reciprocal Approximation
  val rec7 = Module(new VFREC7)
  rec7.io.rvs2_input := Mux(valid && vfrec7_inst, rvs2_bits, 0.U)
  rec7.io.eew := op.rvs2_eew
  rec7.io.frm := op.frm

  val out = Mux1H(
    Seq(vfclass_inst, vfrsqrt7_inst, vfrec7_inst, divsqrt_valid),
    Seq(Mux1H(Seq(op.rvs2_eew === 3.U, op.rvs2_eew === 2.U, op.rvs2_eew === 1.U), Seq(gen_vfclass(2), gen_vfclass(1), gen_vfclass(0))), recSqrt7.io.out, rec7.io.out, divsqrt_reg)
  )(63,0)

  io.write.valid := valid && ((vfclass_inst || vfrsqrt7_inst || vfrec7_inst) || divsqrt_valid)
  io.write.bits.eg := op.wvd_eg
  io.write.bits.mask := FillInterleaved(8, op.wmask)
  io.write.bits.data := Fill(dLenB >> 3, out)
  io.stall := valid
  last := io.write.fire

  io.set_fflags.valid := divsqrt_valid || (vfrsqrt7_inst && io.write.fire) || (vfrec7_inst && io.write.fire)
  io.set_fflags.bits := Mux(divsqrt_valid, divsqrt_exc,
    (recSqrt7.io.exc & Fill(5, vfrsqrt7_inst)) | (rec7.io.exc & Fill(5, vfrec7_inst)))

  io.scalar_write.valid := false.B
  io.scalar_write.bits := DontCare

  io.acc := false.B
  io.tail := false.B
}
