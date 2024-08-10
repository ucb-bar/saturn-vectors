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

  val default_bits = BitPat("b0000000")
  val vfrec7_table = Seq(
    127, 125, 123, 121, 119, 117, 116, 114, 112, 110, // 0-9
    109, 107, 105, 104, 102, 100, 99, 97, 96, 94,
    93, 91, 90, 88, 87, 85, 84, 83, 81, 80,
    79, 77, 76, 75, 74, 72, 71, 70, 69, 68,
    66, 65, 64, 63, 62, 61, 60, 59, 58, 57,
    56, 55, 54, 53, 52, 51, 50, 49, 48, 47,
    46, 45, 44, 43, 42, 41, 40, 40, 39, 38,
    37, 36, 35, 35, 34, 33, 32, 31, 31, 30,
    29, 28, 28, 27, 26, 25, 25, 24, 23, 23,
    22, 21, 21, 20, 19, 19, 18, 17, 17, 16,
    15, 15, 14, 14, 13, 12, 12, 11, 11, 10,
    9, 9, 8, 8, 7, 7, 6, 5, 5, 4,
    4, 3, 3, 2, 2, 1, 1, 0)

  def count_leading_zeros(in: UInt, width: Int): UInt = {
    PriorityEncoder(Reverse(in))
  }

  val rvs2_bits = io.rvs2_input
  val fTypes = Seq(FType.H, FType.S, FType.D)
  val is_negative = Wire(Vec(3, Bool()))
  val is_pos_zero = Wire(Vec(3, Bool()))
  val is_neg_zero = Wire(Vec(3, Bool()))
  val is_pos_inf = Wire(Vec(3, Bool()))
  val is_neg_inf = Wire(Vec(3, Bool()))
  val is_sNaN = Wire(Vec(3, Bool()))
  val is_qNaN = Wire(Vec(3, Bool()))
  val is_subnormal = Wire(Vec(3, Bool()))
  val more_than_1_leading_sign_zero = Wire(Vec(3, Bool()))
  val output_sign = Wire(Vec(3, UInt(1.W)))
  val output_significand_index = Wire(Vec(3, UInt(7.W)))
  val output_exponent = fTypes.map(f => Wire(UInt(f.exp.W)))
  val is_subnormal_output = Wire(Vec(3, Bool()))

  val mux_select = (1 to 3).map(_.U === io.eew).asUInt

  fTypes.zipWithIndex.foreach { case(fType, i) =>
    val rvs2_classify = fType.classify(fType.recode(rvs2_bits(fType.ieeeWidth-1, 0)))
    is_negative(i) := rvs2_classify(2,0).orR
    is_pos_zero(i) := rvs2_classify(4)
    is_neg_zero(i) := rvs2_classify(3)
    is_pos_inf(i) := rvs2_classify(7)
    is_neg_inf(i) := rvs2_classify(0)
    is_sNaN(i) := rvs2_classify(8)
    is_qNaN(i) := rvs2_classify(9)
    is_subnormal(i) := rvs2_classify(2) || rvs2_classify(5)

    val num_leading_significand_zeros = count_leading_zeros(rvs2_bits(fType.sig-1, 0), fType.sig)
    more_than_1_leading_sign_zero(i) := num_leading_significand_zeros > 1.U

    val is_normal = rvs2_classify(1) || rvs2_classify(6)

    val normalized_exponent = Wire(UInt(fType.exp.W))
    val normalized_significand = Wire(UInt((fType.sig - 1).W))
    normalized_exponent := Mux(is_normal,
                               rvs2_bits(fType.ieeeWidth-2, fType.ieeeWidth-2-fType.exp+1),
                               (0.U - num_leading_significand_zeros))
    normalized_significand := Mux(is_normal,
                                  rvs2_bits(fType.sig-2, 0),
                                  rvs2_bits(fType.sig-2, 0) << (1.U - normalized_exponent))

    output_exponent(i) := (2.U * ((1 << (fType.exp - 1)) - 1).U) - 1.U - normalized_exponent

    is_subnormal_output(i) := (output_exponent(i) === 0.U) || (output_exponent(i).asSInt === -1.S)
    output_significand_index(i) := normalized_significand(fType.sig - 2, fType.sig - 8)
    output_sign(i) := rvs2_bits(fType.ieeeWidth - 1)
  }

  val significand_bits = WireInit(VecInit(vfrec7_table.map(_.U(7.W)))(Mux1H(mux_select, output_significand_index)))
  dontTouch(output_significand_index)
  dontTouch(significand_bits)

  val out_bits = fTypes.zipWithIndex.map{ case(fType, i) =>
    val out_exponent = Wire(UInt(fType.exp.W))
    val out_significand = Wire(UInt((fType.sig - 1).W))

    dontTouch(out_exponent)
    dontTouch(out_significand)

    when (is_subnormal_output(i)) {
      out_exponent := 0.U
      when (output_exponent(i) === 0.U) {
        out_significand := 1.U ## significand_bits ## 0.U((fType.sig - 9).W)
      } .otherwise {
        out_significand := Cat(Cat(1.U, significand_bits), 0.U((fType.sig - 9).W)) >> 1
      }
    }
    // special case where the input is a very small subnormal and the output depends on the rounding mode
    .elsewhen (is_subnormal(i) && more_than_1_leading_sign_zero(i)) {
      when ((output_sign(i).asBool && (io.frm === 3.U || io.frm === 1.U)) || (!output_sign(i) && (io.frm === 2.U || io.frm === 1.U))) {
        out_exponent := ~(1.U(fType.exp.W))
        out_significand := ~(0.U(fType.sig.W))
      } .otherwise {
        out_exponent := ~(0.U(fType.exp.W))
        out_significand := 0.U
      }
    }
    .otherwise {
      out_exponent := output_exponent(i)
      out_significand := Cat(significand_bits, 0.U((fType.sig - 8).W))
    }

    Fill(64 / fType.ieeeWidth, output_sign(i) ## out_exponent ## out_significand)
  }

  val eew_sel = io.eew - 1.U

  when (is_sNaN(eew_sel) || is_qNaN(eew_sel)) {
    io.out := Mux1H(mux_select, fTypes.map{ fType => Fill(64 / fType.ieeeWidth, fType.ieeeQNaN) })
  } .elsewhen (is_pos_inf(eew_sel)) {
    io.out := 0.U
  } .elsewhen (is_neg_inf(eew_sel)) {
    io.out := Mux1H(mux_select, Seq("h8000800080008000".U, "h8000000080000000".U, "h8000000000000000".U)) 
  } .elsewhen (is_pos_zero(eew_sel)) {
    io.out := Mux1H(mux_select, Seq("h7C007C007C007C00".U, "h7F8000007F800000".U, "h7FF0000000000000".U))
  } .elsewhen (is_neg_zero(eew_sel)) {
    io.out := Mux1H(mux_select, Seq("hFC00FC00FC00FC00".U, "hFF800000FF800000".U, "hFFF0000000000000".U))
  } .otherwise {
    io.out := Mux1H(mux_select, out_bits)
  }

  val NV = is_negative(eew_sel) || is_sNaN(eew_sel)
  val DZ = is_pos_zero(eew_sel) || is_neg_zero(eew_sel)
  val OF = is_subnormal(eew_sel) && more_than_1_leading_sign_zero(eew_sel)
  val NX = is_subnormal(eew_sel) && more_than_1_leading_sign_zero(eew_sel)

  io.exc := Seq(NV, DZ, OF, false.B, NX).asUInt
}


class VFRSQRT7(implicit p: Parameters) extends FPUModule()(p) {
  val io = IO(new Bundle {
    val rvs2_input = Input(UInt(64.W))
    val eew = Input(UInt(2.W))
    val out = Output(UInt(64.W))
    val exc = Output(UInt(5.W))
  })

  val default_bits = BitPat("b0000000")
  val vfrsqrt7_table = Seq(
    (BitPat("b0000000") , BitPat(52.U(7.W))),
    (BitPat("b0000001") , BitPat(51.U(7.W))),
    (BitPat("b0000010") , BitPat(50.U(7.W))),
    (BitPat("b0000011") , BitPat(48.U(7.W))),
    (BitPat("b0000100") , BitPat(47.U(7.W))),
    (BitPat("b0000101") , BitPat(46.U(7.W))),
    (BitPat("b0000110") , BitPat(44.U(7.W))),
    (BitPat("b0000111") , BitPat(43.U(7.W))),
    (BitPat("b0001000") , BitPat(42.U(7.W))),
    (BitPat("b0001001") , BitPat(41.U(7.W))),
    (BitPat("b0001010") , BitPat(40.U(7.W))),
    (BitPat("b0001011") , BitPat(39.U(7.W))),
    (BitPat("b0001100") , BitPat(38.U(7.W))),
    (BitPat("b0001101") , BitPat(36.U(7.W))),
    (BitPat("b0001110") , BitPat(35.U(7.W))),
    (BitPat("b0001111") , BitPat(34.U(7.W))),
    (BitPat("b0010000") , BitPat(33.U(7.W))),
    (BitPat("b0010001") , BitPat(32.U(7.W))),
    (BitPat("b0010010") , BitPat(31.U(7.W))),
    (BitPat("b0010011") , BitPat(30.U(7.W))),
    (BitPat("b0010100") , BitPat(30.U(7.W))),
    (BitPat("b0010101") , BitPat(29.U(7.W))),
    (BitPat("b0010110") , BitPat(28.U(7.W))),
    (BitPat("b0010111") , BitPat(27.U(7.W))),
    (BitPat("b0011000") , BitPat(26.U(7.W))),
    (BitPat("b0011001") , BitPat(25.U(7.W))),
    (BitPat("b0011010") , BitPat(24.U(7.W))),
    (BitPat("b0011011") , BitPat(23.U(7.W))),
    (BitPat("b0011100") , BitPat(23.U(7.W))),
    (BitPat("b0011101") , BitPat(22.U(7.W))),
    (BitPat("b0011110") , BitPat(21.U(7.W))),
    (BitPat("b0011111") , BitPat(20.U(7.W))),
    (BitPat("b0100000") , BitPat(19.U(7.W))),
    (BitPat("b0100001") , BitPat(19.U(7.W))),
    (BitPat("b0100010") , BitPat(18.U(7.W))),
    (BitPat("b0100011") , BitPat(17.U(7.W))),
    (BitPat("b0100100") , BitPat(16.U(7.W))),
    (BitPat("b0100101") , BitPat(16.U(7.W))),
    (BitPat("b0100110") , BitPat(15.U(7.W))),
    (BitPat("b0100111") , BitPat(14.U(7.W))),
    (BitPat("b0101000") , BitPat(14.U(7.W))),
    (BitPat("b0101001") , BitPat(13.U(7.W))),
    (BitPat("b0101010") , BitPat(12.U(7.W))),
    (BitPat("b0101011") , BitPat(12.U(7.W))),
    (BitPat("b0101100") , BitPat(11.U(7.W))),
    (BitPat("b0101101") , BitPat(10.U(7.W))),
    (BitPat("b0101110") , BitPat(10.U(7.W))),
    (BitPat("b0101111") , BitPat(9.U(7.W))),
    (BitPat("b0110000") , BitPat(9.U(7.W))),
    (BitPat("b0110001") , BitPat(8.U(7.W))),
    (BitPat("b0110010") , BitPat(7.U(7.W))),
    (BitPat("b0110011") , BitPat(7.U(7.W))),
    (BitPat("b0110100") , BitPat(6.U(7.W))),
    (BitPat("b0110101") , BitPat(6.U(7.W))),
    (BitPat("b0110110") , BitPat(5.U(7.W))),
    (BitPat("b0110111") , BitPat(4.U(7.W))),
    (BitPat("b0111000") , BitPat(4.U(7.W))),
    (BitPat("b0111001") , BitPat(3.U(7.W))),
    (BitPat("b0111010") , BitPat(3.U(7.W))),
    (BitPat("b0111011") , BitPat(2.U(7.W))),
    (BitPat("b0111100") , BitPat(2.U(7.W))),
    (BitPat("b0111101") , BitPat(1.U(7.W))),
    (BitPat("b0111110") , BitPat(1.U(7.W))),
    (BitPat("b0111111") , BitPat(0.U(7.W))),
    (BitPat("b1000001") , BitPat(127.U(7.W))),
    (BitPat("b1000010") , BitPat(125.U(7.W))),
    (BitPat("b1000011") , BitPat(123.U(7.W))),
    (BitPat("b1000100") , BitPat(121.U(7.W))),
    (BitPat("b1000101") , BitPat(119.U(7.W))),
    (BitPat("b1000110") , BitPat(118.U(7.W))),
    (BitPat("b1000111") , BitPat(116.U(7.W))),
    (BitPat("b1001000") , BitPat(114.U(7.W))),
    (BitPat("b1001001") , BitPat(113.U(7.W))),
    (BitPat("b1001010") , BitPat(111.U(7.W))),
    (BitPat("b1001011") , BitPat(109.U(7.W))),
    (BitPat("b1001100") , BitPat(108.U(7.W))),
    (BitPat("b1001101") , BitPat(106.U(7.W))),
    (BitPat("b1001110") , BitPat(105.U(7.W))),
    (BitPat("b1001111") , BitPat(103.U(7.W))),
    (BitPat("b1010000") , BitPat(102.U(7.W))),
    (BitPat("b1010001") , BitPat(100.U(7.W))),
    (BitPat("b1010010") , BitPat(99.U(7.W))),
    (BitPat("b1010011") , BitPat(97.U(7.W))),
    (BitPat("b1010100") , BitPat(96.U(7.W))),
    (BitPat("b1010101") , BitPat(95.U(7.W))),
    (BitPat("b1010110") , BitPat(93.U(7.W))),
    (BitPat("b1010111") , BitPat(92.U(7.W))),
    (BitPat("b1011000") , BitPat(91.U(7.W))),
    (BitPat("b1011001") , BitPat(90.U(7.W))),
    (BitPat("b1011010") , BitPat(88.U(7.W))),
    (BitPat("b1011011") , BitPat(87.U(7.W))),
    (BitPat("b1011100") , BitPat(86.U(7.W))),
    (BitPat("b1011101") , BitPat(85.U(7.W))),
    (BitPat("b1011110") , BitPat(84.U(7.W))),
    (BitPat("b1011111") , BitPat(83.U(7.W))),
    (BitPat("b1100000") , BitPat(82.U(7.W))),
    (BitPat("b1100001") , BitPat(80.U(7.W))),
    (BitPat("b1100010") , BitPat(79.U(7.W))),
    (BitPat("b1100011") , BitPat(78.U(7.W))),
    (BitPat("b1100100") , BitPat(77.U(7.W))),
    (BitPat("b1100101") , BitPat(76.U(7.W))),
    (BitPat("b1100110") , BitPat(75.U(7.W))),
    (BitPat("b1100111") , BitPat(74.U(7.W))),
    (BitPat("b1101000") , BitPat(73.U(7.W))),
    (BitPat("b1101001") , BitPat(72.U(7.W))),
    (BitPat("b1101010") , BitPat(71.U(7.W))),
    (BitPat("b1101011") , BitPat(70.U(7.W))),
    (BitPat("b1101100") , BitPat(70.U(7.W))),
    (BitPat("b1101101") , BitPat(69.U(7.W))),
    (BitPat("b1101110") , BitPat(68.U(7.W))),
    (BitPat("b1101111") , BitPat(67.U(7.W))),
    (BitPat("b1110000") , BitPat(66.U(7.W))),
    (BitPat("b1110001") , BitPat(65.U(7.W))),
    (BitPat("b1110010") , BitPat(64.U(7.W))),
    (BitPat("b1110011") , BitPat(63.U(7.W))),
    (BitPat("b1110100") , BitPat(62.U(7.W))),
    (BitPat("b1110101") , BitPat(61.U(7.W))),
    (BitPat("b1110110") , BitPat(60.U(7.W))),
    (BitPat("b1110111") , BitPat(59.U(7.W))),
    (BitPat("b1111000") , BitPat(59.U(7.W))),
    (BitPat("b1111001") , BitPat(58.U(7.W))),
    (BitPat("b1111010") , BitPat(57.U(7.W))),
    (BitPat("b1111011") , BitPat(56.U(7.W))),
    (BitPat("b1111100") , BitPat(56.U(7.W))),
    (BitPat("b1111101") , BitPat(55.U(7.W))),
    (BitPat("b1111110") , BitPat(54.U(7.W))),
    (BitPat("b1111111") , BitPat(53.U(7.W))),
  )

  def count_leading_zeros(in: UInt, width: Int): UInt = {
    width.U - PriorityEncoder(Reverse(in))
  }

  val rvs2_bits = io.rvs2_input
  val fTypes = Seq(FType.H, FType.S, FType.D)
  val is_negative = Wire(Vec(3, Bool()))
  val is_pos_zero = Wire(Vec(3, Bool()))
  val is_neg_zero = Wire(Vec(3, Bool()))
  val is_pos_inf = Wire(Vec(3, Bool()))
  val is_sNaN = Wire(Vec(3, Bool()))
  val is_qNaN = Wire(Vec(3, Bool()))
  val output_sign = Wire(Vec(3, UInt(1.W)))
  val output_significand_index = Wire(Vec(3, UInt(7.W)))
  val output_exponent = fTypes.map(f => Wire(UInt(f.exp.W)))

  val mux_select = (1 to 3).map(_.U === io.eew).asUInt

  fTypes.zipWithIndex.foreach { case(fType, i) =>
    val rvs2_rec = fType.recode(rvs2_bits(fType.ieeeWidth-1, 0))

    val rvs2_classify = fType.classify(rvs2_rec)

    is_negative(i) := rvs2_classify(2,0).orR
    is_pos_zero(i) := rvs2_classify(4)
    is_neg_zero(i) := rvs2_classify(3)
    is_pos_inf(i) := rvs2_classify(7)
    is_sNaN(i) := rvs2_classify(8)
    is_qNaN(i) := rvs2_classify(9)

    val num_leading_significand_zeros = count_leading_zeros(rvs2_bits(fType.sig-2, 0), fType.sig)
    val is_pos_normal = rvs2_classify(6)
    val normalized_exponent = Wire(UInt(fType.exp.W))
    val normalized_significand = Wire(UInt((fType.sig - 1).W))
    normalized_exponent := Mux(is_pos_normal,
                               rvs2_bits(fType.ieeeWidth-2, fType.ieeeWidth-2-fType.exp+1),
                               (0.U - num_leading_significand_zeros))
    normalized_significand := Mux(is_pos_normal,
                                  rvs2_bits(fType.sig-2, 0),
                                  rvs2_bits(fType.sig-2, 0) << (1.U - normalized_exponent))

    output_exponent(i) := (((3.U * ((1 << (fType.exp - 1)) - 1).U) - normalized_exponent) >> 1)

    output_significand_index(i) := Cat(normalized_exponent(0), normalized_significand(fType.sig - 2, fType.sig - 7))
    output_sign(i) := rvs2_bits(fType.ieeeWidth - 1)
  }

  val truthTable = TruthTable(vfrsqrt7_table, default_bits)
  val significand_bits = chisel3.util.experimental.decode.decoder(EspressoMinimizer, Mux1H(mux_select, output_significand_index), truthTable)

  val out_bits = fTypes.zipWithIndex.map{ case(fType, i) =>
    val out = WireInit(output_sign(i) ## output_exponent(i) ## significand_bits ## 0.U((fType.sig - 8).W))
    Fill(64 / fType.ieeeWidth, out)
  }

  val eew_sel = io.eew - 1.U

  when (is_negative(eew_sel) || is_sNaN(eew_sel) || is_qNaN(eew_sel)) {
    io.out := Mux1H(mux_select, fTypes.map{ fType => Fill(64 / fType.ieeeWidth, fType.ieeeQNaN) })
  } .elsewhen (is_pos_inf(eew_sel)) {
    io.out := 0.U
  } .elsewhen (is_pos_zero(eew_sel)) {
    io.out := Mux1H(mux_select, Seq("h7C007C007C007C00".U, "h7F8000007F800000".U, "h7FF0000000000000".U))
  } .elsewhen (is_neg_zero(eew_sel)) {
    io.out := Mux1H(mux_select, Seq("hFC00FC00FC00FC00".U, "hFF800000FF800000".U, "hFFF0000000000000".U))
  } .otherwise {
    io.out := Mux1H(mux_select, out_bits)
  }

  val NV = is_negative(eew_sel) || is_sNaN(eew_sel)
  val DZ = is_pos_zero(eew_sel) || is_neg_zero(eew_sel)

  io.exc := Cat(NV, Cat(DZ, 0.U(3.W)))
}


class FPDivSqrt(implicit p: Parameters) extends IterativeFunctionalUnit()(p) with HasFPUParameters {
  val supported_insns = Seq(
    FDIV.VV, FDIV.VF,
    FRDIV.VF,
    FSQRT_V,
    FRSQRT7_V,
    FREC7_V,
    FCLASS_V
  ).map(_.elementWise)

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
  iss_fire_pipe := io.iss.valid && io.iss.ready

  divSqrt.io.inValid := iss_fire_pipe && !(op.rvd_eew === 1.U) && (div_op || (op.opff6 === OPFFunct6.funary1 && op.rs1 === 0.U))
  divSqrt.io.sqrtOp := !div_op
  divSqrt16.io.inValid := iss_fire_pipe && (op.rvd_eew === 1.U) && (div_op || (op.opff6 === OPFFunct6.funary1 && op.rs1 === 0.U))
  divSqrt16.io.sqrtOp := !div_op

  io.hazard.valid := valid
  io.hazard.bits.eg := op.wvd_eg

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

  val out_buffer = RegEnable(divSqrt_out, divSqrt_out_valid)
  val out_toWrite = RegInit(false.B)
  val divSqrt_write = Mux(out_toWrite, out_buffer, divSqrt_out)

  val divSqrt16_out = FType.H.ieee(divSqrt16.io.out)
  val out16_buffer = RegEnable(divSqrt16_out, divSqrt16_out_valid)
  val out16_toWrite = RegInit(false.B)
  val divSqrt16_write = Mux(out16_toWrite, out16_buffer, divSqrt16_out)

  // vfclass instruction
  val gen_vfclass = Seq(FType.H, FType.S, FType.D).zipWithIndex.map { case(fType, i) =>
    Fill(2, Cat(0.U((fType.ieeeWidth-10).W), fType.classify(fType.recode(rvs2_bits(fType.ieeeWidth-1,0)))))
  }

  // Reciprocal Sqrt Approximation
  val recSqrt7 = Module(new VFRSQRT7)
  recSqrt7.io.rvs2_input := rvs2_bits
  recSqrt7.io.eew := op.rvs2_eew

  // Reciprocal Approximation
  val rec7 = Module(new VFREC7)
  rec7.io.rvs2_input := rvs2_bits
  rec7.io.eew := op.rvs2_eew
  rec7.io.frm := op.frm

  // Capture result in case of write port backpressure
  when (io.write.fire) {
    out_toWrite := false.B
    out16_toWrite := false.B
  } .elsewhen (divSqrt_out_valid) {
    out_toWrite := true.B
    out16_toWrite := true.B
  }

  val vfclass_inst = op.opff6.isOneOf(OPFFunct6.funary1) && op.rs1 === 16.U
  val vfrsqrt7_inst = op.opff6.isOneOf(OPFFunct6.funary1) && op.rs1 === 4.U
  val vfrec7_inst = op.opff6.isOneOf(OPFFunct6.funary1) && op.rs1 === 5.U
  val out = Mux1H(
    Seq(vfclass_inst, vfrsqrt7_inst, vfrec7_inst, out_toWrite || divSqrt_out_valid || divSqrt16_out_valid),
    Seq(Mux1H(Seq(op.rvs2_eew === 3.U, op.rvs2_eew === 2.U, op.rvs2_eew === 1.U), Seq(gen_vfclass(2), gen_vfclass(1), gen_vfclass(0))), recSqrt7.io.out, rec7.io.out, divSqrt_write)
  )(63,0)

  io.write.valid := ((vfclass_inst || vfrsqrt7_inst || vfrec7_inst) && valid) || out_toWrite || divSqrt_out_valid
  io.write.bits.eg := op.wvd_eg
  io.write.bits.mask := FillInterleaved(8, op.wmask)
  io.write.bits.data := Fill(dLenB >> 3, out)
  io.iss.ready := accept_inst.matched && ((divSqrt_ready && io.iss.op.vd_eew >= 2.U) || (divSqrt16_ready && io.iss.op.vd_eew === 1.U)) && (!valid || last)
  last := io.write.fire

  io.set_fflags.valid := divSqrt_out_valid || divSqrt16_out_valid || (vfrsqrt7_inst && io.write.fire) || (vfrec7_inst && io.write.fire)
  io.set_fflags.bits := (divSqrt.io.exceptionFlags & Fill(5, divSqrt_out_valid)) | divSqrt16.io.exceptionFlags & Fill(5, divSqrt_out_valid) | (recSqrt7.io.exc & Fill(5, vfrsqrt7_inst)) | (rec7.io.exc & Fill(5, vfrec7_inst))

  io.scalar_write.valid := false.B
  io.scalar_write.bits := DontCare

  io.acc := false.B
  io.tail := false.B
}
