package vector.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import chisel3.util.experimental.decode._
import vector.common._


class VFREC7(implicit p: Parameters) extends FPUModule()(p) {
  val io = IO(new Bundle {
    val rvs2_input = Input(UInt(64.W))
    val eew = Input(UInt(3.W))
    val frm = Input(UInt(3.W))
    val out = Output(UInt(64.W))
    val exc = Output(UInt(5.W))
  })

  val default_bits = BitPat("b0000000")
  val vfrec7_table = Seq(
    (BitPat("b0000000") , BitPat(127.U(7.W))),
    (BitPat("b0000001") , BitPat(125.U(7.W))),
    (BitPat("b0000010") , BitPat(123.U(7.W))),
    (BitPat("b0000011") , BitPat(121.U(7.W))),
    (BitPat("b0000100") , BitPat(119.U(7.W))),
    (BitPat("b0000101") , BitPat(117.U(7.W))),
    (BitPat("b0000110") , BitPat(116.U(7.W))),
    (BitPat("b0000111") , BitPat(114.U(7.W))),
    (BitPat("b0001000") , BitPat(112.U(7.W))),
    (BitPat("b0001001") , BitPat(110.U(7.W))),
    (BitPat("b0001010") , BitPat(109.U(7.W))),
    (BitPat("b0001011") , BitPat(107.U(7.W))),
    (BitPat("b0001100") , BitPat(105.U(7.W))),
    (BitPat("b0001101") , BitPat(104.U(7.W))),
    (BitPat("b0001110") , BitPat(102.U(7.W))),
    (BitPat("b0001111") , BitPat(100.U(7.W))),
    (BitPat("b0010000") , BitPat(99.U(7.W))),
    (BitPat("b0010001") , BitPat(97.U(7.W))),
    (BitPat("b0010010") , BitPat(96.U(7.W))),
    (BitPat("b0010011") , BitPat(94.U(7.W))),
    (BitPat("b0010100") , BitPat(93.U(7.W))),
    (BitPat("b0010101") , BitPat(91.U(7.W))),
    (BitPat("b0010110") , BitPat(90.U(7.W))),
    (BitPat("b0010111") , BitPat(88.U(7.W))),
    (BitPat("b0011000") , BitPat(87.U(7.W))),
    (BitPat("b0011001") , BitPat(85.U(7.W))),
    (BitPat("b0011010") , BitPat(84.U(7.W))),
    (BitPat("b0011011") , BitPat(83.U(7.W))),
    (BitPat("b0011100") , BitPat(81.U(7.W))),
    (BitPat("b0011101") , BitPat(80.U(7.W))),
    (BitPat("b0011110") , BitPat(79.U(7.W))),
    (BitPat("b0011111") , BitPat(77.U(7.W))),
    (BitPat("b0100000") , BitPat(76.U(7.W))),
    (BitPat("b0100001") , BitPat(75.U(7.W))),
    (BitPat("b0100010") , BitPat(74.U(7.W))),
    (BitPat("b0100011") , BitPat(72.U(7.W))),
    (BitPat("b0100100") , BitPat(71.U(7.W))),
    (BitPat("b0100101") , BitPat(70.U(7.W))),
    (BitPat("b0100110") , BitPat(69.U(7.W))),
    (BitPat("b0100111") , BitPat(68.U(7.W))),
    (BitPat("b0101000") , BitPat(66.U(7.W))),
    (BitPat("b0101001") , BitPat(65.U(7.W))),
    (BitPat("b0101010") , BitPat(64.U(7.W))),
    (BitPat("b0101011") , BitPat(63.U(7.W))),
    (BitPat("b0101100") , BitPat(62.U(7.W))),
    (BitPat("b0101101") , BitPat(61.U(7.W))),
    (BitPat("b0101110") , BitPat(60.U(7.W))),
    (BitPat("b0101111") , BitPat(59.U(7.W))),
    (BitPat("b0110000") , BitPat(58.U(7.W))),
    (BitPat("b0110001") , BitPat(57.U(7.W))),
    (BitPat("b0110010") , BitPat(56.U(7.W))),
    (BitPat("b0110011") , BitPat(55.U(7.W))),
    (BitPat("b0110100") , BitPat(54.U(7.W))),
    (BitPat("b0110101") , BitPat(53.U(7.W))),
    (BitPat("b0110110") , BitPat(52.U(7.W))),
    (BitPat("b0110111") , BitPat(51.U(7.W))),
    (BitPat("b0111000") , BitPat(50.U(7.W))),
    (BitPat("b0111001") , BitPat(49.U(7.W))),
    (BitPat("b0111010") , BitPat(48.U(7.W))),
    (BitPat("b0111011") , BitPat(47.U(7.W))),
    (BitPat("b0111100") , BitPat(46.U(7.W))),
    (BitPat("b0111101") , BitPat(46.U(7.W))),
    (BitPat("b0111110") , BitPat(44.U(7.W))),
    (BitPat("b0111111") , BitPat(43.U(7.W))),
    (BitPat("b1000001") , BitPat(42.U(7.W))),
    (BitPat("b1000010") , BitPat(41.U(7.W))),
    (BitPat("b1000011") , BitPat(40.U(7.W))),
    (BitPat("b1000100") , BitPat(40.U(7.W))),
    (BitPat("b1000101") , BitPat(39.U(7.W))),
    (BitPat("b1000110") , BitPat(38.U(7.W))),
    (BitPat("b1000111") , BitPat(37.U(7.W))),
    (BitPat("b1001000") , BitPat(36.U(7.W))),
    (BitPat("b1001001") , BitPat(35.U(7.W))),
    (BitPat("b1001010") , BitPat(35.U(7.W))),
    (BitPat("b1001011") , BitPat(34.U(7.W))),
    (BitPat("b1001100") , BitPat(33.U(7.W))),
    (BitPat("b1001101") , BitPat(32.U(7.W))),
    (BitPat("b1001110") , BitPat(31.U(7.W))),
    (BitPat("b1001111") , BitPat(31.U(7.W))),
    (BitPat("b1010000") , BitPat(30.U(7.W))),
    (BitPat("b1010001") , BitPat(29.U(7.W))),
    (BitPat("b1010010") , BitPat(28.U(7.W))),
    (BitPat("b1010011") , BitPat(28.U(7.W))),
    (BitPat("b1010100") , BitPat(27.U(7.W))),
    (BitPat("b1010101") , BitPat(26.U(7.W))),
    (BitPat("b1010110") , BitPat(25.U(7.W))),
    (BitPat("b1010111") , BitPat(25.U(7.W))),
    (BitPat("b1011000") , BitPat(24.U(7.W))),
    (BitPat("b1011001") , BitPat(23.U(7.W))),
    (BitPat("b1011010") , BitPat(23.U(7.W))),
    (BitPat("b1011011") , BitPat(22.U(7.W))),
    (BitPat("b1011100") , BitPat(21.U(7.W))),
    (BitPat("b1011101") , BitPat(21.U(7.W))),
    (BitPat("b1011110") , BitPat(20.U(7.W))),
    (BitPat("b1011111") , BitPat(19.U(7.W))),
    (BitPat("b1100000") , BitPat(19.U(7.W))),
    (BitPat("b1100001") , BitPat(18.U(7.W))),
    (BitPat("b1100010") , BitPat(17.U(7.W))),
    (BitPat("b1100011") , BitPat(17.U(7.W))),
    (BitPat("b1100100") , BitPat(16.U(7.W))),
    (BitPat("b1100101") , BitPat(15.U(7.W))),
    (BitPat("b1100110") , BitPat(15.U(7.W))),
    (BitPat("b1100111") , BitPat(14.U(7.W))),
    (BitPat("b1101000") , BitPat(14.U(7.W))),
    (BitPat("b1101001") , BitPat(13.U(7.W))),
    (BitPat("b1101010") , BitPat(12.U(7.W))),
    (BitPat("b1101011") , BitPat(12.U(7.W))),
    (BitPat("b1101100") , BitPat(11.U(7.W))),
    (BitPat("b1101101") , BitPat(11.U(7.W))),
    (BitPat("b1101110") , BitPat(10.U(7.W))),
    (BitPat("b1101111") , BitPat(9.U(7.W))),
    (BitPat("b1110000") , BitPat(8.U(7.W))),
    (BitPat("b1110001") , BitPat(8.U(7.W))),
    (BitPat("b1110010") , BitPat(7.U(7.W))),
    (BitPat("b1110011") , BitPat(7.U(7.W))),
    (BitPat("b1110100") , BitPat(6.U(7.W))),
    (BitPat("b1110101") , BitPat(5.U(7.W))),
    (BitPat("b1110110") , BitPat(5.U(7.W))),
    (BitPat("b1110111") , BitPat(4.U(7.W))),
    (BitPat("b1111000") , BitPat(4.U(7.W))),
    (BitPat("b1111001") , BitPat(3.U(7.W))),
    (BitPat("b1111010") , BitPat(3.U(7.W))),
    (BitPat("b1111011") , BitPat(2.U(7.W))),
    (BitPat("b1111100") , BitPat(2.U(7.W))),
    (BitPat("b1111101") , BitPat(1.U(7.W))),
    (BitPat("b1111110") , BitPat(1.U(7.W))),
    (BitPat("b1111111") , BitPat(0.U(7.W))),
  )

  def count_leading_zeros(in: UInt, width: Int): UInt = {
    width.U - PriorityEncoder(Reverse(in))
  }

  val rvs2_bits = io.rvs2_input
  val fTypes = Seq(FType.S, FType.D)
  val is_negative = Wire(Vec(2, Bool()))
  val is_pos_zero = Wire(Vec(2, Bool()))
  val is_neg_zero = Wire(Vec(2, Bool()))
  val is_pos_inf = Wire(Vec(2, Bool()))
  val is_neg_inf = Wire(Vec(2, Bool()))
  val is_sNaN = Wire(Vec(2, Bool()))
  val is_qNaN = Wire(Vec(2, Bool()))
  val is_subnormal = Wire(Vec(2, Bool()))
  val more_than_1_leading_sign_zero = Wire(Vec(2, Bool()))
  val output_sign = Wire(Vec(2, UInt(1.W)))
  val output_significand_index = Wire(Vec(2, UInt(7.W)))

  val is_subnormal_output = Wire(Vec(2, Bool()))

  val output_exponent_s = Wire(UInt(8.W))
  val output_exponent_d = Wire(UInt(11.W))

  for (eew <- 2 until 4) {
    val fType = fTypes(eew - 2)

    val rvs2_classify = fType.classify(fType.recode(rvs2_bits(fType.ieeeWidth-1, 0)))
    is_negative(eew - 2) := rvs2_classify(2,0).orR
    is_pos_zero(eew - 2) := rvs2_classify(4)
    is_neg_zero(eew - 2) := rvs2_classify(3)
    is_pos_inf(eew - 2) := rvs2_classify(7)
    is_neg_inf(eew - 2) := rvs2_classify(0)
    is_sNaN(eew - 2) := rvs2_classify(8)
    is_qNaN(eew - 2) := rvs2_classify(9)
    is_subnormal(eew - 2) := rvs2_classify(2) || rvs2_classify(5)

    val num_leading_significand_zeros = count_leading_zeros(rvs2_bits(fType.sig-2, 0), fType.sig)
    more_than_1_leading_sign_zero(eew - 2) := num_leading_significand_zeros > 1.U

    val is_normal = rvs2_classify(1) || rvs2_classify(6)

    val normalized_exponent = Wire(UInt(fType.exp.W))
    val normalized_significand = Wire(UInt((fType.sig - 1).W))
    normalized_exponent := Mux(is_normal,
                               rvs2_bits(fType.ieeeWidth-2, fType.ieeeWidth-2-fType.exp+1),
                               (0.U - num_leading_significand_zeros))
    normalized_significand := Mux(is_normal,
                                  rvs2_bits(fType.sig-2, 0),
                                  rvs2_bits(fType.sig-2, 0) << (1.U - normalized_exponent))

    val output_exponent_val = (2.U * ((1 << (fType.exp - 1)) - 1).U) - 1.U - normalized_exponent
    if (eew == 3) {
      output_exponent_d := output_exponent_val
    } else {
      output_exponent_s := output_exponent_val
    }

    is_subnormal_output(eew - 2) := output_exponent_val === 0.U || output_exponent_val.asSInt === -1.S
    output_significand_index(eew-2) := normalized_significand(fType.sig - 2, fType.sig - 8)
    output_sign(eew-2) := rvs2_bits(fType.ieeeWidth - 1)
  }

  val truthTable = TruthTable(vfrec7_table, default_bits)
  val significand_bits = chisel3.util.experimental.decode.decoder(EspressoMinimizer, Mux(io.eew === 3.U, output_significand_index(1), output_significand_index(0)), truthTable)

  val s_out = Wire(UInt(32.W))
  val d_out = Wire(UInt(64.W))

  val s_out_exponent = Wire(UInt(8.W))
  val d_out_exponent = Wire(UInt(11.W))
  val s_out_significand = Wire(UInt(23.W))
  val d_out_significand = Wire(UInt(52.W))

  // Special case where the normalized output exponent is 0 or -1
  // This means the output will be subnormal and special changes are required
  when (is_subnormal_output(0)) {
    s_out_exponent := 0.U
    when (output_exponent_s(0) === 0.U) {
      s_out_significand := Cat(Cat(1.U, significand_bits), 0.U(15.W))
    } .otherwise {
      s_out_significand := Cat(Cat(1.U, significand_bits), 0.U(15.W)) >> 1
    }
  }
  // Special case where the input is a very small subnormal and the output depends on the rounding mode
  .elsewhen (is_subnormal(0) && more_than_1_leading_sign_zero(0)) {
    when ((output_sign(0).asBool && (io.frm === 3.U || io.frm === 1.U)) || (!output_sign(0) && (io.frm === 2.U || io.frm === 1.U))) {
      s_out_exponent := "b11111110".U
      s_out_significand := "b11111111111111111111111".U
    } .otherwise {
      s_out_exponent := "b11111111".U
      s_out_significand := 0.U
    }
  }
  .otherwise {
    s_out_exponent := output_exponent_s
    s_out_significand := Cat(significand_bits, 0.U(16.W))
  }

  when (is_subnormal_output(1)) {
    d_out_exponent := 0.U
    when (output_exponent_d(1) === 0.U) {
      d_out_significand := Cat(Cat(1.U, significand_bits), 0.U(44.W))
    } .otherwise {
      d_out_significand := Cat(Cat(1.U, significand_bits), 0.U(44.W)) >> 1
    }
  }
  .elsewhen (is_subnormal(1) && more_than_1_leading_sign_zero(1)) {
    when ((output_sign(1).asBool && (io.frm === 3.U || io.frm === 1.U)) || (!output_sign(1) && (io.frm === 2.U || io.frm === 1.U))) {
      d_out_exponent := "b11111111110".U
      d_out_significand := "b1111111111111111111111111111111111111111111111111111".U
    } .otherwise {
      d_out_exponent := "b11111111".U
      d_out_significand := 0.U
    }
  }
  .otherwise {
    d_out_exponent := output_exponent_d
    d_out_significand := Cat(significand_bits, 0.U(45.W))
  }

  s_out := Cat(output_sign(0), Cat(s_out_exponent, s_out_significand))
  d_out := Cat(output_sign(1), Cat(d_out_exponent, d_out_significand))

  val sel = io.eew === 3.U

  when (is_sNaN(sel) || is_qNaN(sel)) {
    io.out := Mux(sel, FType.D.ieeeQNaN, Fill(2, FType.S.ieeeQNaN))
  } .elsewhen (is_pos_inf(sel)) {
    io.out := 0.U
  } .elsewhen (is_neg_inf(sel)) {
    io.out := Mux(sel, "h8000000000000000".U, "h8000000080000000".U)
  } .elsewhen (is_pos_zero(sel)) {
    io.out := Mux(sel, "h7FF0000000000000".U, "h7F8000007F800000".U)
  } .elsewhen (is_neg_zero(sel)) {
    io.out := Mux(sel, "hFFF0000000000000".U, "hFF800000FF800000".U)
  } .otherwise {
    io.out := Mux(sel, d_out, Fill(2, s_out))
  }

  val NV = is_negative(sel) || is_sNaN(sel)
  val DZ = is_pos_zero(sel) || is_neg_zero(sel)
  val OF = is_subnormal(sel) && more_than_1_leading_sign_zero(sel)
  val NX = is_subnormal(sel) && more_than_1_leading_sign_zero(sel)

  io.exc := Seq(NV, DZ, OF, false.B, NX).asUInt
}


class VFRSQRT7(implicit p: Parameters) extends FPUModule()(p) {
  val io = IO(new Bundle {
    val rvs2_input = Input(UInt(64.W))
    val eew = Input(UInt(3.W))
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
  val fTypes = Seq(FType.S, FType.D)
  val is_negative = Wire(Vec(2, Bool()))
  val is_pos_zero = Wire(Vec(2, Bool()))
  val is_neg_zero = Wire(Vec(2, Bool()))
  val is_pos_inf = Wire(Vec(2, Bool()))
  val is_sNaN = Wire(Vec(2, Bool()))
  val is_qNaN = Wire(Vec(2, Bool()))
  val output_sign = Wire(Vec(2, UInt(1.W)))
  val output_significand_index = Wire(Vec(2, UInt(7.W)))

  val output_exponent_s = Wire(UInt(8.W))
  val output_exponent_d = Wire(UInt(11.W))

  for (eew <- 2 until 4) {
    val fType = fTypes(eew - 2)
    val rvs2_rec = fType.recode(rvs2_bits(fType.ieeeWidth-1, 0))

    val rvs2_classify = fType.classify(rvs2_rec)

    is_negative(eew - 2) := rvs2_classify(2,0).orR
    is_pos_zero(eew - 2) := rvs2_classify(4)
    is_neg_zero(eew - 2) := rvs2_classify(3)
    is_pos_inf(eew - 2) := rvs2_classify(7)
    is_sNaN(eew - 2) := rvs2_classify(8)
    is_qNaN(eew - 2) := rvs2_classify(9)

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

    val output_exponent_val = (((3.U * ((1 << (fType.exp - 1)) - 1).U) - normalized_exponent) >> 1)
    if (eew == 3) {
      output_exponent_d := output_exponent_val
    } else {
      output_exponent_s := output_exponent_val
    }
    output_significand_index(eew-2) := Cat(normalized_exponent(0), normalized_significand(fType.sig - 2, fType.sig - 7))
    output_sign(eew-2) := rvs2_bits(fType.ieeeWidth - 1)
  }

  val truthTable = TruthTable(vfrsqrt7_table, default_bits)
  val significand_bits = chisel3.util.experimental.decode.decoder(EspressoMinimizer, Mux(io.eew === 3.U, output_significand_index(1), output_significand_index(0)), truthTable)

  val s_out = Wire(UInt(32.W))
  val d_out = Wire(UInt(64.W))
  s_out := Cat(output_sign(0), Cat(output_exponent_s, Cat(significand_bits, 0.U(16.W))))
  d_out := Cat(output_sign(1), Cat(output_exponent_d, Cat(significand_bits, 0.U(45.W))))

  val sel = io.eew === 3.U

  when (is_negative(sel) || is_sNaN(sel) || is_qNaN(sel)) {
    io.out := Mux(sel, FType.D.ieeeQNaN, Fill(2, FType.S.ieeeQNaN))
  } .elsewhen (is_pos_inf(sel)) {
    io.out := 0.U
  } .elsewhen (is_pos_zero(sel)) {
    io.out := Mux(sel, "h7FF0000000000000".U, "h7F8000007F800000".U)
  } .elsewhen (is_neg_zero(sel)) {
    io.out := Mux(sel, "hFFF0000000000000".U, "hFF800000FF800000".U)
  } .otherwise {
    io.out := Mux(sel, d_out, Fill(2, s_out))
  }

  val NV = is_negative(sel) || is_sNaN(sel)
  val DZ = is_pos_zero(sel) || is_neg_zero(sel)

  io.exc := Cat(NV, Cat(DZ, "b000".U))
}


class VFDivSqrt(implicit p: Parameters) extends IterativeFunctionalUnit()(p) with HasFPUParameters {
  io.iss.sub_dlen := log2Ceil(dLenB).U - io.iss.op.rvs2_eew
  io.set_vxsat := false.B

  val divSqrt = Module(new hardfloat.DivSqrtRecF64)

  lazy val ctrl_table = Seq(
    (OPFFunct6.fdiv     ,   Seq(Y,N)),
    (OPFFunct6.frdiv    ,   Seq(Y,Y)),
    (OPFFunct6.funary1  ,   Seq(N,N)),
  )
  val ctrl_isDiv :: ctrl_swap12 :: Nil = VecDecode.applyBools(
    io.iss.op.funct3, io.iss.op.funct6,
    Seq.fill(2)(X), ctrl_table
  )
  def accepts(f3: UInt, f6: UInt): Bool = VecDecode(f3, f6, ctrl_table.map(_._1))
  val divSqrt_ready = (ctrl_isDiv && divSqrt.io.inReady_div) || (!ctrl_isDiv && divSqrt.io.inReady_sqrt)

  val rvs2_bits = extract(io.iss.op.rvs2_data, false.B, io.iss.op.rvs2_eew, io.iss.op.eidx)(63,0)
  val rvs1_bits = extract(io.iss.op.rvs1_data, false.B, io.iss.op.rvs1_eew, io.iss.op.eidx)(63,0)
  val rvs2_op_bits = extract(op.rvs2_data, false.B, op.rvs2_eew, op.eidx)(63,0)

  divSqrt.io.detectTininess := hardfloat.consts.tininess_afterRounding
  divSqrt.io.roundingMode := io.iss.op.frm

  divSqrt.io.inValid := (io.iss.valid && io.iss.ready) && (ctrl_isDiv || (io.iss.op.opff6 === OPFFunct6.funary1 && io.iss.op.rs1 === 0.U))
  divSqrt.io.sqrtOp := !ctrl_isDiv

  io.hazard.valid := valid
  io.hazard.bits.vat := op.vat
  io.hazard.bits.eg := op.wvd_eg

  when (io.iss.op.rvs1_eew === 3.U) {
    divSqrt.io.a := Mux(ctrl_swap12 && ctrl_isDiv, FType.D.recode(rvs1_bits), FType.D.recode(rvs2_bits))
    divSqrt.io.b := Mux(ctrl_swap12 || !ctrl_isDiv, FType.D.recode(rvs2_bits), FType.D.recode(rvs1_bits))
  } .otherwise {
    val narrow_rvs2_bits = rvs2_bits(31,0)
    val narrow_rvs1_bits = rvs1_bits(31,0)
    val widen = Seq(FType.S.recode(narrow_rvs2_bits), FType.S.recode(narrow_rvs1_bits)).zip(
      Seq.fill(2)(Module(new hardfloat.RecFNToRecFN(8, 24, 11, 53)))).map { case(input, upconvert) =>
      upconvert.io.in := input
      upconvert.io.roundingMode := io.iss.op.frm
      upconvert.io.detectTininess := hardfloat.consts.tininess_afterRounding
      upconvert
    }

    divSqrt.io.a := Mux(ctrl_swap12 && ctrl_isDiv, widen(1).io.out, widen(0).io.out)
    divSqrt.io.b := Mux(ctrl_swap12 || !ctrl_isDiv, widen(0).io.out, widen(1).io.out)
  }

  val divSqrt_out_valid = divSqrt.io.outValid_div || divSqrt.io.outValid_sqrt

  val narrow = Module(new hardfloat.RecFNToRecFN(11, 53, 8, 24))
  narrow.io.roundingMode := op.frm
  narrow.io.detectTininess := hardfloat.consts.tininess_afterRounding
  narrow.io.in := divSqrt.io.out

  val divSqrt_out = Mux(op.vd_eew === 3.U, FType.D.ieee(divSqrt.io.out), Fill(2, FType.S.ieee(narrow.io.out)))

  val out_buffer = RegEnable(divSqrt_out, divSqrt_out_valid)
  val out_toWrite = RegInit(false.B)
  val divSqrt_write = Mux(out_toWrite, out_buffer, divSqrt_out)

  // vfclass instruction
  val gen_vfclass = Seq(FType.S, FType.D).zipWithIndex.map { case(fType, i) =>
    Fill(2, Cat(0.U((fType.ieeeWidth-10).W), fType.classify(fType.recode(rvs2_op_bits(fType.ieeeWidth-1,0)))))
  }

  // Reciprocal Sqrt Approximation
  val recSqrt7 = Module(new VFRSQRT7)
  recSqrt7.io.rvs2_input := rvs2_op_bits
  recSqrt7.io.eew := op.rvs2_eew

  // Reciprocal Approximation
  val rec7 = Module(new VFREC7)
  rec7.io.rvs2_input := rvs2_op_bits
  rec7.io.eew := op.rvs2_eew
  rec7.io.frm := op.frm

  // Capture result in case of write port backpressure
  when (io.write.fire()) {
    out_toWrite := false.B
  } .elsewhen (divSqrt_out_valid) {
    out_toWrite := true.B
  }

  val vfclass_inst = op.opff6.isOneOf(OPFFunct6.funary1) && op.rs1 === 16.U
  val vfrsqrt7_inst = op.opff6.isOneOf(OPFFunct6.funary1) && op.rs1 === 4.U
  val vfrec7_inst = op.opff6.isOneOf(OPFFunct6.funary1) && op.rs1 === 5.U

  io.write.valid := ((vfclass_inst || vfrsqrt7_inst || vfrec7_inst) && valid) || out_toWrite || divSqrt_out_valid
  io.write.bits.eg := op.wvd_eg
  io.write.bits.mask := FillInterleaved(8, op.wmask)
  io.write.bits.data := Mux1H(Seq(vfclass_inst, vfrsqrt7_inst, vfrec7_inst, out_toWrite || divSqrt_out_valid),
                              Seq(Mux(op.rvs2_eew === 3.U, gen_vfclass(1), gen_vfclass(0)), recSqrt7.io.out, rec7.io.out, divSqrt_write))
  io.iss.ready := accepts(io.iss.op.funct3, io.iss.op.funct6) && divSqrt_ready && (!valid || last)
  last := io.write.fire()

  io.set_fflags.valid := divSqrt_out_valid || (vfrsqrt7_inst && io.write.fire()) || (vfrec7_inst && io.write.fire())
  io.set_fflags.bits := (divSqrt.io.exceptionFlags & Fill(5, divSqrt_out_valid)) | (recSqrt7.io.exc & Fill(5, vfrsqrt7_inst)) | (rec7.io.exc & Fill(5, vfrec7_inst))

  io.scalar_write.valid := false.B
  io.scalar_write.bits := DontCare
}
