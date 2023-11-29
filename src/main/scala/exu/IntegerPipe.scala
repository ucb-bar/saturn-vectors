package vector.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import vector.common._

class AdderArray(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val in1 = Input(Vec(dLenB, UInt(8.W)))
    val in2 = Input(Vec(dLenB, UInt(8.W)))
    val use_carry  = Input(UInt(dLenB.W))
    val mask_carry = Input(UInt(dLenB.W))

    val sub       = Input(Bool())
    val cmask     = Input(Bool())

    val out   = Output(Vec(dLenB, UInt(8.W)))
    val carry = Output(Vec(dLenB, Bool()))
  })

  val carries = Wire(Vec(dLenB+1, Bool()))
  carries(0) := io.sub
  io.carry := carries.drop(1)

  for (i <- 0 until dLenB) {
    val sum = (Mux(io.sub, ~io.in1(i), io.in1(i)) +&
      io.in2(i) +&
      Mux(io.use_carry(i), carries(i), io.sub) +&
      (io.cmask & !io.sub & io.mask_carry(i)) - (io.cmask & io.sub & io.mask_carry(i))
    )

    io.out(i) := sum(7,0)
    carries(i+1) := sum(8)
  }
}

class IntegerPipe(implicit p: Parameters) extends PipelinedFunctionalUnit(1, true)(p) {
  io.iss.sub_dlen := 0.U

  val rvs1_eew = io.pipe(0).bits.rvs1_eew
  val rvs2_eew = io.pipe(0).bits.rvs2_eew
  val vd_eew   = io.pipe(0).bits.vd_eew

  lazy val ctrl_table = Seq(
    (OPIFunct6.add    , Seq(N,X,N,N,N,X,N,X,N,X)),
    (OPIFunct6.sub    , Seq(Y,X,N,N,N,X,N,X,N,X)),
    (OPIFunct6.rsub   , Seq(Y,X,N,N,N,X,N,X,Y,X)),
    (OPMFunct6.waddu  , Seq(N,N,N,N,N,X,N,X,N,X)),
    (OPMFunct6.wadd   , Seq(N,Y,N,N,N,X,N,X,N,X)),
    (OPMFunct6.wsubu  , Seq(Y,N,N,N,N,X,N,X,N,X)),
    (OPMFunct6.wsub   , Seq(Y,Y,N,N,N,X,N,X,N,X)),
    (OPMFunct6.wadduw , Seq(N,N,Y,N,N,X,N,X,N,X)),
    (OPMFunct6.waddw  , Seq(N,Y,Y,N,N,X,N,X,N,X)),
    (OPMFunct6.wsubuw , Seq(Y,N,Y,N,N,X,N,X,N,X)),
    (OPMFunct6.wsubw  , Seq(Y,Y,Y,N,N,X,N,X,N,X)),
    (OPIFunct6.adc    , Seq(N,X,N,N,N,X,N,X,N,X)),
    (OPIFunct6.madc   , Seq(N,X,N,N,N,X,Y,N,N,X)),
    (OPIFunct6.sbc    , Seq(Y,X,N,N,N,X,N,X,N,X)),
    (OPIFunct6.msbc   , Seq(Y,X,N,N,N,X,Y,N,N,X)),
    (OPIFunct6.and    , Seq(X,X,X,Y,N,X,N,X,X,X)),
    (OPIFunct6.or     , Seq(X,X,X,Y,N,X,N,X,X,X)),
    (OPIFunct6.xor    , Seq(X,X,X,Y,N,X,N,X,X,X)),
    (OPMFunct6.xunary0, Seq(X,X,X,N,N,X,N,X,X,X)),
    (OPIFunct6.sll    , Seq(X,X,N,N,Y,Y,N,X,X,X)),
    (OPIFunct6.sra    , Seq(X,X,N,N,Y,N,N,X,X,X)),
    (OPIFunct6.srl    , Seq(X,N,N,N,Y,N,N,X,X,X)),
    (OPIFunct6.nsra   , Seq(X,N,Y,N,Y,N,N,X,X,X)),
    (OPIFunct6.nsrl   , Seq(X,N,Y,N,Y,N,N,X,X,X)),
    (OPIFunct6.mseq   , Seq(X,X,X,N,N,X,Y,Y,X,N)),
    (OPIFunct6.msne   , Seq(X,X,X,N,N,X,Y,Y,X,N)),
    (OPIFunct6.msltu  , Seq(X,X,X,N,N,X,Y,Y,N,Y)),
    (OPIFunct6.mslt   , Seq(X,X,X,N,N,X,Y,Y,N,Y)),
    (OPIFunct6.msleu  , Seq(X,X,X,N,N,X,Y,Y,N,Y)),
    (OPIFunct6.msle   , Seq(X,X,X,N,N,X,Y,Y,N,Y)),
    (OPIFunct6.msgtu  , Seq(X,X,X,N,N,X,Y,Y,Y,Y)),
    (OPIFunct6.msgt   , Seq(X,X,X,N,N,X,Y,Y,Y,Y)),
    (OPIFunct6.minu   , Seq(X,X,X,N,N,X,N,X,N,Y)),
    (OPIFunct6.min    , Seq(X,X,X,N,N,X,N,X,N,Y)),
    (OPIFunct6.maxu   , Seq(X,X,X,N,N,X,N,X,Y,Y)),
    (OPIFunct6.max    , Seq(X,X,X,N,N,X,N,X,Y,Y)),
    (OPIFunct6.merge  , Seq(X,X,X,N,N,X,N,X,N,X)),
    (OPIFunct6.saddu  , Seq(N,X,N,N,N,X,N,X,N,X)),
    (OPIFunct6.sadd   , Seq(N,X,N,N,N,X,N,X,N,X)),
    (OPIFunct6.ssubu  , Seq(Y,X,N,N,N,X,N,X,N,X)),
    (OPIFunct6.ssub   , Seq(Y,X,N,N,N,X,N,X,N,X)),
  )
  override def accepts(f3: UInt, f6: UInt): Bool = VecDecode(f3, f6, ctrl_table.map(_._1))
  val ctrl_sub :: ctrl_add_sext :: ctrl_narrow_vs1 :: ctrl_bw :: ctrl_shift :: ctrl_shift_left :: ctrl_mask_write :: ctrl_cmp :: ctrl_rev12 :: cmp_less :: Nil = VecDecode.applyBools(
    io.pipe(0).bits.funct3, io.pipe(0).bits.funct6,
    Seq.fill(10)(X), ctrl_table)
  val ctrl_cmask = (
    io.pipe(0).bits.opif6.isOneOf(OPIFunct6.adc, OPIFunct6.sbc) ||
    ((io.pipe(0).bits.opif6.isOneOf(OPIFunct6.madc, OPIFunct6.msbc)) && !io.pipe(0).bits.vm)
  )

  val ctrl_rsub = io.pipe(0).bits.opif6 === OPIFunct6.rsub
  val ctrl_xunary0 = io.pipe(0).bits.opmf6 === OPMFunct6.xunary0
  val ctrl_minmax = io.pipe(0).bits.opif6.isOneOf(OPIFunct6.minu, OPIFunct6.min, OPIFunct6.maxu, OPIFunct6.max)
  val ctrl_merge = io.pipe(0).bits.opif6 === OPIFunct6.merge
  val ctrl_sat = io.pipe(0).bits.opif6.isOneOf(OPIFunct6.saddu, OPIFunct6.sadd, OPIFunct6.ssubu, OPIFunct6.ssub)

  val bw_and = io.pipe(0).bits.funct6(1,0) === 1.U
  val bw_or  = io.pipe(0).bits.funct6(1,0) === 2.U
  val bw_xor = io.pipe(0).bits.funct6(1,0) === 3.U
  val bw_out = Mux1H(Seq(
    (bw_and, (io.pipe(0).bits.rvs1_data & io.pipe(0).bits.rvs2_data)),
    (bw_or , (io.pipe(0).bits.rvs1_data | io.pipe(0).bits.rvs2_data)),
    (bw_xor, (io.pipe(0).bits.rvs1_data ^ io.pipe(0).bits.rvs2_data))
  ))

  val shift_sra = io.pipe(0).bits.funct6(0)

  val cmp_signed = io.pipe(0).bits.funct6(0)
  val cmp_inv = io.pipe(0).bits.funct6(0)
  val cmp_sle = io.pipe(0).bits.funct6(2,1) === 2.U

  val sat_signed = io.pipe(0).bits.funct6(0)
  val sat_addu   = io.pipe(0).bits.funct6(1,0) === 0.U
  val sat_subu   = io.pipe(0).bits.funct6(1,0) === 2.U

  val rvs1_bytes = io.pipe(0).bits.rvs1_data.asTypeOf(Vec(dLenB, UInt(8.W)))
  val rvs2_bytes = io.pipe(0).bits.rvs2_data.asTypeOf(Vec(dLenB, UInt(8.W)))

  val in1_bytes = Mux(ctrl_rev12, rvs2_bytes, rvs1_bytes)
  val in2_bytes = Mux(ctrl_rev12, rvs1_bytes, rvs2_bytes)

  val add_use_carry = Cat(0.U(1.W), VecInit.tabulate(4)({ eew =>
    Fill(dLenB >> eew, ~(1.U((1 << eew).W)))
  })(rvs2_eew))
  val add_mask_carry = VecInit.tabulate(4)({ eew =>
    VecInit((0 until dLenB >> eew).map { i => io.pipe(0).bits.rmask(i) | 0.U((1 << eew).W) }).asUInt
  })(rvs2_eew)
  val add_carry = Wire(Vec(dLenB, UInt(1.W)))
  val add_out = Wire(Vec(dLenB, UInt(8.W)))
  val add_wide_out_eew = (0 until 3).map { eew => Wire(Vec(dLenB >> eew, UInt((16 << eew).W))) }
  val add_wide_out = VecInit(add_wide_out_eew.map(_.asUInt))(rvs2_eew)
  val add_narrow_vs1_eew = (0 until 3).map { eew => Wire(Vec(dLenB >> (eew + 1), UInt((16 << eew).W))) }
  val add_narrow_vs1 = VecInit(add_narrow_vs1_eew.map(_.asUInt))(rvs1_eew).asTypeOf(Vec(dLenB, UInt(8.W)))

  val sat_hi_bits = (0 until 4).map { eew => Mux(sat_signed,
    VecInit.tabulate(dLenB >> eew)(i => add_out((i << eew))(7)).asUInt,
    VecInit.tabulate(dLenB >> eew)(i => add_carry(((i+1) << eew)-1)).asUInt
  )}
  val sat_unsigned_mask = VecInit.tabulate(4)({ eew =>
    FillInterleaved(1 << eew, VecInit.tabulate(dLenB >> eew)(i => ctrl_sub ^ add_carry(((i+1) << eew)-1)).asUInt)
  })(vd_eew)
  val sat_unsigned_clip = Mux(sat_addu, ~(0.U(dLen.W)), 0.U(dLen.W)).asTypeOf(Vec(dLenB, UInt(8.W)))

  val (sat_signed_masks, sat_signed_clips): (Seq[UInt], Seq[UInt]) = Seq.tabulate(4)({ eew =>
    val out_sign = VecInit.tabulate(dLenB >> eew)(i =>    add_out(((i+1)<<eew)-1)(7)).asUInt
    val vs2_sign = VecInit.tabulate(dLenB >> eew)(i => rvs2_bytes(((i+1)<<eew)-1)(7)).asUInt
    val vs1_sign = VecInit.tabulate(dLenB >> eew)(i => rvs1_bytes(((i+1)<<eew)-1)(7)).asUInt
    val input_xor  = vs2_sign ^ vs1_sign
    val may_clip   = Mux(ctrl_sub, input_xor, ~input_xor) // add clips when signs match, sub clips when signs mismatch
    val clip       = (vs2_sign ^ out_sign) & may_clip     // clips if the output sign doesn't match the input sign
    val clip_neg   = Cat(1.U, 0.U(((8 << eew)-1).W))
    val clip_pos   = ~clip_neg
    val clip_value = VecInit(vs2_sign.asBools.map(sign => Mux(sign, clip_neg, clip_pos))).asUInt
    (FillInterleaved((1 << eew), clip), clip_value)
  }).unzip
  val sat_signed_mask = VecInit(sat_signed_masks)(vd_eew)
  val sat_signed_clip = VecInit(sat_signed_clips)(vd_eew).asTypeOf(Vec(dLenB, UInt(8.W)))
  val sat_mask = Mux(sat_signed, sat_signed_mask, sat_unsigned_mask)
  val sat_clip = Mux(sat_signed, sat_signed_clip, sat_unsigned_clip)
  val sat_out = VecInit(add_out.zipWithIndex.map { case (o,i) => Mux(sat_mask(i), sat_clip(i), o) })

  val cmp_lt = VecInit(in2_bytes.zip(in1_bytes).map { x => x._1 < x._2 })
  val cmp_eq = VecInit(in2_bytes.zip(in1_bytes).map { x => x._1 === x._2 })
  val cmp_minmax = VecInit.tabulate(4)({eew =>
    val lts = cmp_lt.grouped(1 << eew)
    val eqs = cmp_eq.grouped(1 << eew)
    val bits = VecInit((lts zip eqs).zipWithIndex.map { case ((elts, eeqs), i) =>
      val eq = eeqs.andR
      val in1_hi = in1_bytes((i+1)*(1<<eew)-1)(7)
      val in2_hi = in2_bytes((i+1)*(1<<eew)-1)(7)
      val hi_lt = Mux(cmp_signed, in2_hi & !in1_hi, !in2_hi & in1_hi)
      val hi_eq = in1_hi === in2_hi
      val lt = ((elts :+ hi_lt) zip (eeqs :+ hi_eq)).foldLeft(false.B) { case (p, (l, e)) => l || (e && p) }
      Mux(cmp_less, lt || (cmp_sle & eq), cmp_inv ^ eq)
    }.toSeq).asUInt
    VecInit(Seq(Fill(1 << eew, bits), FillInterleaved(1 << eew, bits)))
  })(rvs1_eew)
  val cmp_res = cmp_minmax(0)
  val minmax_sel = cmp_minmax(1).asBools
  val minmax_out = VecInit(rvs1_bytes.zip(rvs2_bytes).zip(minmax_sel).map { case ((v1, v2), s) => Mux(s, v2, v1) }).asUInt

  val merge_mask = VecInit.tabulate(4)({eew => FillInterleaved(1 << eew, io.pipe(0).bits.rmask((dLenB >> eew)-1,0))})(rvs2_eew)
  val merge_out  = VecInit((0 until dLenB).map { i => Mux(merge_mask(i), rvs1_bytes(i), rvs2_bytes(i)) }).asUInt

  val carryborrow_res = VecInit.tabulate(4)({ eew =>
    Fill(1 << eew, VecInit(add_carry.grouped(1 << eew).map(_.last).toSeq).asUInt)
  })(rvs1_eew)
  val mask_out = Fill(8, Mux(ctrl_cmp, cmp_res, carryborrow_res ^ Fill(dLenB, ctrl_sub)))

  val adders = Module(new AdderArray)
  adders.io.in1 := Mux(ctrl_narrow_vs1, add_narrow_vs1, in1_bytes)
  adders.io.in2 := in2_bytes
  adders.io.use_carry  := add_use_carry
  adders.io.mask_carry := add_mask_carry
  adders.io.sub        := ctrl_sub
  adders.io.cmask      := ctrl_cmask
  add_out   := adders.io.out
  add_carry := adders.io.carry

  for (eew <- 0 until 3) {
    val in_vec = rvs1_bytes.asTypeOf(Vec(dLenB >> eew, UInt((8 << eew).W)))
    for (i <- 0 until dLenB >> (eew + 1)) {
      val hi_sel = Mux(ctrl_shift, (io.pipe(0).bits.eidx >> (dLenOffBits.U - rvs2_eew))(0), io.pipe(0).bits.wvd_eg(0))
      val lo = Mux(hi_sel, in_vec(i + (dLenB >> (eew + 1))), in_vec(i))
      val hi = Fill(16 << eew, lo((8 << eew)-1) && ctrl_add_sext)
      add_narrow_vs1_eew(eew)(i) := Cat(hi, lo)
    }

    val out_vec = add_out.asTypeOf(Vec(dLenB >> eew, UInt((8 << eew).W)))
    for (i <- 0 until dLenB >> eew) {
      val carry = add_carry(((i+1) << eew)-1)
      val hi1 = (ctrl_add_sext && rvs1_bytes(((i + 1) << eew) - 1)(7)) ^ ctrl_sub
      val hi2 = ctrl_add_sext && rvs2_bytes(((i + 1) << eew) - 1)(7)
      val hi = Mux(hi1 && hi2, ~(1.U((8 << eew).W)), Fill(8 << eew, hi1 ^ hi2))
      add_wide_out_eew(eew)(i) := Cat(hi + carry, out_vec(i))
    }
  }

  val shift_out = Wire(Vec(dLenB, UInt(8.W)))
  dontTouch(shift_out)
  val shift_narrowing_out = VecInit.tabulate(3)({eew =>
    Fill(2, VecInit(shift_out.grouped(2 << eew).map(_.take(1 << eew)).flatten.toSeq).asUInt)
  })(rvs1_eew)
  val shamt_mask = VecInit.tabulate(4)({eew => ((8 << eew) - 1).U})(rvs2_eew)(5,0)
  val shamt_arr = Mux(ctrl_narrow_vs1, add_narrow_vs1, rvs1_bytes)
  for (i <- 0 until dLenB) {
    val shamt = VecInit.tabulate(4)({ eew =>
      shamt_arr((i / (1 << eew)) << eew)
    })(rvs1_eew) & shamt_mask

    val shift_left_zero_mask = FillInterleaved(8, VecInit.tabulate(4)({eew =>
      ((1 << (1 + (i % (1 << eew)))) - 1).U(8.W)
    })(rvs2_eew))
    val shift_right_zero_mask = FillInterleaved(8, VecInit.tabulate(4)({eew =>
      (((1 << (1 << eew)) - 1) >> (i % (1 << eew))).U(8.W)
    })(rvs2_eew))
    val shift_hi = !ctrl_shift_left & shift_sra & VecInit.tabulate(4)({eew =>
      rvs2_bytes(((i/(1<<eew))+1)*(1<<eew) - 1)(7)
    })(rvs2_eew)
    val shift_left_in = Reverse(VecInit(rvs2_bytes.drop((i/8)*8).take(1+(i%8))).asUInt) & shift_left_zero_mask
    val shift_right_in = (VecInit(rvs2_bytes.drop(i).take(8-(i%8))).asUInt & shift_right_zero_mask) | Mux(shift_hi, ~shift_right_zero_mask, 0.U)
    val shift_in = Mux(ctrl_shift_left,
      shift_left_in,
      shift_right_in)

    val shifted = (Cat(shift_hi, shift_in).asSInt >> shamt).asUInt(7,0)
    shift_out(i) := Mux(ctrl_shift_left,
      Reverse(shifted),
      shifted)
  }

  val xunary0_eew_mul = io.pipe(0).bits.vd_eew - rvs2_eew
  val xunary0_in = (1 until 4).map { m =>
    val w = dLen >> m
    val in = Wire(UInt(w.W))
    val in_mul = io.pipe(0).bits.rvs2_data.asTypeOf(Vec(1 << m, UInt(w.W)))
    in := in_mul(io.pipe(0).bits.wvd_eg(m-1,0))
    in
  }
  val xunary0_out = Mux1H((1 until 4).map { vd_eew => (0 until vd_eew).map { vs2_eew =>
    (io.pipe(0).bits.vd_eew === vd_eew.U && rvs2_eew === vs2_eew.U) -> {
      val mul = vd_eew - vs2_eew
      val in = xunary0_in(mul-1).asTypeOf(Vec(dLenB >> vd_eew, UInt((8 << vs2_eew).W)))
      val out = Wire(Vec(dLenB >> vd_eew, UInt((8 << vd_eew).W)))
      out.zip(in).foreach { case (l, r) => l := Cat(
        Fill((8 << vd_eew) - (8 << vs2_eew), io.pipe(0).bits.rs1(0) && r((8 << vs2_eew)-1)),
        r)
      }
      out.asUInt
    }
  }}.flatten)

  val outs = Seq(
    (ctrl_xunary0        , xunary0_out),
    (ctrl_bw             , bw_out),
    (ctrl_mask_write     , mask_out),
    (ctrl_shift          , Mux(ctrl_narrow_vs1, shift_narrowing_out, shift_out.asUInt)),
    (ctrl_minmax         , minmax_out),
    (ctrl_merge          , merge_out),
    (ctrl_sat            , sat_out.asUInt)
  )
  val out = Mux(outs.map(_._1).orR, Mux1H(outs), add_out.asUInt)

  val mask_write_offset = VecInit.tabulate(4)({ eew =>
    Cat(io.pipe(0).bits.eidx(log2Ceil(dLen)-1, dLenOffBits-eew), 0.U((dLenOffBits-eew).W))
  })(rvs1_eew)
  val mask_write_mask = (VecInit.tabulate(4)({ eew =>
    VecInit(io.pipe(0).bits.wmask.asBools.grouped(1 << eew).map(_.head).toSeq).asUInt
  })(rvs1_eew) << mask_write_offset)(dLen-1,0)

  io.write.valid     := io.pipe(0).valid
  io.write.bits.eg   := io.pipe(0).bits.wvd_eg >> 1
  io.write.bits.mask := Fill(2, Mux(ctrl_mask_write, mask_write_mask, FillInterleaved(8, io.pipe(0).bits.wmask)))
  io.write.bits.data := Fill(2, out)

  io.set_vxsat := io.pipe(0).valid && ctrl_sat && sat_mask.orR

  when (io.pipe(0).bits.wvd_widen2) {
    io.write.bits.mask := FillInterleaved(8, FillInterleaved(2, io.pipe(0).bits.wmask))
    io.write.bits.data := add_wide_out
  }
}
