package saturn.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import saturn.common._
import saturn.insns._

class AdderArray(dLenB: Int) extends Module {
  val io = IO(new Bundle {
    val in1 = Input(Vec(dLenB, UInt(8.W)))
    val in2 = Input(Vec(dLenB, UInt(8.W)))
    val incr = Input(Vec(dLenB, Bool()))
    val mask_carry = Input(UInt(dLenB.W))

    val signed    = Input(Bool())
    val eew       = Input(UInt(2.W))
    val avg       = Input(Bool())
    val rm        = Input(UInt(2.W))
    val sub       = Input(Bool())
    val cmask     = Input(Bool())

    val out   = Output(Vec(dLenB, UInt(8.W)))
    val carry = Output(Vec(dLenB, Bool()))
  })

  val use_carry = VecInit.tabulate(4)({ eew =>
    Fill(dLenB >> eew, ~(1.U((1 << eew).W)))
  })(io.eew)
  val carry_clear = Mux(io.avg, use_carry.asBools.map(Cat(~(0.U(8.W)), _)).asUInt, ~(0.U(73.W)))
  val carry_restore = Mux(io.avg, use_carry.asBools.map(Cat(0.U(8.W), _)).asUInt, 0.U(73.W))

  val avg_in1 = VecInit.tabulate(4) { eew =>
    VecInit(io.in1.asTypeOf(Vec(dLenB >> eew, UInt((8 << eew).W))).map(e => Cat(io.signed && e((8<<eew)-1), e) >> 1)).asUInt
  }(io.eew).asTypeOf(Vec(dLenB, UInt(8.W)))
  val avg_in2 = VecInit.tabulate(4) { eew =>
    VecInit(io.in2.asTypeOf(Vec(dLenB >> eew, UInt((8 << eew).W))).map(e => Cat(io.signed && e((8<<eew)-1), e) >> 1)).asUInt
  }(io.eew).asTypeOf(Vec(dLenB, UInt(8.W)))

  val in1 = Mux(io.avg, avg_in1, io.in1)
  val in2 = Mux(io.avg, avg_in2, io.in2)

  for (i <- 0 until (dLenB >> 3)) {
    val h = (i+1)*8-1
    val l = i*8
    val io_in1_slice = io.in1.slice(l,h+1)
    val io_in2_slice = io.in2.slice(l,h+1)
    val in1_slice = in1.slice(l,h+1)
    val in2_slice = in2.slice(l,h+1)
    val use_carry_slice = use_carry(h,l).asBools
    val mask_carry_slice = io.mask_carry(h,l).asBools
    val incr_slice = io.incr.slice(l,h+1)

    val in1_dummy_bits = (io_in1_slice
      .zip(io_in2_slice)
      .zip(use_carry_slice)
      .zip(mask_carry_slice).map { case(((i1, i2), carry), mask_bit) => {
        val avg_bit = ((io.sub ^ i1(0)) & i2(0)) | (((io.sub ^ i1(0)) ^ i2(0)) & io.sub)
        val bit = (!io.cmask & io.sub) | (io.cmask & (io.sub ^ mask_bit))
        Mux(carry, 1.U(1.W), Mux(io.avg, avg_bit, bit))
      }})
    val in2_dummy_bits = (io_in1_slice
      .zip(io_in2_slice)
      .zip(use_carry_slice)
      .zip(mask_carry_slice).map { case(((i1, i2), carry), mask_bit) => {
        val avg_bit = ((io.sub ^ i1(0)) & i2(0)) | (((io.sub ^ i1(0)) ^ i2(0)) & io.sub)
        val bit = (!io.cmask & io.sub) | (io.cmask & (io.sub ^ mask_bit))
        Mux(carry, 0.U(1.W), Mux(io.avg, avg_bit, bit))
      }})
    val round_incrs = (io_in1_slice
      .zip(io_in2_slice)
      .zipWithIndex.map { case((l, r), i) => {
        val sum = r(1,0) +& ((l(1,0) ^ Fill(2, io.sub)) +& io.sub)
        Cat(0.U(7.W), Cat(Mux(io.avg, RoundingIncrement(io.rm, sum(1), sum(0), None) & !use_carry_slice(i), 0.U), 0.U(1.W)))
      }}
      .asUInt)


    val in1_constructed = in1_slice.zip(in1_dummy_bits).map { case(i1, dummy_bit) => (i1 ^ Fill(8, io.sub)) ## dummy_bit }.asUInt
    val in2_constructed = in2_slice.zip(in2_dummy_bits).map { case(i2, dummy_bit) => i2 ## dummy_bit }.asUInt

    val incr_constructed = incr_slice.zip(use_carry_slice).map { case(incr, masking) => Cat(0.U(7.W), Cat(Mux(!masking, incr, 0.U(1.W)), 0.U(1.W))) }.asUInt

    val sum = (((in1_constructed +& in2_constructed) & carry_clear) | carry_restore) +& round_incrs +& incr_constructed

    for (j <- 0 until 8) {
      io.out((i*8) + j) := sum(((j+1)*9)-1, (j*9) + 1)
      io.carry((i*8) + j) := sum((j+1)*9)
    }
  }
}

class CompareArray(dLenB: Int) extends Module {
  val io = IO(new Bundle {
    val in1 = Input(Vec(dLenB, UInt(8.W)))
    val in2 = Input(Vec(dLenB, UInt(8.W)))
    val eew = Input(UInt(2.W))
    val signed = Input(Bool())
    val less   = Input(Bool())
    val sle    = Input(Bool())
    val inv    = Input(Bool())

    val minmax = Output(UInt(dLenB.W))
    val result = Output(UInt(dLenB.W))
  })

  val eq = io.in2.zip(io.in1).map { x => x._1 === x._2 }
  val lt = io.in2.zip(io.in1).map { x => x._1  <  x._2 }

  val minmax_bits = Wire(Vec(4, UInt(dLenB.W)))
  val result_bits  = Wire(Vec(4, UInt(dLenB.W)))

  io.minmax := minmax_bits(io.eew)
  io.result := result_bits(io.eew)

  for (eew <- 0 until 4) {
    val lts = lt.grouped(1 << eew)
    val eqs = eq.grouped(1 << eew)
    val bits = VecInit(lts.zip(eqs).zipWithIndex.map { case ((e_lts, e_eqs), i) =>
      val eq = e_eqs.andR
      val in1_hi = io.in1((i+1)*(1<<eew)-1)(7)
      val in2_hi = io.in2((i+1)*(1<<eew)-1)(7)
      val hi_lt = Mux(io.signed, in2_hi & !in1_hi, !in2_hi & in1_hi)
      val hi_eq = in1_hi === in2_hi
      val lt = (e_lts :+ hi_lt).zip(e_eqs :+ hi_eq).foldLeft(false.B) { case (p, (l, e)) => l || (e && p) }
      Mux(io.less, lt || (io.sle && eq), io.inv ^ eq)
    }.toSeq).asUInt
    minmax_bits(eew) := FillInterleaved(1 << eew, bits)
    result_bits(eew) := Fill(1 << eew, bits)
  }
}

class SaturatedSumArray(dLenB: Int) extends Module {
  val dLen = dLenB * 8
  val io = IO(new Bundle {
    val sum      = Input(Vec(dLenB, UInt(8.W)))
    val carry    = Input(Vec(dLenB, Bool()))
    val in1_sign = Input(Vec(dLenB, Bool()))
    val in2_sign = Input(Vec(dLenB, Bool()))
    val sub      = Input(Bool())
    val eew      = Input(UInt(2.W))
    val signed   = Input(Bool())

    val set_vxsat = Output(UInt(dLenB.W))
    val out       = Output(Vec(dLenB, UInt(8.W)))
  })

  val unsigned_mask = VecInit.tabulate(4)({ eew =>
    FillInterleaved(1 << eew, VecInit.tabulate(dLenB >> eew)(i => io.sub ^ io.carry(((i+1) << eew)-1)).asUInt)
  })(io.eew)
  val unsigned_clip = Mux(io.sub, 0.U(dLen.W), ~(0.U(dLen.W))).asTypeOf(Vec(dLenB, UInt(8.W)))

  val (signed_masks, signed_clips): (Seq[UInt], Seq[UInt]) = Seq.tabulate(4)({ eew =>
    val out_sign = VecInit.tabulate(dLenB >> eew)(i =>      io.sum(((i+1)<<eew)-1)(7)).asUInt
    val vs2_sign = VecInit.tabulate(dLenB >> eew)(i => io.in2_sign(((i+1)<<eew)-1)   ).asUInt
    val vs1_sign = VecInit.tabulate(dLenB >> eew)(i => io.in1_sign(((i+1)<<eew)-1)   ).asUInt
    val input_xor  = vs2_sign ^ vs1_sign
    val may_clip   = Mux(io.sub, input_xor, ~input_xor) // add clips when signs match, sub clips when signs mismatch
    val clip       = (vs2_sign ^ out_sign) & may_clip   // clips if the output sign doesn't match the input sign
    val clip_neg   = Cat(1.U, 0.U(((8 << eew)-1).W))
    val clip_pos   = ~clip_neg
    val clip_value = VecInit(vs2_sign.asBools.map(sign => Mux(sign, clip_neg, clip_pos))).asUInt
    (FillInterleaved((1 << eew), clip), clip_value)
  }).unzip
  val signed_mask = VecInit(signed_masks)(io.eew)
  val signed_clip = VecInit(signed_clips)(io.eew).asTypeOf(Vec(dLenB, UInt(8.W)))

  val mask = Mux(io.signed, signed_mask, unsigned_mask)
  val clip = Mux(io.signed, signed_clip, unsigned_clip)
  io.out := io.sum.zipWithIndex.map { case (o,i) => Mux(mask(i), clip(i), o) }
  io.set_vxsat := mask
}

case object IntegerPipeFactory extends FunctionalUnitFactory {
  def insns = Seq(
    ADD.VV, ADD.VX, ADD.VI, SUB.VV, SUB.VX, RSUB.VX, RSUB.VI,
    WADDU.VV, WADDU.VX, WADD.VV, WADD.VX, WSUBU.VV, WSUBU.VX, WSUB.VV, WSUB.VX,
    WADDUW.VV, WADDUW.VX, WADDW.VV, WADDW.VX, WSUBUW.VV, WSUBUW.VX, WSUBW.VV, WSUBW.VX,
    ADC.VV, ADC.VX, ADC.VI, MADC.VV, MADC.VX, MADC.VI,
    SBC.VV, SBC.VX, MSBC.VV, MSBC.VX,
    NEXT.VV,
    MSEQ.VV, MSEQ.VX, MSEQ.VI, MSNE.VV, MSNE.VX, MSNE.VI,
    MSLTU.VV, MSLTU.VX, MSLT.VV, MSLT.VX,
    MSLEU.VV, MSLEU.VX, MSLEU.VI, MSLE.VV, MSLE.VX, MSLE.VI,
    MSGTU.VX, MSGTU.VI, MSGT.VX, MSGT.VI,
    MINU.VV, MINU.VX, MIN.VV, MIN.VX,
    MAXU.VV, MAXU.VX, MAX.VV, MAX.VX,
    MERGE.VV, MERGE.VX, MERGE.VI,
    SADDU.VV, SADDU.VX, SADDU.VI, SADD.VV, SADD.VX, SADD.VI,
    SSUBU.VV, SSUBU.VX, SSUB.VV, SSUB.VX,
    AADDU.VV, AADDU.VX, AADD.VV, AADD.VX,
    ASUBU.VV, ASUBU.VX, ASUB.VV, ASUB.VX,
    REDSUM.VV, WREDSUM.VV, WREDSUMU.VV,
    REDMINU.VV, REDMIN.VV, REDMAXU.VV, REDMAX.VV,
    FMERGE.VF,
    // zvbb
    BREV8.VV, BREV.VV, REV8.VV, CLZ.VV, CTZ.VV, CPOP.VV
  ).map(_.pipelined(1))
  def generate(implicit p: Parameters) = new IntegerPipe()(p)
}

class IntegerPipe(implicit p: Parameters) extends PipelinedFunctionalUnit(1)(p) {
  val supported_insns = IntegerPipeFactory.insns

  val rvs1_eew = io.pipe(0).bits.rvs1_eew
  val rvs2_eew = io.pipe(0).bits.rvs2_eew
  val vd_eew   = io.pipe(0).bits.vd_eew

  val ctrl = new VectorDecoder(
    io.pipe(0).bits.funct3, io.pipe(0).bits.funct6, io.pipe(0).bits.rs1, io.pipe(0).bits.rs2,
    supported_insns,
    Seq(UsesCmp, UsesNarrowingSext, UsesMinMax, UsesMerge, UsesSat,
      DoSub, WideningSext, Averaging,
      CarryIn, AlwaysCarryIn, CmpLess, Swap12, WritesAsMask,
      UsesBitSwap, UsesCountZeros))

  io.iss.ready := new VectorDecoder(io.iss.op.funct3, io.iss.op.funct6, 0.U, 0.U, supported_insns, Nil).matched

  val carry_in = ctrl.bool(CarryIn) && (!io.pipe(0).bits.vm || ctrl.bool(AlwaysCarryIn))

  val sat_signed = io.pipe(0).bits.funct6(0)
  val sat_addu   = io.pipe(0).bits.funct6(1,0) === 0.U
  val sat_subu   = io.pipe(0).bits.funct6(1,0) === 2.U

  val rvs1_bytes = io.pipe(0).bits.rvs1_data.asTypeOf(Vec(dLenB, UInt(8.W)))
  val rvs2_bytes = io.pipe(0).bits.rvs2_data.asTypeOf(Vec(dLenB, UInt(8.W)))

  val in1_bytes = Mux(ctrl.bool(Swap12), rvs2_bytes, rvs1_bytes)
  val in2_bytes = Mux(ctrl.bool(Swap12), rvs1_bytes, rvs2_bytes)

  val narrow_vs1 = narrow2_expand(rvs1_bytes, rvs1_eew,
    (io.pipe(0).bits.eidx >> (dLenOffBits.U - vd_eew))(0),
    ctrl.bool(WideningSext))
  val narrow_vs2 = narrow2_expand(rvs2_bytes, rvs2_eew,
    (io.pipe(0).bits.eidx >> (dLenOffBits.U - vd_eew))(0),
    ctrl.bool(WideningSext))

  val add_mask_carry = VecInit.tabulate(4)({ eew =>
    VecInit((0 until dLenB >> eew).map { i => io.pipe(0).bits.rmask(i) | 0.U((1 << eew).W) }).asUInt
  })(rvs2_eew)
  val add_carry = Wire(Vec(dLenB, UInt(1.W)))
  val add_out = Wire(Vec(dLenB, UInt(8.W)))

  val merge_mask = VecInit.tabulate(4)({eew => FillInterleaved(1 << eew, io.pipe(0).bits.rmask((dLenB >> eew)-1,0))})(rvs2_eew)
  val merge_out  = VecInit((0 until dLenB).map { i => Mux(merge_mask(i), rvs1_bytes(i), rvs2_bytes(i)) }).asUInt

  val carryborrow_res = VecInit.tabulate(4)({ eew =>
    Fill(1 << eew, VecInit(add_carry.grouped(1 << eew).map(_.last).toSeq).asUInt)
  })(rvs1_eew)

  val adder_arr = Module(new AdderArray(dLenB))
  adder_arr.io.in1 := Mux(rvs1_eew < vd_eew, narrow_vs1, in1_bytes)
  adder_arr.io.in2 := Mux(rvs2_eew < vd_eew, narrow_vs2, in2_bytes)
  adder_arr.io.incr.foreach(_ := false.B)
  adder_arr.io.avg := ctrl.bool(Averaging)
  adder_arr.io.eew := vd_eew
  adder_arr.io.rm  := io.pipe(0).bits.vxrm
  adder_arr.io.mask_carry := add_mask_carry
  adder_arr.io.sub        := ctrl.bool(DoSub)
  adder_arr.io.cmask      := carry_in
  adder_arr.io.signed     := io.pipe(0).bits.funct6(0)
  add_out   := adder_arr.io.out
  add_carry := adder_arr.io.carry

  val cmp_arr = Module(new CompareArray(dLenB))
  cmp_arr.io.in1 := in1_bytes
  cmp_arr.io.in2 := in2_bytes
  cmp_arr.io.eew := rvs1_eew
  cmp_arr.io.signed := io.pipe(0).bits.funct6(0)
  cmp_arr.io.less   := ctrl.bool(CmpLess)
  cmp_arr.io.sle    := io.pipe(0).bits.funct6(2,1) === 2.U
  cmp_arr.io.inv    := io.pipe(0).bits.funct6(0)
  val minmax_out = VecInit(rvs1_bytes.zip(rvs2_bytes).zip(cmp_arr.io.minmax.asBools).map { case ((v1, v2), s) => Mux(s, v2, v1) }).asUInt

  val mask_out = Fill(8, Mux(ctrl.bool(UsesCmp), cmp_arr.io.result, carryborrow_res ^ Fill(dLenB, ctrl.bool(DoSub))))

  val sat_arr = Module(new SaturatedSumArray(dLenB))
  sat_arr.io.sum      := add_out
  sat_arr.io.carry    := add_carry
  sat_arr.io.in1_sign := rvs1_bytes.map(_(7))
  sat_arr.io.in2_sign := rvs2_bytes.map(_(7))
  sat_arr.io.sub      := ctrl.bool(DoSub)
  sat_arr.io.eew      := vd_eew
  sat_arr.io.signed   := io.pipe(0).bits.funct6(0)
  val sat_out = sat_arr.io.out.asUInt

  val narrowing_ext_eew_mul = io.pipe(0).bits.vd_eew - rvs2_eew
  val narrowing_ext_in = (1 until 4).map { m =>
    val w = dLen >> m
    val in = Wire(UInt(w.W))
    val in_mul = io.pipe(0).bits.rvs2_data.asTypeOf(Vec(1 << m, UInt(w.W)))
    val sel = (io.pipe(0).bits.eidx >> (dLenOffBits.U - vd_eew))(m-1,0)
    in := in_mul(sel)
    in
  }
  val narrowing_ext_out = Mux1H((1 until 4).map { eew => (0 until eew).map { vs2_eew =>
    (vd_eew === eew.U && rvs2_eew === vs2_eew.U) -> {
      val mul = eew - vs2_eew
      val in = narrowing_ext_in(mul-1).asTypeOf(Vec(dLenB >> eew, UInt((8 << vs2_eew).W)))
      val out = Wire(Vec(dLenB >> eew, UInt((8 << eew).W)))
      out.zip(in).foreach { case (l, r) => l := Cat(
        Fill((8 << eew) - (8 << vs2_eew), io.pipe(0).bits.rs1(0) && r((8 << vs2_eew)-1)),
        r)
      }
      out.asUInt
    }
  }}.flatten)

  val brev_bytes = VecInit(in2_bytes.map(b => Reverse(b))).asUInt
  val brev_elements = VecInit((0 until 4).map { eew =>
    VecInit(in2_bytes.asTypeOf(Vec(dLenB >> eew, UInt((8 << eew).W))).map(b => Reverse(b))).asUInt
  })(vd_eew)
  val rev8_elements = VecInit((0 until 4).map { eew =>
    VecInit(in2_bytes.asTypeOf(Vec(dLenB >> eew, Vec(1 << eew, UInt(8.W)))).map(b => VecInit(b.reverse))).asUInt
  })(vd_eew)
  val swap_out = Mux1H(Seq(
    (io.pipe(0).bits.rs1(1,0) === 0.U) -> brev_bytes,
    (io.pipe(0).bits.rs1(1,0) === 1.U) -> rev8_elements,
    (io.pipe(0).bits.rs1(1,0) === 2.U) -> brev_elements
  ))

  val tz_in = Mux(io.pipe(0).bits.rs1(0), in2_bytes, brev_elements.asTypeOf(Vec(dLenB, UInt(8.W))))
  val tz_8b = tz_in.map(b => (b === 0.U, (PriorityEncoderOH(1.U ## b) - 1.U)(7,0)))
  val tz_16b = tz_8b.grouped(2).toSeq.map(t =>
    (t.map(_._1).andR, Mux(t(0)._1, t(1)._2 ## ~(0.U(8.W)), t(0)._2))
  )
  val tz_32b = tz_16b.grouped(2).toSeq.map(t =>
    (t.map(_._1).andR, Mux(t(0)._1, t(1)._2 ## ~(0.U(16.W)), t(0)._2))
  )
  val tz_64b = tz_32b.grouped(2).toSeq.map(t =>
    (t.map(_._1).andR, Mux(t(0)._1, t(1)._2 ## ~(0.U(32.W)), t(0)._2))
  )
  val tz_out = WireInit(VecInit(
    VecInit(tz_8b.map(_._2)).asUInt,
    VecInit(tz_16b.map(_._2)).asUInt,
    VecInit(tz_32b.map(_._2)).asUInt,
    VecInit(tz_64b.map(_._2)).asUInt
  )(vd_eew).asTypeOf(Vec(dLenB, UInt(8.W))))

  val cpop_in = Mux(io.pipe(0).bits.rs1(1), in2_bytes, tz_out)
  val cpop_8b = cpop_in.map(b => PopCount(b))
  val cpop_16b = cpop_8b.grouped(2).toSeq.map(_.reduce(_ +& _))
  val cpop_32b = cpop_16b.grouped(2).toSeq.map(_.reduce(_ +& _))
  val cpop_64b = cpop_32b.grouped(2).toSeq.map(_.reduce(_ +& _))
  val cpops = Seq(cpop_8b, cpop_16b, cpop_32b, cpop_64b)
  val count_out = WireInit(VecInit((0 until 4).map { eew =>
    val out = Wire(Vec(dLenB >> eew, UInt((8 << eew).W)))
    out := VecInit(cpops(eew))
    out.asUInt
  })(vd_eew))

  val outs = Seq(
    (ctrl.bool(UsesNarrowingSext)        , narrowing_ext_out),
    (ctrl.bool(WritesAsMask)             , mask_out),
    (ctrl.bool(UsesMinMax)               , minmax_out),
    (ctrl.bool(UsesMerge)                , merge_out),
    (ctrl.bool(UsesSat)                  , sat_out),
    (ctrl.bool(UsesBitSwap)              , swap_out),
    (ctrl.bool(UsesCountZeros)           , count_out)
  )
  val out = Mux(outs.map(_._1).orR, Mux1H(outs), add_out.asUInt)

  val mask_write_offset = VecInit.tabulate(4)({ eew =>
    Cat(io.pipe(0).bits.eidx(log2Ceil(dLen)-1, dLenOffBits-eew), 0.U((dLenOffBits-eew).W))
  })(rvs1_eew)
  val mask_write_mask = (VecInit.tabulate(4)({ eew =>
    VecInit(io.pipe(0).bits.wmask.asBools.grouped(1 << eew).map(_.head).toSeq).asUInt
  })(rvs1_eew) << mask_write_offset)(dLen-1,0)

  io.pipe0_stall     := false.B
  io.write.valid     := io.pipe(0).valid
  io.write.bits.eg   := io.pipe(0).bits.wvd_eg
  io.write.bits.mask := Mux(ctrl.bool(WritesAsMask), mask_write_mask, FillInterleaved(8, io.pipe(0).bits.wmask))
  io.write.bits.data := out

  val sat_vxsat   = Mux(ctrl.bool(UsesSat)  , sat_arr.io.set_vxsat  , 0.U) & io.pipe(0).bits.wmask
  io.set_vxsat := io.pipe(0).valid && (sat_vxsat =/= 0.U)
  io.set_fflags.valid := false.B
  io.set_fflags.bits := DontCare

  io.scalar_write.valid := false.B
  io.scalar_write.bits := DontCare
}
