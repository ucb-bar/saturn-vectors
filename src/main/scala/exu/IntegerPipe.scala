package vector.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import vector.common._

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

  val round_incrs = io.in1.zip(io.in2).zipWithIndex.map{ case((l, r), i) =>
    val sum = r(1,0) +& ((l(1,0) ^ Fill(2, io.sub)) +& io.sub)
    Cat(0.U(7.W), Cat(Mux(io.avg, RoundingIncrement(io.rm, sum(1), sum(0), None) & !use_carry(i), 0.U), 0.U(1.W)))
  }.asUInt

  val in1_dummy_bits = Wire(Vec(dLenB/8, UInt(8.W)))
  val in2_dummy_bits = Wire(Vec(dLenB/8, UInt(8.W)))

  for (i <- 0 until (dLenB >> 3)) {

    in1_dummy_bits(i) := io.in1.zip(io.in2).zip(use_carry.asBools).zip(io.mask_carry((i*8)+7,i*8).asBools).map { case(((i1, i2), carry), mask_bit) => 
      Mux(carry, 1.U(1.W), Mux(io.avg, ((io.sub ^ i1(0)) & i2(0)) | (((io.sub ^ i1(0)) ^ i2(0)) & io.sub), (!io.cmask & io.sub) | (io.cmask & (io.sub ^ mask_bit)))) 
    }.asUInt
    in2_dummy_bits(i) := io.in1.zip(io.in2).zip(use_carry.asBools).zip(io.mask_carry((i*8)+7,i*8).asBools).map { case(((i1, i2), carry), mask_bit) => 
      Mux(carry, 0.U(1.W), Mux(io.avg, ((io.sub ^ i1(0)) & i2(0)) | (((io.sub ^ i1(0)) ^ i2(0)) & io.sub), (!io.cmask & io.sub) | (io.cmask & (io.sub ^ mask_bit))))
    }.asUInt

    val in1_constructed = in1.zip(in1_dummy_bits(i).asBools).map{ case(i1, dummy_bit) => (i1 ^ Fill(8, io.sub)) ## dummy_bit }.asUInt
    val in2_constructed = in2.zip(in2_dummy_bits(i).asBools).map{ case(i2, dummy_bit) => i2 ## dummy_bit }.asUInt

    val incr_constructed = io.incr.zip(use_carry.asBools).map{ case(incr, masking) => Cat(0.U(7.W), Cat(Mux(!masking, incr, 0.U(1.W)), 0.U(1.W))) }.asUInt

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

class ShiftArray(dLenB: Int) extends Module {
  val dLen = dLenB * 8
  val io = IO(new Bundle {
    val in_eew    = Input(UInt(2.W))
    val in        = Input(Vec(dLenB, UInt(8.W)))
    val shamt     = Input(Vec(dLenB, UInt(8.W)))
    val shl       = Input(Bool())
    val signed    = Input(Bool())
    val scaling   = Input(Bool())
    val rm        = Input(UInt(2.W))
    val narrowing = Input(Bool())

    val out = Output(Vec(dLenB, UInt(8.W)))
    val set_vxsat = Output(Bool())
  })

  val shamt_mask = VecInit.tabulate(4)({eew => ~(0.U((log2Ceil(8) + eew).W))})(io.in_eew)
  val shifted_right = Wire(Vec(dLenB, UInt(8.W)))
  val shifted_left  = Wire(Vec(dLenB, UInt(8.W)))
  val rounding_incrs = Wire(Vec(dLenB, Bool()))

  for (i <- 0 until dLenB) {
    val shamt = VecInit.tabulate(4)({ eew => io.shamt((i / (1 << eew)) << eew) })(io.in_eew) & shamt_mask
    val shift_left_zero_mask = FillInterleaved(8, VecInit.tabulate(4)({eew =>
      ((1 << (1 + (i % (1 << eew)))) - 1).U(8.W)
    })(io.in_eew))
    val shift_right_zero_mask = FillInterleaved(8, VecInit.tabulate(4)({eew =>
      (((1 << (1 << eew)) - 1) >> (i % (1 << eew))).U(8.W)
    })(io.in_eew))
    val shift_hi = !io.shl & io.signed & VecInit.tabulate(4)({eew =>
      io.in(((i/(1<<eew))+1)*(1<<eew) - 1)(7)
    })(io.in_eew)
    val shift_left_in = Reverse(VecInit(io.in.drop((i/8)*8).take(1+(i%8))).asUInt) & shift_left_zero_mask
    val shift_right_in = (VecInit(io.in.drop(i).take(8-(i%8))).asUInt & shift_right_zero_mask) | Mux(shift_hi, ~shift_right_zero_mask, 0.U)
    val shift_in = Mux(io.shl,
      shift_left_in,
      shift_right_in)

    val full_shifted = (Cat(shift_hi, shift_in, false.B).asSInt >> shamt).asUInt
    val shifted = full_shifted(8,1)
    shifted_right(i) := shifted
    shifted_left(i)  := Reverse(shifted)

    rounding_incrs(i) := RoundingIncrement(io.rm, shifted(0), full_shifted(0),
      Some(shift_right_in & (((1.U << shamt) - 1.U) >> 1)(63,0)))
  }
  
  val scaling_array = Module(new AdderArray(dLenB))
  scaling_array.io.in1    := shifted_right
  scaling_array.io.in2.foreach(_ := 0.U)
  scaling_array.io.incr   := Mux(io.scaling, rounding_incrs, VecInit.fill(dLenB)(false.B))
  scaling_array.io.signed := DontCare
  scaling_array.io.eew    := io.in_eew
  scaling_array.io.avg    := false.B
  scaling_array.io.rm     := DontCare
  scaling_array.io.sub    := false.B
  scaling_array.io.cmask  := false.B
  scaling_array.io.mask_carry := DontCare

  val narrow_out_elems: Seq[Seq[UInt]] = Seq.tabulate(3)({eew =>
    scaling_array.io.out.grouped(2 << eew).map(e => VecInit(e.take(1 << eew)).asUInt).toSeq
  })
  val narrow_out_his: Seq[Seq[UInt]] = Seq.tabulate(3)({eew =>
    scaling_array.io.out.grouped(2 << eew).map(e => VecInit(e.drop(1 << eew)).asUInt).toSeq
  })
  val narrow_out_carries = Seq.tabulate(3)({eew =>
    scaling_array.io.carry.grouped(2 << eew).map(_.last).toSeq
  })

  val narrow_unsigned_mask = VecInit.tabulate(3)({ eew =>
    FillInterleaved(1 << eew, VecInit.tabulate(dLenB >> (eew + 1))(i =>
      Cat(narrow_out_carries(eew)(i), narrow_out_his(eew)(i)) =/= 0.U
    ).asUInt)
  })(io.in_eew - 1.U)
  val narrow_unsigned_clip = (~(0.U((dLen >> 1).W))).asTypeOf(Vec(dLenB >> 1, UInt(8.W)))

  val (narrow_signed_masks, narrow_signed_clips): (Seq[UInt], Seq[UInt]) = Seq.tabulate(3)({ eew =>
    val signs = narrow_out_his(eew).map(_((8 << eew)-1))
    val his   = narrow_out_his(eew).zip(narrow_out_elems(eew)).map({ case (h,e) => Cat(h((8 << eew)-2,0), e((8<<eew)-1)) })
    val clip_lo   = signs.zip(his).map({ case (s,h) =>  s && h =/= ~0.U((8 << eew).W) })
    val clip_hi   = signs.zip(his).map({ case (s,h) => !s && h =/=  0.U((8 << eew).W) })
    val clip_neg  = Cat(1.U, 0.U(((8 << eew)-1).W))
    val clip_pos  = ~clip_neg
    val clip_value = VecInit(signs.map(s => Mux(s, clip_neg, clip_pos))).asUInt
    val clip = clip_lo.zip(clip_hi).map(t => t._1 || t._2)
    (FillInterleaved((1 << eew), clip), clip_value)
  }).unzip
  val narrow_signed_mask = VecInit(narrow_signed_masks)(io.in_eew - 1.U)
  val narrow_signed_clip = VecInit(narrow_signed_clips)(io.in_eew - 1.U).asTypeOf(Vec(dLenB >> 1, UInt(8.W)))

  val narrow_mask = Mux(io.signed, narrow_signed_mask, narrow_unsigned_mask)
  val narrow_clip = Mux(io.signed, narrow_signed_clip, narrow_unsigned_clip)

  val narrow_out_clipped = VecInit(narrow_out_elems.map(e => VecInit(e).asUInt))(io.in_eew - 1.U)
    .asTypeOf(Vec(dLenB >> 1, UInt(8.W)))
    .zip(narrow_mask.asBools)
    .zip(narrow_clip).map ({ case ((o,s),c) => Mux(s && io.scaling, c, o) })
  val narrow_out = Fill(2, narrow_out_clipped.asUInt).asTypeOf(Vec(dLenB, UInt(8.W)))

  io.out := Mux(io.narrowing,
    narrow_out,
    Mux(io.scaling,
      scaling_array.io.out,
      Mux(io.shl, shifted_left, shifted_right))
  )
  io.set_vxsat := io.narrowing && io.scaling && narrow_mask =/= 0.U
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

    val set_vxsat = Output(Bool())
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
  io.set_vxsat := mask.orR
}

class IntegerPipe(implicit p: Parameters) extends PipelinedFunctionalUnit(1)(p) {
  io.iss.sub_dlen := 0.U

  val rvs1_eew = io.pipe(0).bits.rvs1_eew
  val rvs2_eew = io.pipe(0).bits.rvs2_eew
  val vd_eew   = io.pipe(0).bits.vd_eew

  lazy val ctrl_table = Seq(
    (OPIFunct6.add     , Seq(N,X,N,X,N,X,N,X,N)),
    (OPIFunct6.sub     , Seq(Y,X,N,X,N,X,N,X,N)),
    (OPIFunct6.rsub    , Seq(Y,X,N,X,N,X,Y,X,N)),
    (OPMFunct6.waddu   , Seq(N,N,N,X,N,X,N,X,N)),
    (OPMFunct6.wadd    , Seq(N,Y,N,X,N,X,N,X,N)),
    (OPMFunct6.wsubu   , Seq(Y,N,N,X,N,X,N,X,N)),
    (OPMFunct6.wsub    , Seq(Y,Y,N,X,N,X,N,X,N)),
    (OPMFunct6.wadduw  , Seq(N,N,N,X,N,X,N,X,N)),
    (OPMFunct6.waddw   , Seq(N,Y,N,X,N,X,N,X,N)),
    (OPMFunct6.wsubuw  , Seq(Y,N,N,X,N,X,N,X,N)),
    (OPMFunct6.wsubw   , Seq(Y,Y,N,X,N,X,N,X,N)),
    (OPIFunct6.adc     , Seq(N,X,N,X,N,X,N,X,N)),
    (OPIFunct6.madc    , Seq(N,X,N,X,Y,N,N,X,N)),
    (OPIFunct6.sbc     , Seq(Y,X,N,X,N,X,N,X,N)),
    (OPIFunct6.msbc    , Seq(Y,X,N,X,Y,N,N,X,N)),
    (OPMFunct6.xunary0 , Seq(X,X,N,X,N,X,X,X,N)),
    (OPIFunct6.sll     , Seq(X,X,Y,Y,N,X,X,X,N)),
    (OPIFunct6.sra     , Seq(X,X,Y,N,N,X,X,X,N)),
    (OPIFunct6.srl     , Seq(X,N,Y,N,N,X,X,X,N)),
    (OPIFunct6.nsra    , Seq(X,N,Y,N,N,X,X,X,N)),
    (OPIFunct6.nsrl    , Seq(X,N,Y,N,N,X,X,X,N)),
    (OPIFunct6.mseq    , Seq(X,X,N,X,Y,Y,X,N,N)),
    (OPIFunct6.msne    , Seq(X,X,N,X,Y,Y,X,N,N)),
    (OPIFunct6.msltu   , Seq(X,X,N,X,Y,Y,N,Y,N)),
    (OPIFunct6.mslt    , Seq(X,X,N,X,Y,Y,N,Y,N)),
    (OPIFunct6.msleu   , Seq(X,X,N,X,Y,Y,N,Y,N)),
    (OPIFunct6.msle    , Seq(X,X,N,X,Y,Y,N,Y,N)),
    (OPIFunct6.msgtu   , Seq(X,X,N,X,Y,Y,Y,Y,N)),
    (OPIFunct6.msgt    , Seq(X,X,N,X,Y,Y,Y,Y,N)),
    (OPIFunct6.minu    , Seq(X,X,N,X,N,X,N,Y,N)),
    (OPIFunct6.min     , Seq(X,X,N,X,N,X,N,Y,N)),
    (OPIFunct6.maxu    , Seq(X,X,N,X,N,X,Y,Y,N)),
    (OPIFunct6.max     , Seq(X,X,N,X,N,X,Y,Y,N)),
    (OPIFunct6.merge   , Seq(X,X,N,X,N,X,N,X,N)),
    (OPIFunct6.saddu   , Seq(N,X,N,X,N,X,N,X,N)),
    (OPIFunct6.sadd    , Seq(N,X,N,X,N,X,N,X,N)),
    (OPIFunct6.ssubu   , Seq(Y,X,N,X,N,X,N,X,N)),
    (OPIFunct6.ssub    , Seq(Y,X,N,X,N,X,N,X,N)),
    (OPMFunct6.aadd    , Seq(N,X,N,X,N,X,N,X,Y)),
    (OPMFunct6.aaddu   , Seq(N,X,N,X,N,X,N,X,Y)),
    (OPMFunct6.asub    , Seq(Y,X,N,X,N,X,N,X,Y)),
    (OPMFunct6.asubu   , Seq(Y,X,N,X,N,X,N,X,Y)),
    (OPIFunct6.ssrl    , Seq(X,X,Y,N,N,X,X,X,N)),
    (OPIFunct6.ssra    , Seq(X,X,Y,N,N,X,X,X,N)),
    (OPIFunct6.nclip   , Seq(X,N,Y,N,N,X,X,X,N)),
    (OPIFunct6.nclipu  , Seq(X,N,Y,N,N,X,X,X,N)),
    (OPMFunct6.redsum  , Seq(N,X,N,X,N,X,N,X,N)),
    (OPIFunct6.wredsum , Seq(N,Y,N,X,N,X,N,X,N)),
    (OPIFunct6.wredsumu, Seq(N,N,N,X,N,X,N,X,N)),
    (OPMFunct6.redminu , Seq(X,X,N,X,N,X,N,Y,N)),
    (OPMFunct6.redmin  , Seq(X,X,N,X,N,X,N,Y,N)),
    (OPMFunct6.redmaxu , Seq(X,X,N,X,N,X,Y,Y,N)),
    (OPMFunct6.redmax  , Seq(X,X,N,X,N,X,Y,Y,N)),
    (OPFFunct6.fmerge  , Seq(X,X,N,X,N,X,N,X,N)),
  )
  def accepts(f3: UInt, f6: UInt): Bool = VecDecode(f3, f6, ctrl_table.map(_._1))
  io.iss.ready := accepts(io.iss.op.funct3, io.iss.op.funct6)
  val ctrl_sub :: ctrl_add_sext :: ctrl_shift :: ctrl_shift_left :: ctrl_mask_write :: ctrl_cmp :: ctrl_rev12 :: cmp_less :: ctrl_avg :: Nil = VecDecode.applyBools(
    io.pipe(0).bits.funct3, io.pipe(0).bits.funct6,
    Seq.fill(9)(X), ctrl_table)
  val ctrl_cmask = (
    io.pipe(0).bits.opif6.isOneOf(OPIFunct6.adc, OPIFunct6.sbc) ||
    ((io.pipe(0).bits.opif6.isOneOf(OPIFunct6.madc, OPIFunct6.msbc)) && !io.pipe(0).bits.vm)
  )

  val ctrl_rsub = io.pipe(0).bits.opif6 === OPIFunct6.rsub
  val ctrl_xunary0 = io.pipe(0).bits.opmf6 === OPMFunct6.xunary0
  val ctrl_minmax = io.pipe(0).bits.opif6.isOneOf(OPIFunct6.minu, OPIFunct6.min, OPIFunct6.maxu, OPIFunct6.max) || io.pipe(0).bits.opmf6.isOneOf(OPMFunct6.redmin, OPMFunct6.redminu, OPMFunct6.redmax, OPMFunct6.redmaxu)
  val ctrl_merge = io.pipe(0).bits.opif6 === OPIFunct6.merge || io.pipe(0).bits.opff6 === OPFFunct6.fmerge
  val ctrl_sat = io.pipe(0).bits.opif6.isOneOf(OPIFunct6.saddu, OPIFunct6.sadd, OPIFunct6.ssubu, OPIFunct6.ssub)

  val sat_signed = io.pipe(0).bits.funct6(0)
  val sat_addu   = io.pipe(0).bits.funct6(1,0) === 0.U
  val sat_subu   = io.pipe(0).bits.funct6(1,0) === 2.U

  val rvs1_bytes = io.pipe(0).bits.rvs1_data.asTypeOf(Vec(dLenB, UInt(8.W)))
  val rvs2_bytes = io.pipe(0).bits.rvs2_data.asTypeOf(Vec(dLenB, UInt(8.W)))

  val in1_bytes = Mux(ctrl_rev12, rvs2_bytes, rvs1_bytes)
  val in2_bytes = Mux(ctrl_rev12, rvs1_bytes, rvs2_bytes)

  def narrow2_expand(bits: Seq[UInt], eew: UInt, upper: Bool, sext: Bool): Vec[UInt] = {
    val narrow_eew = (0 until 3).map { eew => Wire(Vec(dLenB >> (eew + 1), UInt((16 << eew).W))) }
    for (eew <- 0 until 3) {
      val in_vec = bits.grouped(1 << eew).map(g => VecInit(g).asUInt).toSeq
      for (i <- 0 until dLenB >> (eew + 1)) {
        val lo = Mux(upper, in_vec(i + (dLenB >> (eew + 1))), in_vec(i))
        val hi = Fill(16 << eew, lo((8 << eew)-1) && sext)
        narrow_eew(eew)(i) := Cat(hi, lo)
      }
    }
    VecInit(narrow_eew.map(_.asUInt))(eew).asTypeOf(Vec(dLenB, UInt(8.W)))
  }

  val narrow_vs1 = narrow2_expand(rvs1_bytes, rvs1_eew,
    (io.pipe(0).bits.eidx >> (dLenOffBits.U - Mux(ctrl_shift, rvs2_eew, vd_eew)))(0),
    ctrl_add_sext)
  val narrow_vs2 = narrow2_expand(rvs2_bytes, rvs2_eew,
    (io.pipe(0).bits.eidx >> (dLenOffBits.U - vd_eew))(0),
    ctrl_add_sext)

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
  adder_arr.io.avg := ctrl_avg
  adder_arr.io.eew := vd_eew
  adder_arr.io.rm  := io.pipe(0).bits.vxrm
  adder_arr.io.mask_carry := add_mask_carry
  adder_arr.io.sub        := ctrl_sub
  adder_arr.io.cmask      := ctrl_cmask
  adder_arr.io.signed     := io.pipe(0).bits.funct6(0)
  add_out   := adder_arr.io.out
  add_carry := adder_arr.io.carry

  val cmp_arr = Module(new CompareArray(dLenB))
  cmp_arr.io.in1 := in1_bytes
  cmp_arr.io.in2 := in2_bytes
  cmp_arr.io.eew := rvs1_eew
  cmp_arr.io.signed := io.pipe(0).bits.funct6(0)
  cmp_arr.io.less   := cmp_less
  cmp_arr.io.sle    := io.pipe(0).bits.funct6(2,1) === 2.U
  cmp_arr.io.inv    := io.pipe(0).bits.funct6(0)
  val minmax_out = VecInit(rvs1_bytes.zip(rvs2_bytes).zip(cmp_arr.io.minmax.asBools).map { case ((v1, v2), s) => Mux(s, v2, v1) }).asUInt

  val mask_out = Fill(8, Mux(ctrl_cmp, cmp_arr.io.result, carryborrow_res ^ Fill(dLenB, ctrl_sub)))

  val shift_narrowing = io.pipe(0).bits.opif6.isOneOf(OPIFunct6.nclip, OPIFunct6.nclipu, OPIFunct6.nsra, OPIFunct6.nsrl)
  val shift_arr = Module(new ShiftArray(dLenB))
  shift_arr.io.in_eew := rvs2_eew
  shift_arr.io.in     := rvs2_bytes
  shift_arr.io.shamt     := Mux(shift_narrowing, narrow_vs1, rvs1_bytes)
  shift_arr.io.shl       := ctrl_shift_left
  shift_arr.io.signed    := io.pipe(0).bits.funct6(0)
  shift_arr.io.rm        := io.pipe(0).bits.vxrm
  shift_arr.io.scaling   := io.pipe(0).bits.opif6.isOneOf(OPIFunct6.ssra, OPIFunct6.ssrl, OPIFunct6.nclip, OPIFunct6.nclipu)
  shift_arr.io.narrowing := shift_narrowing

  val shift_out = shift_arr.io.out.asUInt

  val sat_arr = Module(new SaturatedSumArray(dLenB))
  sat_arr.io.sum      := add_out
  sat_arr.io.carry    := add_carry
  sat_arr.io.in1_sign := rvs1_bytes.map(_(7))
  sat_arr.io.in2_sign := rvs2_bytes.map(_(7))
  sat_arr.io.sub      := ctrl_sub
  sat_arr.io.eew      := vd_eew
  sat_arr.io.signed   := io.pipe(0).bits.funct6(0)
  val sat_out = sat_arr.io.out.asUInt

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
    (ctrl_mask_write     , mask_out),
    (ctrl_shift          , shift_out),
    (ctrl_minmax         , minmax_out),
    (ctrl_merge          , merge_out),
    (ctrl_sat            , sat_out)
  )
  val out = Mux(outs.map(_._1).orR, Mux1H(outs), add_out.asUInt)

  val mask_write_offset = VecInit.tabulate(4)({ eew =>
    Cat(io.pipe(0).bits.eidx(log2Ceil(dLen)-1, dLenOffBits-eew), 0.U((dLenOffBits-eew).W))
  })(rvs1_eew)
  val mask_write_mask = (VecInit.tabulate(4)({ eew =>
    VecInit(io.pipe(0).bits.wmask.asBools.grouped(1 << eew).map(_.head).toSeq).asUInt
  })(rvs1_eew) << mask_write_offset)(dLen-1,0)

  io.write.valid     := io.pipe(0).valid
  io.write.bits.eg   := io.pipe(0).bits.wvd_eg
  io.write.bits.mask := Mux(ctrl_mask_write, mask_write_mask, FillInterleaved(8, io.pipe(0).bits.wmask))
  io.write.bits.data := out

  io.set_vxsat := io.pipe(0).valid && ((ctrl_sat && sat_arr.io.set_vxsat) || (ctrl_shift && shift_arr.io.set_vxsat))
  io.set_fflags.valid := false.B
  io.set_fflags.bits := DontCare

  io.scalar_write.valid := false.B
  io.scalar_write.bits := DontCare
}
