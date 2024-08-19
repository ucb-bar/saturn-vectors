package saturn.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import saturn.common._
import saturn.insns._

class ShiftBlock(w: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(w.W))
    val shamt = Input(UInt((1+log2Ceil(w)).W))
    val shl = Input(Bool())
    val sign = Input(Bool())
    val rm = Input(UInt(2.W))
    val out = Output(UInt(w.W))
    val round = Output(Bool())
  })
  val full_shifted = (Mux(io.shl,
    Cat(false.B, Reverse(io.in), false.B),
    Cat(io.sign, io.in, false.B)).asSInt >> io.shamt)(w,0).asUInt

  val shifted = full_shifted(w,1)

  io.out := Mux(io.shl, Reverse(shifted), shifted)
  io.round := RoundingIncrement(io.rm, shifted(0), full_shifted(0),
    Some(io.in & (((1.U << io.shamt) - 1.U) >> 1)(w-1,0)))
}

class ShiftUnit extends Module {
  val io = IO(new Bundle {
    val in_eew = Input(UInt(2.W))
    val in     = Input(UInt(64.W))
    val shamt  = Input(UInt(64.W))
    val rot    = Input(Bool())
    val shl    = Input(Bool())
    val signed = Input(Bool())
    val rm     = Input(UInt(2.W))

    val out = Output(UInt(64.W))
    val round = Output(UInt(8.W))
  })

  val shamt_mask = VecInit.tabulate(4)({eew => ~(0.U((log2Ceil(8) + eew).W))})(io.in_eew)

  val shift_out = Seq.tabulate(4) { sew => Wire(Vec(8 >> sew, UInt((8 << sew).W))) }
  val round_out = Seq.tabulate(4) { sew => Wire(Vec(8 >> sew, UInt((1 << sew).W))) }

  def sextElem(in: UInt, sew: Int, sext: Bool): UInt = Cat(Fill(65 - (8 << sew), sext && in((8 << sew)-1)), in((8 << sew)-1,0))(63,0)

  for (i <- 0 until 8) {
    val sews = (0 until 4).filter { sew => i < (8 >> sew) }
    val shifter = Module(new ShiftBlock(8 << (sews.max)))
    val rotator = Module(new ShiftBlock(8 << (sews.max)))
    shifter.io.in := Mux1H(sews
      .map { sew => (io.in_eew === sew.U, sextElem(io.in((i+1)*(8<<sew)-1,i*(8<<sew)), sew, io.signed)) })
    rotator.io.in := Mux1H(sews
      .map { sew => (io.in_eew === sew.U, io.in((i+1)*(8<<sew)-1,i*(8<<sew))) })
    val shamt = shamt_mask & Mux1H(sews
      .map { sew => (io.in_eew === sew.U, io.shamt((i+1)*(8<<sew)-1,i*(8<<sew))) })
    shifter.io.shamt := shamt
    rotator.io.shamt := (8.U << io.in_eew) - shamt
    shifter.io.shl := io.shl
    rotator.io.shl := !io.shl
    shifter.io.sign := io.signed && Mux1H(sews
      .map { sew => (io.in_eew === sew.U, io.in((i+1)*(8<<sew)-1)) })
    rotator.io.sign := false.B
    shifter.io.rm := io.rm
    rotator.io.rm := false.B
    //shifter.io.rot := io.rot
    sews.foreach { sew =>
      shift_out(sew)(i) := shifter.io.out | Mux(io.rot, rotator.io.out, 0.U)
      round_out(sew)(i) := shifter.io.round
    }
  }
  io.out   := Mux1H(UIntToOH(io.in_eew), shift_out.map(_.asUInt))
  io.round := Mux1H(UIntToOH(io.in_eew), round_out.map(_.asUInt))
}

class ShiftArray(dLenB: Int) extends Module {
  val dLen = dLenB * 8
  val io = IO(new Bundle {
    val in_eew    = Input(UInt(2.W))
    val in        = Input(UInt(dLen.W))
    val shamt     = Input(UInt(dLen.W))
    val rori_hi   = Input(Bool())
    val rot       = Input(Bool())
    val shl       = Input(Bool())
    val signed    = Input(Bool())
    val scaling   = Input(Bool())
    val rm        = Input(UInt(2.W))
    val narrowing = Input(Bool())

    val out = Output(Vec(dLenB, UInt(8.W)))
    val set_vxsat = Output(UInt(dLenB.W))
  })

  val shifted = Reg(Vec(dLenB, UInt(8.W)))
  val rounding_incrs = Reg(Vec(dLenB, Bool()))
  val in_eew_pipe = RegNext(io.in_eew)
  val signed_pipe = RegNext(io.signed)
  val scaling_pipe = RegNext(io.scaling)
  val narrowing_pipe = RegNext(io.narrowing)

  for (i <- 0 until (dLenB >> 3)) {
    val shifter = Module(new ShiftUnit)
    shifter.io.in_eew := io.in_eew
    shifter.io.in := io.in((i+1)*64-1,i*64)
    shifter.io.shamt := io.shamt((i+1)*64-1,i*64) | Mux(io.rori_hi, 0x20.U, 0.U)
    shifter.io.rot := io.rot
    shifter.io.shl := io.shl
    shifter.io.signed := io.signed
    shifter.io.rm := io.rm
    for (j <- 0 until 8) {
      shifted(i*8+j) := shifter.io.out((j+1)*8-1,j*8)
      rounding_incrs(i*8+j) := shifter.io.round(j)
    }
  }

  val scaling_array = Module(new AdderArray(dLenB))
  scaling_array.io.in1    := shifted
  scaling_array.io.in2.foreach(_ := 0.U)
  scaling_array.io.incr   := Mux(scaling_pipe, rounding_incrs, VecInit.fill(dLenB)(false.B))
  scaling_array.io.signed := DontCare
  scaling_array.io.eew    := in_eew_pipe
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
  })(in_eew_pipe - 1.U)
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
  val narrow_signed_mask = VecInit(narrow_signed_masks)(in_eew_pipe - 1.U)
  val narrow_signed_clip = VecInit(narrow_signed_clips)(in_eew_pipe - 1.U).asTypeOf(Vec(dLenB >> 1, UInt(8.W)))

  val narrow_mask = Mux(signed_pipe, narrow_signed_mask, narrow_unsigned_mask)
  val narrow_clip = Mux(signed_pipe, narrow_signed_clip, narrow_unsigned_clip)

  val narrow_out_clipped = VecInit(narrow_out_elems.map(e => VecInit(e).asUInt))(in_eew_pipe - 1.U)
    .asTypeOf(Vec(dLenB >> 1, UInt(8.W)))
    .zip(narrow_mask.asBools)
    .zip(narrow_clip).map ({ case ((o,s),c) => Mux(s && scaling_pipe, c, o) })
  val narrow_out = Fill(2, narrow_out_clipped.asUInt).asTypeOf(Vec(dLenB, UInt(8.W)))

  io.out := Mux(narrowing_pipe, narrow_out, scaling_array.io.out)
  io.set_vxsat := Mux(narrowing_pipe && scaling_pipe, Fill(2, narrow_mask), 0.U)
}

case object ShiftPipeFactory extends FunctionalUnitFactory {
  def insns = Seq(
    SLL.VV, SLL.VX, SLL.VI, SRL.VV, SRL.VX, SRL.VI, SRA.VV, SRA.VX, SRA.VI,
    NSRA.VV, NSRA.VX, NSRA.VI, NSRL.VV, NSRL.VX, NSRL.VI,
    NCLIPU.VV, NCLIPU.VX, NCLIPU.VI, NCLIP.VV, NCLIP.VX, NCLIP.VI,
    SSRL.VV, SSRL.VX, SSRL.VI, SSRA.VV, SSRA.VX, SSRA.VI,
    // Zvbb
    ROL.VV, ROL.VX, ROR.VV, ROR.VX, ROR.VI, RORI.VI, WSLL.VV, WSLL.VX, WSLL.VI
  ).map(_.pipelined(2))
  def generate(implicit p: Parameters) = new ShiftPipe()(p)
}

class ShiftPipe(implicit p: Parameters) extends PipelinedFunctionalUnit(2)(p) {
  val supported_insns = ShiftPipeFactory.insns

  val rvs1_eew = io.pipe(0).bits.rvs1_eew
  val rvs2_eew = io.pipe(0).bits.rvs2_eew
  val vd_eew   = io.pipe(0).bits.vd_eew

  val ctrl = new VectorDecoder(
    io.pipe(0).bits.funct3, io.pipe(0).bits.funct6, 0.U, 0.U,
    supported_insns,
    Seq(UsesShift, ShiftsLeft, ScalingShift))

  io.iss.ready := true.B

  val shift_narrowing = vd_eew < rvs2_eew
  val shift_widening = vd_eew > rvs2_eew

  val rvs1_bytes = io.pipe(0).bits.rvs1_data.asTypeOf(Vec(dLenB, UInt(8.W)))
  val rvs2_bytes = io.pipe(0).bits.rvs2_data.asTypeOf(Vec(dLenB, UInt(8.W)))
  val narrow_vs1 = narrow2_expand(rvs1_bytes, rvs1_eew,
    (io.pipe(0).bits.eidx >> (dLenOffBits.U - Mux(shift_narrowing, rvs2_eew, vd_eew)))(0), false.B)
  val narrow_vs2 = narrow2_expand(rvs2_bytes, rvs2_eew,
    (io.pipe(0).bits.eidx >> (dLenOffBits.U - vd_eew))(0), false.B)

  val shift_arr = Module(new ShiftArray(dLenB))
  shift_arr.io.in_eew    := Mux(shift_widening, vd_eew, rvs2_eew)
  shift_arr.io.in        := Mux(shift_widening, narrow_vs2, rvs2_bytes).asUInt
  shift_arr.io.shamt     := Mux(shift_narrowing || shift_widening, narrow_vs1, rvs1_bytes).asUInt
  shift_arr.io.rori_hi   := io.pipe(0).bits.opif6 === OPIFunct6.rol && io.pipe(0).bits.funct3 === OPIVI
  shift_arr.io.rot       := io.pipe(0).bits.opif6.isOneOf(OPIFunct6.rol, OPIFunct6.ror)
  shift_arr.io.shl       := ctrl.bool(ShiftsLeft)
  shift_arr.io.signed    := io.pipe(0).bits.funct6(0)
  shift_arr.io.rm        := io.pipe(0).bits.vxrm
  shift_arr.io.scaling   := ctrl.bool(ScalingShift)
  shift_arr.io.narrowing := shift_narrowing

  io.write.valid     := io.pipe(depth-1).valid
  io.write.bits.eg   := io.pipe(depth-1).bits.wvd_eg
  io.write.bits.mask := FillInterleaved(8, io.pipe(depth-1).bits.wmask)
  io.write.bits.data := shift_arr.io.out.asUInt

  val shift_vxsat = shift_arr.io.set_vxsat & io.pipe(depth-1).bits.wmask
  io.set_vxsat := io.pipe(depth-1).valid && (shift_vxsat =/= 0.U)
  io.set_fflags.valid := false.B
  io.set_fflags.bits := DontCare

  io.scalar_write.valid := false.B
  io.scalar_write.bits := DontCare
}
