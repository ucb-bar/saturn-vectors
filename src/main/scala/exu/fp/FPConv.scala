package saturn.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import saturn.common._
import saturn.insns._
import hardfloat._

case object FPConvFactory extends FunctionalUnitFactory {
  def insns = Seq(FCVT_SGL, FCVT_NRW, FCVT_WID).map(_.pipelined(3))
  def generate(implicit p: Parameters) = new FPConvPipe()(p)
}

class FPConvBlock(implicit p: Parameters) extends CoreModule()(p) with HasFPUParameters {
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val in = Input(UInt(64.W))
    val in_eew = Input(UInt(2.W))
    val widen = Input(Bool())
    val narrow = Input(Bool())
    val signed = Input(Bool())
    val frm = Input(UInt(2.W))
    val i2f = Input(Bool())
    val f2i = Input(Bool())
    val truncating = Input(Bool())
    val rto = Input(Bool())

    val out = Output(UInt(64.W))
    val exc = Output(Vec(8, UInt(FPConstants.FLAGS_SZ.W)))
  })

  def f2raw(t: FType, in: UInt) = rawFloatFromFN(t.exp, t.sig, in)
  def raw2raw(t: FType, in: RawFloat) = {
    val out = WireInit(resizeRawFloat(t.exp, t.sig, in))
    // workaround bug in resizeRawFloat
    when (in.isNaN || in.isInf) {
      out.sExp := ((1 << t.exp) + (1 << (t.exp - 1))).U(t.exp,0).zext
    }
    out
  }
  def raw2rec(t: FType, rawIn: RawFloat) = {
    (rawIn.sign ##
      (Mux(rawIn.isZero, 0.U(3.W), rawIn.sExp(t.exp, t.exp - 2)) |
        Mux(rawIn.isNaN, 1.U, 0.U)) ##
      rawIn.sExp(t.exp - 3, 0) ##
      rawIn.sig(t.sig - 2, 0)
    )
  }

  val out_eew = Mux(io.widen, io.in_eew + 1.U, Mux(io.narrow, io.in_eew - 1.U, io.in_eew))

  val in64 = Seq(io.in)
  val in32 = io.in.asTypeOf(Vec(2, UInt(32.W)))
  val in16 = io.in.asTypeOf(Vec(4, UInt(16.W)))

  val raw64 = VecInit(Seq(f2raw(FType.D, io.in)))
  val raw32 = VecInit(in32.map(u => f2raw(FType.S, u)))
  val raw16 = VecInit(in16.map(u => f2raw(FType.H, u)))

  val raw32as64 = VecInit(raw32.map(r => raw2raw(FType.D, r)))
  val raw16as32 = VecInit(raw16.map(r => raw2raw(FType.S, r)))
  val raw16as64 = VecInit(raw16.map(r => raw2raw(FType.D, r)))

  dontTouch(raw64)
  dontTouch(raw32)
  dontTouch(raw16)
  dontTouch(raw32as64)
  dontTouch(raw16as32)
  dontTouch(raw16as64)

  val d2i = Seq(Module(new RecFNToINDynamic(FType.D.exp, FType.D.sig, Seq(16, 32, 64))))
  val s2i = Seq(Module(new RecFNToINDynamic(FType.S.exp, FType.S.sig, Seq(16, 32))))
  val h2i = Seq.fill(2)(Module(new RecFNToINDynamic(FType.H.exp, FType.H.sig, Seq(16))))

  d2i(0).io.in := raw2rec(FType.D,
    Mux(io.in_eew === 3.U, raw64(0), Mux(io.in_eew === 2.U, raw32as64(0), raw16as64(0)))
  )
  s2i(0).io.in := raw2rec(FType.S,
    Mux(io.in_eew === 2.U, raw32(1), raw16as32(2))
  )
  h2i(0).io.in := raw2rec(FType.H, raw16(1))
  h2i(1).io.in := raw2rec(FType.H, raw16(3))

  d2i(0).io.outW := (out_eew === 3.U) ## (out_eew === 2.U) ## (out_eew === 1.U)
  s2i(0).io.outW := (out_eew === 2.U) ## (out_eew === 1.U)
  h2i(0).io.outW := 1.U(1.W)
  h2i(1).io.outW := 1.U(1.W)

  (d2i ++ s2i ++ h2i).foreach { f2i =>
    f2i.io.signedOut := io.signed
    f2i.io.roundingMode := Mux(io.truncating, 1.U, io.frm)
  }

  val i2d = Seq(Module(new hardfloat.INToRecFN(64, FType.D.exp, FType.D.sig)))
  val i2s = Seq(
    Module(new hardfloat.INToRecFN(64, FType.S.exp, FType.S.sig)),
    Module(new hardfloat.INToRecFN(32, FType.S.exp, FType.S.sig))
  )
  val i2h = Seq(
    Module(new hardfloat.INToRecFN(32, FType.H.exp, FType.H.sig)),
    Module(new hardfloat.INToRecFN(16, FType.H.exp, FType.H.sig)),
    Module(new hardfloat.INToRecFN(32, FType.H.exp, FType.H.sig)),
    Module(new hardfloat.INToRecFN(16, FType.H.exp, FType.H.sig))
  )

  def sext(w: Int, in: UInt) = Fill(w - in.getWidth, io.signed && in(in.getWidth-1)) ## in

  i2d(0).io.in := Mux(io.widen, sext(64, in32(0)), in64(0))

  i2s(0).io.in := Mux(io.widen, sext(64, in16(0)), Mux(io.narrow, in64(0), sext(64, in32(0))))
  i2s(1).io.in := Mux(io.widen, sext(32, in16(2)), in32(1))

  i2h(0).io.in := Mux(io.narrow, in32(0), sext(32, in16(0)))
  i2h(1).io.in := in16(1)
  i2h(2).io.in := Mux(io.narrow, in32(1), sext(32, in16(2)))
  i2h(3).io.in := in16(3)

  (i2h ++ i2s ++ i2d).foreach { i2f =>
    i2f.io.signedIn := io.signed
    i2f.io.roundingMode := io.frm
    i2f.io.detectTininess := hardfloat.consts.tininess_afterRounding
  }

  val h2s = Seq.fill(2)(Module(new hardfloat.RecFNToRecFN(FType.H.exp, FType.H.sig, FType.S.exp, FType.S.sig)))
  val s2d = Seq.fill(1)(Module(new hardfloat.RecFNToRecFN(FType.S.exp, FType.S.sig, FType.D.exp, FType.D.sig)))
  val s2h = Seq.fill(2)(Module(new hardfloat.RecFNToRecFN(FType.S.exp, FType.S.sig, FType.H.exp, FType.H.sig)))
  val d2s = Seq.fill(1)(Module(new hardfloat.RecFNToRecFN(FType.D.exp, FType.D.sig, FType.S.exp, FType.S.sig)))

  h2s(0).io.in := raw2rec(FType.H, raw16(0))
  h2s(1).io.in := raw2rec(FType.H, raw16(2))
  s2d(0).io.in := raw2rec(FType.S, raw32(0))
  s2h(0).io.in := raw2rec(FType.S, raw32(0))
  s2h(1).io.in := raw2rec(FType.S, raw32(1))
  d2s(0).io.in := raw2rec(FType.D, raw64(0))

  (h2s ++ s2d ++ s2h ++ d2s).foreach { f2f =>
    f2f.io.roundingMode := io.frm
    f2f.io.detectTininess := hardfloat.consts.tininess_afterRounding
  }
  (s2h ++ d2s).foreach { f2f =>
    f2f.io.roundingMode := Mux(io.rto, "b110".U, io.frm)
  }

  val out = WireInit(0.U(64.W))
  val exc = WireInit(0.U.asTypeOf(Vec(8, UInt(FPConstants.FLAGS_SZ.W))))

  io.out := out
  io.exc := exc

  def toExc(iFlags: UInt) = Cat(iFlags(2,1).orR, 0.U(3.W), iFlags(0))

  val i2d_out = i2d.map(f => RegEnable(FType.D.ieee(f.io.out), io.valid))
  val i2s_out = i2s.map(f => RegEnable(FType.S.ieee(f.io.out), io.valid))
  val i2h_out = i2h.map(f => RegEnable(FType.H.ieee(f.io.out), io.valid))
  val i2d_exc = i2d.map(f => RegEnable(f.io.exceptionFlags, io.valid))
  val i2s_exc = i2s.map(f => RegEnable(f.io.exceptionFlags, io.valid))
  val i2h_exc = i2h.map(f => RegEnable(f.io.exceptionFlags, io.valid))

  val d2i_out = d2i.map(f => RegEnable(f.io.out, io.valid))
  val s2i_out = s2i.map(f => RegEnable(f.io.out, io.valid))
  val h2i_out = h2i.map(f => RegEnable(f.io.out, io.valid))
  val d2i_exc = d2i.map(f => RegEnable(toExc(f.io.intExceptionFlags), io.valid))
  val s2i_exc = s2i.map(f => RegEnable(toExc(f.io.intExceptionFlags), io.valid))
  val h2i_exc = h2i.map(f => RegEnable(toExc(f.io.intExceptionFlags), io.valid))

  val s2d_out = s2d.map(f => RegEnable(FType.D.ieee(f.io.out), io.valid))
  val h2s_out = h2s.map(f => RegEnable(FType.S.ieee(f.io.out), io.valid))
  val d2s_out = d2s.map(f => RegEnable(FType.S.ieee(f.io.out), io.valid))
  val s2h_out = s2h.map(f => RegEnable(FType.H.ieee(f.io.out), io.valid))
  val s2d_exc = s2d.map(f => RegEnable(f.io.exceptionFlags, io.valid))
  val h2s_exc = h2s.map(f => RegEnable(f.io.exceptionFlags, io.valid))
  val d2s_exc = d2s.map(f => RegEnable(f.io.exceptionFlags, io.valid))
  val s2h_exc = s2h.map(f => RegEnable(f.io.exceptionFlags, io.valid))

  val s1_i2f = RegEnable(io.i2f, io.valid)
  val s1_f2i = RegEnable(io.f2i, io.valid)
  val s1_widen = RegEnable(io.widen, io.valid)
  val s1_narrow = RegEnable(io.narrow, io.valid)
  val s1_out_eew = RegEnable(out_eew, io.valid)

  when (s1_i2f) {
    when (s1_out_eew === 3.U) {
      out := VecInit(i2d_out).asUInt
      exc := VecInit.fill(8)(i2d_exc(0))
    }
    when (s1_out_eew === 2.U) {
      out := VecInit(i2s_out).asUInt
      for (i <- 0 until 8) { exc(i) := i2s_exc(i/4) }
    }
    when (s1_out_eew === 1.U) {
      out := VecInit(i2h_out).asUInt
      for (i <- 0 until 8) { exc(i) := i2h_exc(i/2) }
    }
  } .elsewhen (s1_f2i) {
    when (s1_out_eew === 3.U) {
      out := d2i_out(0)
      exc := VecInit.fill(8)(d2i_exc(0))
    }
    when (s1_out_eew === 2.U) {
      out := s2i_out(0)(31,0) ## d2i_out(0)(31,0)
      exc := VecInit(Seq.fill(4)(d2i_exc(0)) ++ Seq.fill(4)(s2i_exc(0)))
    }
    when (s1_out_eew === 1.U) {
      out := h2i_out(1)(15,0) ## s2i_out(0)(15,0) ## h2i_out(0)(15,0) ## d2i_out(0)(15,0)
      exc := VecInit(Seq.fill(2)(d2i_exc(0)) ++ Seq.fill(2)(h2i_exc(0)) ++ Seq.fill(2)(s2i_exc(0)) ++ Seq.fill(2)(h2i_exc(1)))
    }
  } .otherwise {
    when (s1_out_eew === 3.U) {
      out := s2d_out(0)
      exc := VecInit.fill(8)(s2d_exc(0))
    }
    when (s1_widen && s1_out_eew === 2.U) {
      out := VecInit(h2s_out).asUInt
      for (i <- 0 until 8) { exc(i) := h2s_exc(i/4) }
    }
    when (s1_narrow && s1_out_eew === 2.U) {
      out := d2s_out(0)
      exc := VecInit.fill(8)(d2s_exc(0))
    }
    when (s1_out_eew === 1.U) {
      out := VecInit(s2h_out.map(o => 0.U(16.W) ## o)).asUInt
      for (i <- 0 until 8) { exc(i) := s2h_exc(i/4) }
    }
  }
}

class FPConvPipe(implicit p: Parameters) extends PipelinedFunctionalUnit(3)(p) with HasFPUParameters {
  val supported_insns = FPConvFactory.insns

  io.set_vxsat := false.B
  io.stall := false.B

  val rs1 = io.pipe(0).bits.rs1
  val ctrl_widen = rs1(3)
  val ctrl_narrow = rs1(4)
  val ctrl_signed = rs1(0)
  val ctrl_i2f = !rs1(2) && rs1(1)
  val ctrl_f2i = (!rs1(2) && !rs1(1)) || (rs1(2) && rs1(1))
  val ctrl_truncating = rs1(2) && rs1(1)
  val ctrl_round_to_odd = rs1(0)

  val rvs2_data = io.pipe(0).bits.rvs2_data
  val vd_eew = io.pipe(0).bits.vd_eew
  val rvs2_eew = io.pipe(0).bits.rvs2_eew

  val hi = (io.pipe(0).bits.eidx >> (dLenOffBits.U - vd_eew))(0)
  val expanded_rvs2_data = narrow2_expand(rvs2_data.asTypeOf(Vec(dLenB, UInt(8.W))), rvs2_eew,
    hi, false.B).asUInt

  val conv_blocks = Seq.fill(dLen/64) { Module(new FPConvBlock) }
  conv_blocks.zipWithIndex.foreach { case (c,i) =>
    c.io.valid := io.pipe(0).valid
    c.io.in := Mux(ctrl_widen,
      expanded_rvs2_data,
      rvs2_data)((i*64)+63,i*64)

    c.io.in_eew := rvs2_eew
    c.io.widen := ctrl_widen
    c.io.narrow := ctrl_narrow
    c.io.signed := ctrl_signed
    c.io.frm := io.pipe(0).bits.frm
    c.io.i2f := ctrl_i2f
    c.io.f2i := ctrl_f2i
    c.io.truncating := ctrl_truncating
    c.io.rto := ctrl_round_to_odd
  }

  val out = Wire(UInt(dLen.W))
  val exc = Wire(Vec(dLenB, UInt(FPConstants.FLAGS_SZ.W)))

  val s1_ctrl_narrow = io.pipe(1).bits.rs1(4)
  val s1_vd_eew = io.pipe(1).bits.vd_eew
  when (s1_ctrl_narrow) {
    val bits = VecInit(conv_blocks.map(_.io.out)).asUInt
    val out8  = VecInit(bits.asTypeOf(Vec(dLenB     , UInt( 8.W))).grouped(2).map(_.head).toSeq).asUInt
    val out16 = VecInit(bits.asTypeOf(Vec(dLenB >> 1, UInt(16.W))).grouped(2).map(_.head).toSeq).asUInt
    val out32 = VecInit(bits.asTypeOf(Vec(dLenB >> 2, UInt(32.W))).grouped(2).map(_.head).toSeq).asUInt
    out := Fill(2, Mux1H(Seq(
      (s1_vd_eew === 0.U) -> out8,
      (s1_vd_eew === 1.U) -> out16,
      (s1_vd_eew === 2.U) -> out32
    )))
    val excs = VecInit(conv_blocks.map(_.io.exc).flatten)
    val exc_out = Mux1H(Seq(
      (s1_vd_eew === 0.U) -> VecInit(excs.grouped(2).map(_.take(1)).flatten.toSeq),
      (s1_vd_eew === 1.U) -> VecInit(excs.grouped(4).map(_.take(2)).flatten.toSeq),
      (s1_vd_eew === 2.U) -> VecInit(excs.grouped(8).map(_.take(4)).flatten.toSeq),
    ))

    exc := VecInit(Seq(exc_out, exc_out).flatten)
  } .otherwise {
    out := VecInit(conv_blocks.map(_.io.out)).asUInt
    exc := VecInit(conv_blocks.map(_.io.exc).flatten)
  }


  val pipe_out = Pipe(io.pipe(1).valid, out, depth-2).bits
  val pipe_exc = Pipe(io.pipe(1).valid, exc, depth-2).bits

  io.write.valid := io.pipe(depth-1).valid
  io.write.bits.eg := io.pipe(depth-1).bits.wvd_eg
  io.write.bits.mask := FillInterleaved(8, io.pipe(depth-1).bits.wmask)
  io.write.bits.data := pipe_out

  io.set_fflags.valid := io.write.valid
  io.set_fflags.bits := (0 until dLenB).map { i => Mux(io.pipe(depth-1).bits.wmask(i), pipe_exc(i), 0.U) }.reduce(_|_)
  io.scalar_write.valid := false.B
  io.scalar_write.bits := DontCare
}
