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
  def insns = Seq(FCVT_SGL, FCVT_NRW, FCVT_WID).map(_.pipelined(2))
  def generate(implicit p: Parameters) = new FPConvPipe()(p)
}

class FPConvBlock(implicit p: Parameters) extends CoreModule()(p) with HasFPUParameters {
  val io = IO(new Bundle {
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
  val h2i = Seq.fill(2)(Module(new RecFNToINDynamic(FType.H.exp, FType.H.sig, Seq(32))))

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
  val i2h = (
    Seq.fill(2) { Module(new hardfloat.INToRecFN(32, FType.H.exp, FType.H.sig)) } ++
    Seq.fill(2) { Module(new hardfloat.INToRecFN(16, FType.H.exp, FType.H.sig)) }
  )

  def sext(w: Int, in: UInt) = Fill(w - in.getWidth, io.signed && in(in.getWidth-1)) ## in

  i2d(0).io.in := Mux(io.widen, sext(64, in32(0)), in64(0))
  i2s(0).io.in := Mux(io.widen, sext(64, in16(0)), Mux(io.narrow, in64(0), sext(64, in32(0))))
  i2s(1).io.in := Mux(io.widen, sext(32, in16(1)), in32(1))
  i2h(0).io.in := Mux(io.narrow, in32(0), sext(32, in16(0)))
  i2h(1).io.in := Mux(io.narrow, in32(1), sext(32, in16(1)))
  i2h(2).io.in := in16(2)
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
  h2s(1).io.in := raw2rec(FType.H, raw16(1))
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

  io.out := DontCare
  io.exc := DontCare

  def exc(iFlags: UInt) = Cat(iFlags(2,1).orR, 0.U(3.W), iFlags(0))

  when (io.i2f) {
    val i2d_out = i2d.map(f => FType.D.ieee(f.io.out))
    val i2s_out = i2s.map(f => FType.S.ieee(f.io.out))
    val i2h_out = i2h.map(f => FType.H.ieee(f.io.out))
    when (out_eew === 3.U) {
      io.out := VecInit(i2d_out).asUInt
      io.exc := VecInit.fill(8)(i2d(0).io.exceptionFlags)
    }
    when (out_eew === 2.U) {
      io.out := VecInit(i2s_out).asUInt
      for (i <- 0 until 8) { io.exc(i) := i2s(i/4).io.exceptionFlags }
    }
    when (out_eew === 1.U) {
      io.out := VecInit(i2h_out).asUInt
      for (i <- 0 until 8) { io.exc(i) := i2h(i/2).io.exceptionFlags }
    }
  } .elsewhen (io.f2i) {
    when (out_eew === 3.U) {
      io.out := d2i(0).io.out
      io.exc := VecInit.fill(8)(exc(d2i(0).io.intExceptionFlags))
    }
    when (out_eew === 2.U) {
      io.out := s2i(0).io.out ## d2i(0).io.out(31,0)
      io.exc(0) := exc(d2i(0).io.intExceptionFlags)
      io.exc(1) := exc(d2i(0).io.intExceptionFlags)
      io.exc(2) := exc(d2i(0).io.intExceptionFlags)
      io.exc(3) := exc(d2i(0).io.intExceptionFlags)
      io.exc(4) := exc(s2i(0).io.intExceptionFlags)
      io.exc(5) := exc(s2i(0).io.intExceptionFlags)
      io.exc(6) := exc(s2i(0).io.intExceptionFlags)
      io.exc(7) := exc(s2i(0).io.intExceptionFlags)
    }
    when (out_eew === 1.U) {
      io.out := h2i(1).io.out ## s2i(0).io.out(15,0) ## h2i(0).io.out ## d2i(0).io.out(15,0)
      io.exc(0) := exc(d2i(0).io.intExceptionFlags)
      io.exc(1) := exc(d2i(0).io.intExceptionFlags)
      io.exc(2) := exc(h2i(0).io.intExceptionFlags)
      io.exc(3) := exc(h2i(0).io.intExceptionFlags)
      io.exc(4) := exc(s2i(0).io.intExceptionFlags)
      io.exc(5) := exc(s2i(0).io.intExceptionFlags)
      io.exc(6) := exc(h2i(1).io.intExceptionFlags)
      io.exc(7) := exc(h2i(1).io.intExceptionFlags)
    }
  } .otherwise {
    when (out_eew === 3.U) {
      io.out := FType.D.ieee(s2d(0).io.out)
      io.exc := VecInit.fill(8)(s2d(0).io.exceptionFlags)
    }
    when (io.widen && out_eew === 2.U) {
      io.out := VecInit(h2s.map(h => FType.S.ieee(h.io.out))).asUInt
      for (i <- 0 until 8) { io.exc(i) := h2s(i/4).io.exceptionFlags }
    }
    when (io.narrow && out_eew === 2.U) {
      io.out := FType.S.ieee(d2s(0).io.out)
      io.exc := VecInit.fill(8)(d2s(0).io.exceptionFlags)
    }
    when (out_eew === 1.U) {

    }
  }

  dontTouch(io)
}

class FPConvPipe(implicit p: Parameters) extends PipelinedFunctionalUnit(2)(p) with HasFPUParameters {
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

  when (ctrl_narrow) {
    val bits = VecInit(conv_blocks.map(_.io.out)).asUInt
    val out8  = VecInit(bits.asTypeOf(Vec(dLenB     , UInt( 8.W))).grouped(2).map(_.head).toSeq).asUInt
    val out16 = VecInit(bits.asTypeOf(Vec(dLenB >> 1, UInt(16.W))).grouped(2).map(_.head).toSeq).asUInt
    val out32 = VecInit(bits.asTypeOf(Vec(dLenB >> 2, UInt(32.W))).grouped(2).map(_.head).toSeq).asUInt
    out := Fill(2, Mux1H(Seq(
      (vd_eew === 0.U) -> out8,
      (vd_eew === 1.U) -> out16,
      (vd_eew === 2.U) -> out32
    )))
    val excs = VecInit(conv_blocks.map(_.io.exc).flatten)
    val exc_out = Mux1H(Seq(
      (vd_eew === 0.U) -> VecInit(excs.grouped(2).map(_.take(1)).flatten.toSeq),
      (vd_eew === 1.U) -> VecInit(excs.grouped(4).map(_.take(2)).flatten.toSeq),
      (vd_eew === 2.U) -> VecInit(excs.grouped(8).map(_.take(4)).flatten.toSeq),
    ))

    exc := VecInit(Seq(exc_out, exc_out).flatten)
  } .otherwise {
    out := VecInit(conv_blocks.map(_.io.out)).asUInt
    exc := VecInit(conv_blocks.map(_.io.exc).flatten)
  }


  val pipe_out = Pipe(io.pipe(0).valid, out).bits
  val pipe_exc = Pipe(io.pipe(0).valid, exc).bits

  io.write.valid := io.pipe(depth-1).valid
  io.write.bits.eg := io.pipe(depth-1).bits.wvd_eg
  io.write.bits.mask := FillInterleaved(8, io.pipe(depth-1).bits.wmask)
  io.write.bits.data := pipe_out

  io.set_fflags.valid := io.write.valid
  io.set_fflags.bits := (0 until dLenB).map { i => Mux(io.pipe(depth-1).bits.wmask(i), pipe_exc(i), 0.U) }.reduce(_|_)
  io.scalar_write.valid := false.B
  io.scalar_write.bits := DontCare
}

  // val nh = dLen / 16
  // val ns = dLen / 32
  // val nd = dLen / 64

  // val x16_ins = rvs2_data.asTypeOf(Vec(dLen/16, UInt(16.W)))
  // val x32_ins = rvs2_data.asTypeOf(Vec(dLen/32, UInt(32.W)))
  // val x64_ins = rvs2_data.asTypeOf(Vec(dLen/64, UInt(64.W)))
  // val f16_ins = x16_ins.map(x => FType.H.recode(x))
  // val f32_ins = x32_ins.map(x => FType.S.recode(x))
  // val f64_ins = x64_ins.map(x => FType.D.recode(x))

  // val h2i = Seq.fill(nh)(Module(new hardfloat.RecFNToIN(FType.H.exp, FType.H.sig, 32)))
  // val s2i = Seq.fill(ns)(Module(new hardfloat.RecFNToIN(FType.S.exp, FType.S.sig, 64)))
  // val d2i = Seq.fill(nd)(Module(new hardfloat.RecFNToIN(FType.D.exp, FType.D.sig, 64)))
  // val i2h = Seq.fill(nh)(Module(new hardfloat.INToRecFN(32, FType.H.exp, FType.H.sig)))
  // val i2s = Seq.fill(ns)(Module(new hardfloat.INToRecFN(64, FType.S.exp, FType.S.sig)))
  // val i2d = Seq.fill(nd)(Module(new hardfloat.INToRecFN(64, FType.D.exp, FType.D.sig)))
  // val h2s = Seq.fill(ns)(Module(new hardfloat.RecToRecFN(FType.H.exp, FType.H.sig, FType.S.exp, FType.S.sig)))
  // val s2d = Seq.fill(nd)(Module(new hardfloat.RecToRecFN(FType.S.exp, FType.S.sig, FType.D.exp, FType.D.sig)))
  // val s2h = Seq.fill(ns)(Module(new hardfloat.RecToRecFN(FType.S.exp, FType.S.sig, FType.H.exp, FType.H.sig)))
  // val d2s = Seq.fill(nd)(Module(new hardfloat.RecToRecFN(FType.D.exp, FType.D.sig, FType.S.exp, FType.S.sig)))

  // (h2i ++ s2i ++ d2i).foreach { f2i =>
  //   f2i.io.signedOut := ctrl_signed
  //   f2i.io.roundingMode := Mux(ctrl_truncating, 1.U, io.pipe(0).bits.frm)
  // }
  // (i2h ++ i2s ++ i2d).foreach { i2f =>
  //   i2f.io.signedIn := ctrl_signed
  //   i2f.io.roundingMode := io.pipe(0).bits.frm
  //   i2f.io.detectTininess := hardfloat.consts.tininess_afterRounding
  // }
  // (h2s ++ s2d ++ s2h ++ d2s).foreach { f2f =>
  //   f2nf.io.roundingMode := Mux(ctrl_round_to_odd && ctrl_narrow, "b110".U, io.pipe(0).bits.frm)
  //   f2nf.io.detectTininess := hardfloat.consts.tininess_afterRounding
  // }

  // h2i.zipWithIndex.foreach { case (f2i, i) => f2i(i).io.in := f16_ins(i) }
  // s2i.zipWithIndex.foreach { case (f2i, i) => f2i(i).io.in := f16_ins(i) }


  // val outs = fTypes.zipWithIndex.map { case (fType, i) =>
  //   val n = dLen / fType.ieeeWidth

  //   val wfType = fTypes((i+1) max 0)
  //   val nfType = fTypes((i-1) max 0)

  //   val intWidth = (fType.ieeeWidth * 2) max 64

  //   val f2is  = Seq.fill(n)(Module(new hardfloat.RecFNToIN(fType.exp, fType.sig, intWidth)))
  //   val f2nfs = Seq.fill(n)(Module(new hardfloat.RecToRecFN(wfType.exp, wfType.sig, fType.exp, fType.sig)))
  //   val f2wfs = Seq.fill(n)(Module(new hardfloat.RecToRecFN(nfType.exp, nfType.sig, fType.exp, fType.sig)))
  //   val i2fs  = Seq.fill(n)(Module(new hardfloat.INToRecFN(intWidth, fType.exp, fType.sig)))

  //   f2is.foreach { f2i =>
  //     f2i.io.signedOut := ctrl_signed
  //     f2i.io.roundingMode := Mux(ctrl_truncating, 1.U, io.pipe(0).bits.frm)
  //   }
  //   i2fs.foreach { i2f =>
  //     i2f.io.signedIn := ctrl_signed
  //     i2f.io.roundingMode := io.pipe(0).bits.frm
  //     i2f.io.detectTininess := hardfloat.consts.tininess_afterRounding
  //   }
  //   f2nfs.foreach { f2nf =>
  //     f2nf.io.roundingMode := Mux(ctrl_round_to_odd, "b110".U, io.pipe(0).bits.frm)
  //     f2nf.io.detectTininess := hardfloat.consts.tininess_afterRounding
  //   }
  //   f2wfs.foreach { f2wf =>
  //     f2wf.io.roundingMode := io.pipe(0).bits.frm
  //     f2wf.io.detectTininess := hardfloat.consts.tininess_afterRounding
  //   }

  //   val rvs2_elems        = rvs2_data.asTypeOf(Vec(n, UInt(fType.ieeeWidth.W)))
  //   val rvs2_wide_elems   = if (i == 2) { rvs2_elems } else {
  //     rvs2_data.asTypeOf(Vec(n >> 1, UInt(wfType.ieeeWidth.W)))
  //   }
  //   val rvs2_narrow_elems = if (i == 0) { rvs2_elems } else {
  //     rvs2_data.asTypeOf(Vec(n << 1, UInt(nfType.ieeeWidth.W)))
  //   }

  //   f2is.zipWithIndex.foreach { case (f2i, i) =>
  //     f2i.io.in := fType.recode(rvs2_elems(i))
  //   }
  //   i2fs.zipWithIndex.foreach { case (i2f, i) =>
  //     i2f.io.in := rvs2_elems(i)
  //   }

  //   val f2i_narrow_outs = f2is.map(_.io.out(nfType.ieeeWidth-1,0))
  //   val f2i_wide_outs   = f2is.map(_.io.out(wfType.ieeeWidth-1,0))
  //   val f2i_single_outs = f2is.map(_.io.out(fType.ieeeWidth-1,0))
  //   val i2f_outs        = i2fs.map(_.io.out)
  //   val f2nf_outs       = f2nfs.map(_.io.out)
  //   val f2wf_outs       = f2wfs.map(_.io.out)
  //   val f_outs          = (0 until n).map { i => fType.ieee(Mux1H(Seq(
  //     ctrl_i2f -> i2f_outs(i),
  //     ctrl_narrow -> f2nf_outs(i),
  //     ctrl_widen -> f2wf_outs(i)
  //   ))) }


  //   Mux(ctrl_i2f, i2f_outs.asUInt, f2i_single_outs.asUInt)
  // }

  // val out = Mux1H(eew_select, outs)
  // val pipe_out = Pipe(io.pipe(0).valid, out).bits

  // // Single Width Conversions
  // val single_width_conversions = fTypes.map { fType =>
  //   val n = dLen / fType.ieeeWidth
  //   val rvs2_chunks = rvs2_data.asTypeOf(Vec(n, UInt(fType.ieeeWidth.W)))

  //   // FP to Int
  //   val fptoint_modules = Seq.fill(n)(Module(new hardfloat.RecFNToIN(fType.exp, fType.sig, fType.ieeeWidth)))
  //   val gen_fptoint = rvs2_chunks.zip(fptoint_modules).map { case(rvs2, conv) =>
  //     conv.io.signedOut := ctrl_signed
  //     conv.io.roundingMode := Mux(ctrl_truncating, 1.U, io.pipe(0).bits.frm)
  //     conv.io.in := fType.recode(rvs2)
  //     conv.io.out
  //   }

  //   // Int to FP
  //   val inttofp_modules = Seq.fill(n)(Module(new hardfloat.INToRecFN(fType.ieeeWidth, fType.exp, fType.sig)))
  //   val gen_inttofp = rvs2_chunks.zip(inttofp_modules).map { case(rvs2, conv) =>
  //     conv.io.signedIn := ctrl_signed
  //     conv.io.roundingMode := io.pipe(0).bits.frm
  //     conv.io.detectTininess := hardfloat.consts.tininess_afterRounding
  //     conv.io.in := rvs2
  //     fType.ieee(conv.io.out)
  //   }

  //   Mux(ctrl_out, gen_inttofp.asUInt, gen_fptoint.asUInt)
  // }

  // val single_width_out = Mux1H(eew_select, single_width_conversions)

  // // Widening Conversions
  // val widening_conversions = fTypes.zipWithIndex.filter(_._1.ieeeWidth <= 32).map { case (fType, i) =>
  //   val num_converts = dLen / (2 * fType.ieeeWidth)
  //   val in_sew = log2Ceil(fType.ieeeWidth / 8)
  //   val wideType = fTypes(i+1)

  //   // Int to FP conversions
  //   val wide_inttofp_modules = Seq.fill(num_converts)(Module(new hardfloat.INToRecFN(fType.ieeeWidth, wideType.exp, wideType.sig)))
  //   val gen_inttofp = wide_inttofp_modules.zipWithIndex.map { case(wide, idx) =>
  //     wide.io.signedIn := ctrl_signed
  //     wide.io.roundingMode := io.pipe(0).bits.frm
  //     wide.io.detectTininess := hardfloat.consts.tininess_afterRounding
  //     wide.io.in := extractElem(rvs2_data, in_sew.U, io.pipe(0).bits.eidx + idx.U)(fType.ieeeWidth-1,0)
  //     wideType.ieee(wide.io.out)
  //   }.asUInt

  //   // FP to FP conversions
  //   val wide_fptofp_modules = Seq.fill(num_converts)(Module(new hardfloat.RecFNToRecFN(fType.exp, fType.sig, wideType.exp, wideType.sig)))
  //   val gen_fptofp = wide_fptofp_modules.zipWithIndex.map{ case(wide, idx) =>
  //     wide.io.in := fType.recode(extractElem(rvs2_data, in_sew.U, io.pipe(0).bits.eidx + idx.U)(fType.ieeeWidth-1,0))
  //     wide.io.roundingMode := io.pipe(0).bits.frm
  //     wide.io.detectTininess := hardfloat.consts.tininess_afterRounding
  //     wideType.ieee(wide.io.out)
  //   }.asUInt

  //   // FP to Int conversions
  //   val wide_fptoint_modules = Seq.fill(num_converts)(Module(new hardfloat.RecFNToIN(fType.exp, fType.sig, wideType.ieeeWidth)))
  //   val gen_fptoint = wide_fptoint_modules.zipWithIndex.map{ case(wide, idx) =>
  //     val extracted_rvs2_bits = extractElem(rvs2_data, in_sew.U, io.pipe(0).bits.eidx + idx.U)(fType.ieeeWidth-1,0)
  //     wide.io.signedOut := ctrl_signed
  //     wide.io.roundingMode := Mux(ctrl_truncating, 1.U, io.pipe(0).bits.frm)
  //     wide.io.in := fType.recode(extracted_rvs2_bits)
  //     wide.io.out
  //   }.asUInt

  //   Mux1H(Seq(ctrl_i2f         , ctrl_f2f         , ctrl_f2i))
  //         Seq(gen_inttofp      , gen_fptofp       , gen_fptoint))
  // }

  // val widening_out = Mux1H(Seq(vd_eew === 2.U, vd_eew === 3.U), widening_conversions)

  // // Narrowing Conversions
  // // Just EEW of 32 and 64
  // val narrowing_conversions = fTypes.zipWithIndex.filter(_._1.ieeeWidth >= 32).map { case (fType, i) =>
  //   val num_converts = dLen / fType.ieeeWidth
  //   val in_sew = log2Ceil(fType.ieeeWidth / 8)
  //   val narrowType = fTypes(i-1)

  //   // Int to FP Conversions
  //   val narrow_inttofp_modules = Seq.fill(num_converts)(Module(new hardfloat.INToRecFN(fType.ieeeWidth, narrowType.exp, narrowType.sig)))
  //   val gen_inttofp = narrow_inttofp_modules.zipWithIndex.map { case(narrow, idx) =>
  //     narrow.io.signedIn := ctrl_signed
  //     narrow.io.roundingMode := io.pipe(0).bits.frm
  //     narrow.io.detectTininess := hardfloat.consts.tininess_afterRounding
  //     narrow.io.in := extractElem(rvs2_data, in_sew.U, io.pipe(0).bits.eidx + idx.U)(fType.ieeeWidth-1,0)
  //     narrowType.ieee(narrow.io.out)
  //   }.asUInt

  //   // FP to FP Conversions
  //   val fptofp_modules = Seq.fill(num_converts)(Module(new hardfloat.RecFNToRecFN(fType.exp, fType.sig, narrowType.exp, narrowType.sig)))
  //   val gen_fptofp = fptofp_modules.zipWithIndex.map{ case(narrow, idx) =>
  //     narrow.io.in := fType.recode(extractElem(rvs2_data, in_sew.U, io.pipe(0).bits.eidx + idx.U)(fType.ieeeWidth-1,0))
  //     narrow.io.roundingMode := Mux(ctrl_round_to_odd, "b110".U, io.pipe(0).bits.frm)
  //     narrow.io.detectTininess := hardfloat.consts.tininess_afterRounding
  //     narrowType.ieee(narrow.io.out)
  //   }.asUInt

  //   // FP to Int Conversions
  //   val fptoint_modules = Seq.fill(num_converts)(Module(new hardfloat.RecFNToIN(fType.exp, fType.sig, narrowType.ieeeWidth)))
  //   val gen_fptoint = fptoint_modules.zipWithIndex.map { case(conv, idx) =>
  //     val extracted_rvs2_bits = extractElem(rvs2_data, in_sew.U, io.pipe(0).bits.eidx + idx.U)(fType.ieeeWidth-1,0)
  //     conv.io.signedOut := ctrl_signed
  //     conv.io.roundingMode := Mux(ctrl_truncating, 1.U, io.pipe(0).bits.frm)
  //     conv.io.in := fType.recode(extracted_rvs2_bits)
  //     conv.io.out
  //   }.asUInt

  //   Mux1H(Seq(!rs1(2) && rs1(1), rs1(2) && !rs1(1), (!rs1(2) && !rs1(1)) || (rs1(2) && rs1(1))),
  //         Seq(gen_inttofp      , gen_fptofp       , gen_fptoint))
  // }

  // // Special Case for FP16 narrowing converts
  // // Only narrowing FP to Int
  // val fp16_fptoint_modules = Seq.fill(dLen/16)(Module(new hardfloat.RecFNToIN(FType.H.exp, FType.H.sig, FType.H.ieeeWidth/2)))
  // val fp16_gen_fptoint = fp16_fptoint_modules.zipWithIndex.map { case(conv, idx) =>
  //   val extracted_rvs2_bits = extractElem(rvs2_data, 1.U, io.pipe(0).bits.eidx + idx.U)(FType.H.ieeeWidth-1,0)
  //   conv.io.signedOut := ctrl_signed
  //   conv.io.roundingMode := io.pipe(0).bits.frm
  //   conv.io.in := FType.H.recode(Mux(ctrl_truncating, extracted_rvs2_bits(FType.H.ieeeWidth-1, FType.H.sig-2) << (FType.H.sig - 2), extracted_rvs2_bits))
  //   conv.io.out
  // }.asUInt


  // val narrowing_out = Fill(2, Mux1H(Seq(vd_eew === 0.U, vd_eew === 1.U, vd_eew === 2.U), Seq(fp16_gen_fptoint) ++ narrowing_conversions))

  // val pipe_out = Pipe(io.pipe(0).valid, Mux1H(Seq(!ctrl_widen && !ctrl_narrow, ctrl_widen, ctrl_narrow),
  //                                             Seq(single_width_out, widening_out, narrowing_out))).bits

  // io.write.valid := io.pipe(depth-1).valid
  // io.write.bits.eg := io.pipe(depth-1).bits.wvd_eg
  // io.write.bits.mask := FillInterleaved(8, io.pipe(depth-1).bits.wmask)
  // io.write.bits.data := pipe_out

  // io.set_fflags := DontCare
  // io.scalar_write.valid := false.B
  // io.scalar_write.bits := DontCare
// }
