package vector.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import vector.common._

class FPConvPipe(implicit p: Parameters) extends PipelinedFunctionalUnit(1)(p) with HasFPUParameters {
  io.iss.sub_dlen := 0.U
  io.set_vxsat := false.B

  lazy val opcodes = Seq(
    OPFFunct6.funary0
  )
  override def accepts(f3: UInt, f6: UInt): Bool = VecDecode(f3, f6, opcodes)

  val rs1 = io.pipe(0).bits.rs1
  val ctrl_widen = rs1(3)
  val ctrl_narrow = rs1(4)
  val ctrl_signed = rs1(0)
  val ctrl_out = !rs1(2) && rs1(1)
  val ctrl_truncating = rs1(2) && rs1(1)
  val ctrl_round_to_odd = rs1(0)

  val rvs2_data = io.pipe(0).bits.rvs2_data

  val fTypes = Seq(FType.S, FType.D)

  val single_width_out = Wire(Vec(2, UInt(dLen.W)))
  val widening_out = Wire(UInt(dLen.W))
  val narrowing_out = Wire(UInt((dLen/2).W))

  // Single Width Type Conversions
  for(eew <- 2 until 4) {
    val fType = fTypes(eew - 2)
    val num_chunks = dLen / fType.ieeeWidth
    val rvs2_chunks = rvs2_data.asTypeOf(Vec(num_chunks, UInt(fType.ieeeWidth.W)))

    // FP to Int
    val fptoint_modules = Seq.fill(num_chunks)(Module(new hardfloat.RecFNToIN(fType.exp, fType.sig, fType.ieeeWidth)))
    val gen_fptoint = rvs2_chunks.zip(fptoint_modules).map { case(rvs2, conv) =>
      conv.io.signedOut := ctrl_signed
      conv.io.roundingMode := Mux(ctrl_truncating, 1.U, io.pipe(0).bits.frm)
      conv.io.in := fType.recode(Mux(ctrl_truncating, rvs2(fType.ieeeWidth-1, fType.sig-2) << (fType.sig - 2), rvs2))
      conv.io
    } 
    val fptoint_results = gen_fptoint.map { case(conv) =>
      conv.out
    }

    // Int to FP
    val inttofp_modules = Seq.fill(num_chunks)(Module(new hardfloat.INToRecFN(fType.ieeeWidth, fType.exp, fType.sig)))
    val gen_inttofp = rvs2_chunks.zip(inttofp_modules).map { case(rvs2, conv) =>
      conv.io.signedIn := ctrl_signed
      conv.io.roundingMode := io.pipe(0).bits.frm
      conv.io.detectTininess := hardfloat.consts.tininess_afterRounding
      conv.io.in := rvs2
      conv.io
    }
    val inttofp_results = gen_inttofp.map { case(conv) =>
      fType.ieee(conv.out)
    }

    single_width_out(eew-2) := Mux(ctrl_out, inttofp_results.asUInt, fptoint_results.asUInt) 
  }

  // Widening Type Conversions
  val num_wide_units = dLen / 64

  //// Int to FP
  //// 01X
  val inttofp_modules = Seq.fill(num_wide_units)(Module(new hardfloat.INToRecFN(FType.S.ieeeWidth, 11, 53)))
  val gen_inttofp = inttofp_modules.zipWithIndex.map { case(conv, idx) =>
    conv.io.signedIn := ctrl_signed
    conv.io.roundingMode := io.pipe(0).bits.frm
    conv.io.detectTininess := hardfloat.consts.tininess_afterRounding
    conv.io.in := extract(rvs2_data, false.B, 2.U, io.pipe(0).bits.eidx + idx.U)(31,0)
    conv.io
  }
  val inttofp_results = gen_inttofp.map { conv =>
    FType.D.ieee(conv.out)
  }

  // FP to Int
  // 00X
  val fptoint_modules = Seq.fill(num_wide_units)(Module(new hardfloat.RecFNToIN(FType.S.exp, FType.S.sig, FType.D.ieeeWidth)))
  val gen_fptoint = fptoint_modules.zipWithIndex.map { case(conv, idx) =>
    val extracted_rvs2_bits = extract(rvs2_data, false.B, 2.U, io.pipe(0).bits.eidx + idx.U)(31,0)
    conv.io.signedOut := ctrl_signed
    conv.io.roundingMode := io.pipe(0).bits.frm
    conv.io.in := FType.S.recode(Mux(ctrl_truncating, extracted_rvs2_bits(FType.S.ieeeWidth-1, FType.S.sig-2) << (FType.S.sig - 2), extracted_rvs2_bits))
    conv.io
  }
  val fptoint_results = gen_fptoint.map { conv => conv.out }

  // FP to FP
  // 100
  val fptofp_modules = Seq.fill(num_wide_units)(Module(new hardfloat.RecFNToRecFN(FType.S.exp, FType.S.sig, FType.D.exp, FType.D.sig)))
  val gen_fptofp = fptofp_modules.zipWithIndex.map{ case(conv, idx) => 
    conv.io.in := FType.S.recode(extract(rvs2_data, false.B, 2.U, io.pipe(0).bits.eidx + idx.U)(31,0))
    conv.io.roundingMode := io.pipe(0).bits.frm
    conv.io.detectTininess := hardfloat.consts.tininess_afterRounding
    conv.io
  }
  val fptofp_results = gen_fptofp.map { conv => FType.D.ieee(conv.out) }

  when (!rs1(2) && rs1(1)) {
    widening_out := inttofp_results.asUInt
  } .elsewhen (!rs1(1) && rs1(2)) {
    widening_out := fptofp_results.asUInt 
  } .otherwise {
    widening_out := fptoint_results.asUInt
  }

  // Narrowing Type Conversions
  val num_narrow_units = dLen / 64

  val narrow_inttofp_modules = Seq.fill(num_narrow_units)(Module(new hardfloat.INToRecFN(FType.D.ieeeWidth, 8, 24)))
  val narrow_gen_inttofp = narrow_inttofp_modules.zipWithIndex.map { case(conv, idx) =>
    conv.io.signedIn := ctrl_signed
    conv.io.roundingMode := io.pipe(0).bits.frm
    conv.io.detectTininess := hardfloat.consts.tininess_afterRounding
    conv.io.in := extract(rvs2_data, false.B, 3.U, io.pipe(0).bits.eidx + idx.U)(63,0)
    conv.io
  }
  val narrow_inttofp_results = narrow_gen_inttofp.map { conv =>
    FType.S.ieee(conv.out)
  }

  // FP to Int
  // 00X
  val narrow_fptoint_modules = Seq.fill(num_narrow_units)(Module(new hardfloat.RecFNToIN(FType.D.exp, FType.D.sig, FType.S.ieeeWidth)))
  val narrow_gen_fptoint = narrow_fptoint_modules.zipWithIndex.map { case(conv, idx) =>
    val extracted_rvs2_bits = extract(rvs2_data, false.B, 3.U, io.pipe(0).bits.eidx + idx.U)(63,0)
    conv.io.signedOut := ctrl_signed
    conv.io.roundingMode := io.pipe(0).bits.frm
    conv.io.in := FType.D.recode(Mux(ctrl_truncating, extracted_rvs2_bits(FType.D.ieeeWidth-1, FType.D.sig-2) << (FType.D.sig - 2), extracted_rvs2_bits))
    conv.io
  }
  val narrow_fptoint_results = narrow_gen_fptoint.map { conv => conv.out }

  // FP to FP
  // 100
  val narrow_fptofp_modules = Seq.fill(num_narrow_units)(Module(new hardfloat.RecFNToRecFN(FType.D.exp, FType.D.sig, FType.S.exp, FType.S.sig)))
  val narrow_gen_fptofp = narrow_fptofp_modules.zipWithIndex.map{ case(conv, idx) => 
    conv.io.in := FType.D.recode(extract(rvs2_data, false.B, 3.U, io.pipe(0).bits.eidx + idx.U)(63,0))
    conv.io.roundingMode := Mux(ctrl_round_to_odd, "b110".U, io.pipe(0).bits.frm)
    conv.io.detectTininess := hardfloat.consts.tininess_afterRounding
    conv.io
  }
  val narrow_fptofp_results = narrow_gen_fptofp.map { conv => FType.S.ieee(conv.out) }

  when (!rs1(2) && rs1(1)) {
    narrowing_out := narrow_inttofp_results.asUInt
  } .elsewhen (!rs1(1) && rs1(2)) {
    narrowing_out := narrow_fptofp_results.asUInt 
  } .otherwise {
    narrowing_out := narrow_fptoint_results.asUInt
  }

  val single_out_final = Wire(UInt(dLen.W))
  single_out_final := Mux(io.pipe(0).bits.rvs2_eew === 3.U, single_width_out(1), single_width_out(0))

  io.write.valid := io.pipe(0).valid
  io.write.bits.eg := io.pipe(0).bits.wvd_eg
  io.write.bits.mask := FillInterleaved(8, io.pipe(0).bits.wmask)

  when (ctrl_widen) {
    io.write.bits.data := widening_out
  } .elsewhen (ctrl_narrow) {
    io.write.bits.data := Fill(2, narrowing_out)
  } .otherwise {
    io.write.bits.data := single_out_final
  }

  io.set_fflags := DontCare
}
