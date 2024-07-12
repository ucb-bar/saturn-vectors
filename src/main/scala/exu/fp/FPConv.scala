package saturn.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import saturn.common._
import saturn.insns._

class FPConvPipe(implicit p: Parameters) extends PipelinedFunctionalUnit(2)(p) with HasFPUParameters {
  val supported_insns = Seq(FCVT_SGL, FCVT_NRW, FCVT_WID)

  io.set_vxsat := false.B

  io.iss.ready := new VectorDecoder(io.iss.op.funct3, io.iss.op.funct6, 0.U, 0.U,
    supported_insns, Nil).matched

  val rs1 = io.pipe(0).bits.rs1
  val ctrl_widen = rs1(3)
  val ctrl_narrow = rs1(4)
  val ctrl_signed = rs1(0)
  val ctrl_out = !rs1(2) && rs1(1)
  val ctrl_truncating = rs1(2) && rs1(1)
  val ctrl_round_to_odd = rs1(0)

  val rvs2_data = io.pipe(0).bits.rvs2_data
  val vd_eew = io.pipe(0).bits.vd_eew
  val rvs2_eew = io.pipe(0).bits.rvs2_eew
  val eew_select = Seq(rvs2_eew === 1.U, rvs2_eew === 2.U, rvs2_eew === 3.U)

  val fTypes = Seq(FType.H, FType.S, FType.D)

  // Single Width Conversions
  val single_width_conversions = fTypes.map { fType =>
    val num_chunks = dLen / fType.ieeeWidth
    val rvs2_chunks = rvs2_data.asTypeOf(Vec(num_chunks, UInt(fType.ieeeWidth.W)))

    // FP to Int
    val fptoint_modules = Seq.fill(num_chunks)(Module(new hardfloat.RecFNToIN(fType.exp, fType.sig, fType.ieeeWidth)))
    val gen_fptoint = rvs2_chunks.zip(fptoint_modules).map { case(rvs2, conv) =>
      conv.io.signedOut := ctrl_signed
      conv.io.roundingMode := Mux(ctrl_truncating, 1.U, io.pipe(0).bits.frm)
      conv.io.in := fType.recode(rvs2)
      conv.io.out
    }

    // Int to FP
    val inttofp_modules = Seq.fill(num_chunks)(Module(new hardfloat.INToRecFN(fType.ieeeWidth, fType.exp, fType.sig)))
    val gen_inttofp = rvs2_chunks.zip(inttofp_modules).map { case(rvs2, conv) =>
      conv.io.signedIn := ctrl_signed
      conv.io.roundingMode := io.pipe(0).bits.frm
      conv.io.detectTininess := hardfloat.consts.tininess_afterRounding
      conv.io.in := rvs2
      fType.ieee(conv.io.out)
    }

    Mux(ctrl_out, gen_inttofp.asUInt, gen_fptoint.asUInt)
  }

  val single_width_out = Mux1H(eew_select, single_width_conversions)

  // Widening Conversions
  val widening_conversions = fTypes.zipWithIndex.filter(_._1.ieeeWidth <= 32).map { case (fType, i) =>
    val num_converts = dLen / (2 * fType.ieeeWidth)
    val in_sew = log2Ceil(fType.ieeeWidth / 8)
    val wideType = fTypes(i+1)

    // Int to FP conversions
    val wide_inttofp_modules = Seq.fill(num_converts)(Module(new hardfloat.INToRecFN(fType.ieeeWidth, wideType.exp, wideType.sig)))
    val gen_inttofp = wide_inttofp_modules.zipWithIndex.map { case(wide, idx) =>
      wide.io.signedIn := ctrl_signed
      wide.io.roundingMode := io.pipe(0).bits.frm
      wide.io.detectTininess := hardfloat.consts.tininess_afterRounding
      wide.io.in := extractElem(rvs2_data, in_sew.U, io.pipe(0).bits.eidx + idx.U)(fType.ieeeWidth-1,0)
      wideType.ieee(wide.io.out)
    }.asUInt

    // FP to FP conversions
    val wide_fptofp_modules = Seq.fill(num_converts)(Module(new hardfloat.RecFNToRecFN(fType.exp, fType.sig, wideType.exp, wideType.sig)))
    val gen_fptofp = wide_fptofp_modules.zipWithIndex.map{ case(wide, idx) =>
      wide.io.in := fType.recode(extractElem(rvs2_data, in_sew.U, io.pipe(0).bits.eidx + idx.U)(fType.ieeeWidth-1,0))
      wide.io.roundingMode := io.pipe(0).bits.frm
      wide.io.detectTininess := hardfloat.consts.tininess_afterRounding
      wideType.ieee(wide.io.out)
    }.asUInt

    // FP to Int conversions
    val wide_fptoint_modules = Seq.fill(num_converts)(Module(new hardfloat.RecFNToIN(fType.exp, fType.sig, wideType.ieeeWidth)))
    val gen_fptoint = wide_fptoint_modules.zipWithIndex.map{ case(wide, idx) =>
      val extracted_rvs2_bits = extractElem(rvs2_data, in_sew.U, io.pipe(0).bits.eidx + idx.U)(fType.ieeeWidth-1,0)
      wide.io.signedOut := ctrl_signed
      wide.io.roundingMode := Mux(ctrl_truncating, 1.U, io.pipe(0).bits.frm)
      wide.io.in := fType.recode(extracted_rvs2_bits)
      wide.io.out
    }.asUInt

    Mux1H(Seq(!rs1(2) && rs1(1), rs1(2) && !rs1(1), (!rs1(2) && !rs1(1)) || (rs1(2) && rs1(1))),
          Seq(gen_inttofp      , gen_fptofp       , gen_fptoint))
  }

  val widening_out = Mux1H(Seq(vd_eew === 2.U, vd_eew === 3.U), widening_conversions)

  // Narrowing Conversions
  // Just EEW of 32 and 64
  val narrowing_conversions = fTypes.zipWithIndex.filter(_._1.ieeeWidth >= 32).map { case (fType, i) =>
    val num_converts = dLen / fType.ieeeWidth
    val in_sew = log2Ceil(fType.ieeeWidth / 8)
    val narrowType = fTypes(i-1)

    // Int to FP Conversions
    val narrow_inttofp_modules = Seq.fill(num_converts)(Module(new hardfloat.INToRecFN(fType.ieeeWidth, narrowType.exp, narrowType.sig)))
    val gen_inttofp = narrow_inttofp_modules.zipWithIndex.map { case(narrow, idx) =>
      narrow.io.signedIn := ctrl_signed
      narrow.io.roundingMode := io.pipe(0).bits.frm
      narrow.io.detectTininess := hardfloat.consts.tininess_afterRounding
      narrow.io.in := extractElem(rvs2_data, in_sew.U, io.pipe(0).bits.eidx + idx.U)(fType.ieeeWidth-1,0)
      narrowType.ieee(narrow.io.out)
    }.asUInt

    // FP to FP Conversions
    val fptofp_modules = Seq.fill(num_converts)(Module(new hardfloat.RecFNToRecFN(fType.exp, fType.sig, narrowType.exp, narrowType.sig)))
    val gen_fptofp = fptofp_modules.zipWithIndex.map{ case(narrow, idx) =>
      narrow.io.in := fType.recode(extractElem(rvs2_data, in_sew.U, io.pipe(0).bits.eidx + idx.U)(fType.ieeeWidth-1,0))
      narrow.io.roundingMode := Mux(ctrl_round_to_odd, "b110".U, io.pipe(0).bits.frm)
      narrow.io.detectTininess := hardfloat.consts.tininess_afterRounding
      narrowType.ieee(narrow.io.out)
    }.asUInt

    // FP to Int Conversions
    val fptoint_modules = Seq.fill(num_converts)(Module(new hardfloat.RecFNToIN(fType.exp, fType.sig, narrowType.ieeeWidth)))
    val gen_fptoint = fptoint_modules.zipWithIndex.map { case(conv, idx) =>
      val extracted_rvs2_bits = extractElem(rvs2_data, in_sew.U, io.pipe(0).bits.eidx + idx.U)(fType.ieeeWidth-1,0)
      conv.io.signedOut := ctrl_signed
      conv.io.roundingMode := Mux(ctrl_truncating, 1.U, io.pipe(0).bits.frm)
      conv.io.in := fType.recode(extracted_rvs2_bits)
      conv.io.out
    }.asUInt

    Mux1H(Seq(!rs1(2) && rs1(1), rs1(2) && !rs1(1), (!rs1(2) && !rs1(1)) || (rs1(2) && rs1(1))),
          Seq(gen_inttofp      , gen_fptofp       , gen_fptoint))
  }

  // Special Case for FP16 narrowing converts
  // Only narrowing FP to Int
  val fp16_fptoint_modules = Seq.fill(dLen/16)(Module(new hardfloat.RecFNToIN(FType.H.exp, FType.H.sig, FType.H.ieeeWidth/2)))
  val fp16_gen_fptoint = fp16_fptoint_modules.zipWithIndex.map { case(conv, idx) =>
    val extracted_rvs2_bits = extractElem(rvs2_data, 1.U, io.pipe(0).bits.eidx + idx.U)(FType.H.ieeeWidth-1,0)
    conv.io.signedOut := ctrl_signed
    conv.io.roundingMode := io.pipe(0).bits.frm
    conv.io.in := FType.H.recode(Mux(ctrl_truncating, extracted_rvs2_bits(FType.H.ieeeWidth-1, FType.H.sig-2) << (FType.H.sig - 2), extracted_rvs2_bits))
    conv.io.out
  }.asUInt


  val narrowing_out = Fill(2, Mux1H(Seq(vd_eew === 0.U, vd_eew === 1.U, vd_eew === 2.U), Seq(fp16_gen_fptoint) ++ narrowing_conversions))

  val pipe_out = Pipe(io.pipe(0).valid, Mux1H(Seq(!ctrl_widen && !ctrl_narrow, ctrl_widen, ctrl_narrow),
                                              Seq(single_width_out, widening_out, narrowing_out))).bits

  io.write.valid := io.pipe(depth-1).valid
  io.write.bits.eg := io.pipe(depth-1).bits.wvd_eg
  io.write.bits.mask := FillInterleaved(8, io.pipe(depth-1).bits.wmask)
  io.write.bits.data := pipe_out

  io.set_fflags := DontCare
  io.scalar_write.valid := false.B
  io.scalar_write.bits := DontCare
  io.pipe0_stall := false.B
}
