package vector.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import vector.common._

class FPConvPipe(implicit p: Parameters) extends PipelinedFunctionalUnit(1, true)(p) with HasFPUParameters {
  io.iss.sub_dlen := 0.U
  io.set_vxsat := false.B

  lazy val opcodes = Seq(
    OPFFunct6.vfunary0
  )
  override def accepts(f3: UInt, f6: UInt): Bool = VecDecode(f3, f6, opcodes)

  val rs1 = io.pipe(0).bits.rs1
  val ctrl_widen = rs1(3)
  val ctrl_narrow = rs1(4)
  val ctrl_signed = rs1(0)
  val ctrl_out = rs1(2)
  val ctrl_truncating = rs1(1) // truncate or round towards odd
  val ctrl_round_to_odd = rs1(1) // truncate or round towards odd

  val rvs2_data = io.pipe(0).bits.rvs2_data

  val fTypes = Seq(FType.S, FType.D)

  val single_width_out = Wire(Vec(2, UInt(dLen.W)))
  //val widening_out = Wire(Vec(2, UInt((2*dLen).W)))

  // This is the single width generator
  // Generate circuits for FPToInt, IntToFP, and FPToFP
  for(eew <- 2 until 4) {
    val fType = fTypes(eew - 2)
    val num_chunks = dLen / fType.ieeeWidth
    val rvs2_chunks = rvs2_data.asTypeOf(Vec(num_chunks, UInt(fType.ieeeWidth.W)))

    // FP to Int
    val fptoint_modules = Seq.fill(num_chunks)(Module(new hardfloat.RecFNToIN(fType.exp, fType.sig, fType.ieeeWidth)))
    val gen_fptoint = rvs2_chunks.zip(fptoint_modules).map { case(rvs2, conv) =>
      val rvs2_rec = fType.recode(Mux(ctrl_truncating, Cat(rvs2(fType.ieeeWidth-1,fType.sig), 0.U(fType.sig.W)), rvs2))
      conv.io.signedOut := ctrl_signed
      conv.io.roundingMode := io.pipe(0).bits.frm
      conv.io.in := rvs2_rec
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
      conv.out
    }

    //single_width_out(eew-2) := Fill(eew + 1, Mux(ctrl_out, gen_inttofp.out.asUInt, gen_fptoint.out.asUInt)) 
    //single_width_out(eew-2) := Mux(ctrl_out, gen_inttofp.out.asUInt, gen_fptoint.out.asUInt) 
    single_width_out(eew-2) := Mux(ctrl_out, inttofp_results.asUInt, fptoint_results.asUInt) 
  }

  // Widening conversions
  //for (eew <- 2 until 4) {
  //  fType = fTypes(eew - 2)
  //  val num_chunks = dLen / fType.ieeeWidth
  //  val rvs2_chunks = rvs2_data.asTypeOf(Vec(num_chunks, UInt(fType.ieeeWidth.W)))

  //  // FP to Int
  //  // Only valid for S -> 64b integer
  //  // 00X
  //  val fptoint_modules = Seq.fill(num_chunks)(Module(new hardfloat.RecFNToIN(fType.exp, fType.sig, 2*fType.ieeeWidth)))
  //  val gen_fptoint = rvs2_chunks.zip(fptoint_modules).map { case(rvs2, conv) =>
  //    val rvs2_rec = fType.recode(Mux(ctrl_truncating, Cat(rvs2(fType.ieeeWidth-1,fType.sig), 0.U((fType.sig-1).W)), rvs2))
  //    conv.io.signedOut := ctrl_signed
  //    conv.io.roundingMode := io.pipe(0).bits.frm
  //    conv.io.in := rvs2_rec
  //    conv.io
  //  } 
  //  

  //  // Int to FP
  //  // 16b int -> S, 32b int -> D
  //  // 01X
  //  val inttofp_modules = Seq.fill(num_chunks)(Module(new hardfloat.INToRecFN(fType.ieeeWidth / 2, fType.exp, fType.sig)))
  //  val gen_inttofp = rvs2_data.asTypeOf(Vec(num_chunks*2, UInt((fType.ieeeWidth/2).W))).zip(inttofp_modules).map { case(rvs2, conv) =>
  //    conv.io.signedIn := ctrl_signed
  //    conv.io.roundingMode := io.pipe(0).bits.frm
  //    conv.io.detectTininess := hardfloat.consts.tininess_afterRounding
  //    conv.io.in := rvs2
  //    conv.io
  //  }

  //  // FP to FP
  //  // S -> D
  //  // 100
  //  val fptofp_modules = Seq.fill(num_chunks)(Module(new hardfloat.RecFNToRecFN(fType.exp, fType.sig, fType.exp*2, fType.sig*2)))
  //  val gen_fptofp = rvs2_chunks.zip(fptofp_Modules).map { case(rvs2, conv) =>
  //    conv.io.in := fType.recode(rvs2)
  //    conv.io.roundingMode := io.pipe(0).bits.frm
  //    conv.io.detectTininess := hardfloat.consts.tininess_afterRounding
  //    conv.io
  //  }
  //  

  //  if (eew == 2) {
  //    when (!rs1(1) && !rs1(2)) {
  //      widening_out(0) := gen_fptoint.out.asUInt
  //    } .elsewhen (!rs1(2)) {
  //      widening_out(0) := gen_inttofp.out.asUInt 
  //    } .otherwise {
  //      widening_out(0) := gen_fptofp.out.asUInt
  //    }
  //  } else {
  //    widening(eew - 2) := Mux(!rs1(1) && !rs1(0), gen_fptoint.out.asUInt, gen_inttofp.out.asUInt)
  //  }
  //}

  //val widening_out_final = Wire(UInt((2*dLen).W))
  //widening_out_final := Mux(io.pipe(0).bits.rvs2_eew === 3.U, widening_out(1), widening_out(0))
  val single_out_final = Wire(UInt(dLen.W))
  single_out_final := Mux(io.pipe(0).bits.rvs2_eew === 3.U, single_width_out(1), single_width_out(0))

  io.write.valid := io.pipe(0).valid
  io.write.bits.eg := io.pipe(0).bits.wvd_eg >> 1
  io.write.bits.mask := Fill(2, FillInterleaved(8, io.pipe(0).bits.wmask))
  //io.write.bits.data := Mux(ctrl_widen, widening_out_final, Fill(2, single_out_final))
  io.write.bits.data := Fill(2, single_out_final)

  io.exc.valid := false.B
  io.exc.bits := 0.U
}
