package saturn.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import saturn.common._
import saturn.insns._


class ElementwiseFPUFMA(depth: Int)(implicit p: Parameters) extends PipelinedFunctionalUnit(depth)(p) with HasFPUParameters {
  val io_fp_req = IO(Decoupled(new FPInput()))
  val io_fp_resp = IO(Flipped(Decoupled(new FPResult())))

  val supported_insns = Seq(
    FADD.VV, FADD.VF, FSUB.VV, FSUB.VF, FRSUB.VF,
    FMUL.VV, FMUL.VF,
    FMACC.VV, FMACC.VF, FNMACC.VV, FNMACC.VF,
    FMSAC.VV, FMSAC.VF, FNMSAC.VV, FNMSAC.VF,
    FMADD.VV, FMADD.VF, FNMADD.VV, FNMADD.VF,
    FMSUB.VV, FMSUB.VF, FNMSUB.VV, FNMSUB.VF,
    FWADD.VV, FWADD.VF, FWSUB.VV, FWSUB.VF,
    FWADDW.VV, FWADDW.VF, FWSUBW.VV, FWSUBW.VF,
    FWMUL.VV, FWMUL.VF,
    FWMACC.VV, FWMACC.VF, FWNMACC.VV, FWNMACC.VF,
    FWMSAC.VV, FWMSAC.VF, FWNMSAC.VV, FWNMSAC.VF,
    FREDOSUM.VV, FREDUSUM.VV, FWREDOSUM.VV, FWREDUSUM.VV,
  )

  val ctrl = new VectorDecoder(io.pipe(0).bits.funct3, io.pipe(0).bits.funct6, 0.U, 0.U, supported_insns, Seq(
    FPAdd, FPMul, FPSwapVdV2, FPFMACmd, ReadsVD, FPSpecRM, Wide2VD, Wide2VS2, Reduction))

  val vs1_eew = io.pipe(0).bits.rvs1_eew
  val vs2_eew = io.pipe(0).bits.rvs2_eew
  val vd_eew  = io.pipe(0).bits.vd_eew
  val vd_eew64 = io.pipe(0).bits.vd_eew64
  val eidx = Mux(io.pipe(0).bits.acc, 0.U, io.pipe(0).bits.eidx)

  // Functional unit is ready if not currently running and the scalar FPU is available
  io.iss.ready := new VectorDecoder(io.iss.op.funct3, io.iss.op.funct6, 0.U, 0.U, supported_insns, Nil).matched && io_fp_req.ready
  io.iss.sub_dlen := dLenOffBits.U - Mux(ctrl.bool(Wide2VS2), io.iss.op.rvs2_eew, io.iss.op.vd_eew)

  // Create FPInput 
  val req = Wire(new FPInput)
  req.ldst := false.B
  req.wen := false.B
  req.ren1 := true.B
  req.ren2 := true.B
  req.ren3 := ctrl.bool(ReadsVD)
  req.swap12 := false.B
  req.swap23 := ctrl.bool(FPAdd) && !ctrl.bool(FPMul)
  req.typeTagIn := Mux(vd_eew64, D, S)
  req.typeTagOut := Mux(vd_eew64, D, S)
  req.fromint := false.B
  req.toint := false.B  
  req.fastpipe := false.B
  req.fma := true.B
  req.div := false.B
  req.sqrt := false.B
  req.wflags := true.B 
  req.vec := true.B
  req.rm := io.pipe(0).bits.frm
  req.fmaCmd := ctrl.uint(FPFMACmd)
  req.typ := 0.U
  req.fmt := 0.U

  val rvs2_extract = extract(io.pipe(0).bits.rvs2_data, false.B, vs2_eew, eidx)(63,0)
  val rvs1_extract = extract(io.pipe(0).bits.rvs1_data, false.B, vs1_eew, eidx)(63,0) 
  val rvd_extract = extract(io.pipe(0).bits.rvd_data, false.B, vd_eew, eidx)(63,0) 

  val s_rvs2 = FType.S.recode(rvs2_extract(31,0))
  val s_rvs1 = FType.S.recode(rvs1_extract(31,0))
  val s_rvd = FType.S.recode(rvd_extract(31,0))

  // For widening operations, widen the narrow operands to compute with the scalar FPU 
  val widen_rvs1 = Module(new hardfloat.RecFNToRecFN(8, 24, 11, 53))
  widen_rvs1.io.in := s_rvs1
  widen_rvs1.io.roundingMode := io.pipe(0).bits.frm
  widen_rvs1.io.detectTininess := hardfloat.consts.tininess_afterRounding

  val widen_rvs2 = Module(new hardfloat.RecFNToRecFN(8, 24, 11, 53))
  widen_rvs2.io.in := s_rvs2
  widen_rvs2.io.roundingMode := io.pipe(0).bits.frm
  widen_rvs2.io.detectTininess := hardfloat.consts.tininess_afterRounding

  val d_rvs2 = FType.D.recode(rvs2_extract)
  val d_rvs1 = FType.D.recode(rvs1_extract)
  val d_rvd = FType.D.recode(rvd_extract)

  val rvs2_elem = Mux(vd_eew64, d_rvs2, s_rvs2)
  val rvs1_elem = Mux(vd_eew64, d_rvs1, s_rvs1)
  val rvd_elem =  Mux(vd_eew64, d_rvd, s_rvd)

  // Set req.in1
  when (ctrl.bool(FPSwapVdV2)) {
    req.in1 := rvd_elem
  } .elsewhen (vs2_eew === 3.U) {
    req.in1 := d_rvs2 
  } .elsewhen (ctrl.bool(Wide2VD)) {
    req.in1 := widen_rvs2.io.out
  } .otherwise {
    req.in1 := s_rvs2 
  }
  
  // Set req.in2
  when (vs1_eew === 3.U) {
    req.in2 := d_rvs1
  } .elsewhen (ctrl.bool(Wide2VD) && !io.pipe(0).bits.acc) {
    req.in2 := widen_rvs1.io.out
  } .otherwise {
    req.in2 := s_rvs1
  }
  
  // Set req.in3
  when (ctrl.bool(FPSwapVdV2)) {
    req.in3 := rvs2_elem
  } .otherwise {
    req.in3 := rvd_elem
  }

  io_fp_req.bits := req
  io_fp_req.valid := io.pipe(0).valid 
  io_fp_resp.ready := true.B

  io.write.valid := io.pipe(depth-1).valid
  io.write.bits.eg := io.pipe(depth-1).bits.wvd_eg
  io.write.bits.mask := FillInterleaved(8, io.pipe(depth-1).bits.wmask)
  io.write.bits.data := Fill(dLenB >> 3, Mux(io.pipe(depth-1).bits.vd_eew === 3.U, FType.D.ieee(io_fp_resp.bits.data), Fill(2, FType.S.ieee(unbox(io_fp_resp.bits.data, 0.U, Some(FType.S))))))

  io.set_fflags := DontCare
  io.scalar_write.valid := false.B
  io.scalar_write.bits := DontCare
  io.set_vxsat := false.B
}


class ElementwiseFPU(implicit p: Parameters) extends IterativeFunctionalUnit()(p) with HasFPUParameters {
  val io_fp_req = IO(Decoupled(new FPInput()))
  val io_fp_resp = IO(Flipped(Decoupled(new FPResult())))

  val supported_insns = Seq(
    FDIV.VV, FDIV.VF,
    FRDIV.VF,
    FSQRT_V,
    FRSQRT7_V,
    FREC7_V,
    FCLASS_V,
    FMIN.VV, FMIN.VF, FMAX.VV, FMAX.VF,
    FSGNJ.VV, FSGNJ.VF, FSGNJN.VV, FSGNJN.VF, FSGNJX.VV, FSGNJX.VF,
    MFEQ.VV, MFEQ.VF, MFNE.VV, MFNE.VF,
    MFLT.VV, MFLT.VF, MFLE.VV, MFLE.VF,
    MFGT.VF, MFGE.VF,
    FREDMIN.VV, FREDMAX.VV,
    FCVT_SGL, FCVT_WID, FCVT_NRW
  )

  val ctrl = new VectorDecoder(io.iss.op.funct3, io.iss.op.funct6, 0.U, 0.U, supported_insns, Seq(
    FPSwapVdV2, ReadsVD, WritesAsMask, FPSgnj, FPComp, FPSpecRM, FPMNE, FPMGT, Wide2VD, Wide2VS2, Reduction))

  val vs1_eew = io.iss.op.rvs1_eew
  val vs2_eew = io.iss.op.rvs2_eew
  val vd_eew  = io.iss.op.vd_eew
  val vd_eew64 = io.iss.op.vd_eew64
  val eidx = Mux(io.iss.op.acc, 0.U, io.iss.op.eidx)

  val ctrl_isDiv = io.iss.op.opff6.isOneOf(OPFFunct6.fdiv, OPFFunct6.frdiv)
  val ctrl_funary0 = io.iss.op.opff6.isOneOf(OPFFunct6.funary0)
  val ctrl_funary1 = io.iss.op.opff6.isOneOf(OPFFunct6.funary1)
  val ctrl_vfclass = ctrl_funary1 && (io.iss.op.rs1 === 16.U)
  val ctrl_swap12 = io.iss.op.opff6.isOneOf(OPFFunct6.frdiv)

  val rs1 = io.iss.op.rs1
  val ctrl_widen = ctrl_funary0 && rs1(3)
  val ctrl_narrow = rs1(4)
  val ctrl_single_wide = ctrl_funary0 && !ctrl_widen && !ctrl_narrow
  val ctrl_signed = rs1(0)
  val ctrl_truncating = rs1(2) && rs1(1)
  val ctrl_round_to_odd = rs1(0)
  val ctrl_fptoint = ctrl_funary0 && ((!rs1(2) && !rs1(1)) || (rs1(2) && rs1(1)))
  val ctrl_inttofp = ctrl_funary0 && (!rs1(2) && rs1(1)) 
  val ctrl_fptofp = ctrl_funary0 && (rs1(2) && !rs1(1))

  val vfclass_inst = op.opff6.isOneOf(OPFFunct6.funary1) && op.rs1 === 16.U && valid
  val vfrsqrt7_inst = op.opff6.isOneOf(OPFFunct6.funary1) && op.rs1 === 4.U && valid
  val vfrec7_inst = op.opff6.isOneOf(OPFFunct6.funary1) && op.rs1 === 5.U && valid

  // Functional unit is ready if not currently running and the scalar FPU is available
  io.iss.ready := new VectorDecoder(io.iss.op.funct3, io.iss.op.funct6, 0.U, 0.U, supported_insns, Nil).matched && !valid && io_fp_req.ready
  io.iss.sub_dlen := dLenOffBits.U - Mux(ctrl_funary0 && ctrl_narrow, io.iss.op.rvs2_eew, io.iss.op.rvd_eew)

  io.hazard.valid := valid
  io.hazard.bits.vat := op.vat
  io.hazard.bits.eg := op.wvd_eg

  // Create FPInput 
  val req = Wire(new FPInput)
  req.ldst := false.B
  req.wen := false.B
  req.ren1 := true.B
  req.ren2 := !(ctrl_funary0 || ctrl_funary1)
  req.ren3 := false.B
  req.swap12 := false.B
  req.swap23 := false.B
  req.typeTagIn := Mux(((ctrl_single_wide || !ctrl_funary0) && vd_eew64) || (ctrl_inttofp && ctrl_widen) || (ctrl_fptofp && ctrl_narrow), D, S)
  req.typeTagOut := Mux(((ctrl_single_wide || !ctrl_funary0) && vd_eew64) || (ctrl_fptoint && ctrl_narrow) || (ctrl_fptofp && ctrl_widen) || (ctrl_inttofp && ctrl_widen), D, S)
  req.fromint := ctrl_inttofp
  req.toint := (ctrl_fptoint) || ctrl_vfclass || ctrl.bool(WritesAsMask)  
  req.fastpipe := ctrl_fptofp || ctrl.bool(FPSgnj) || ctrl.bool(FPComp)
  req.fma := false.B 
  req.div := ctrl_isDiv
  req.sqrt := ctrl_funary1 && (rs1 === 0.U)
  req.wflags := !ctrl_vfclass && !ctrl.bool(FPSgnj) 
  req.vec := true.B
  req.rm := Mux(ctrl_fptofp && ctrl_round_to_odd, "b110".U, Mux((!ctrl_isDiv && !ctrl_funary1 && !ctrl_funary0) || ctrl_vfclass, ctrl.uint(FPSpecRM), io.iss.op.frm))
  req.fmaCmd := 0.U
  req.typ := Mux(ctrl_funary0, Cat((ctrl_inttofp && ctrl_narrow) || (ctrl_fptoint && ctrl_widen) || (ctrl_single_wide && vd_eew64), !ctrl_signed), 0.U)
  req.fmt := 0.U

  val rvs2_extract = extract(io.iss.op.rvs2_data, false.B, vs2_eew, eidx)(63,0)
  val rvs1_extract = extract(io.iss.op.rvs1_data, false.B, vs1_eew, eidx)(63,0) 
  val rvd_extract = extract(io.iss.op.rvd_data, false.B, vd_eew, eidx)(63,0) 

  val s_rvs2_int = rvs2_extract(31,0)
  val s_rvs2_fp = FType.S.recode(Mux(ctrl_funary0 && ctrl_truncating, rvs2_extract(31,22) << 22, rvs2_extract(31,0)))
  val s_rvs2_unbox = unbox(box(s_rvs2_fp, FType.S), S, None) 

  val s_rvs1 = FType.S.recode(rvs1_extract(31,0))
  val s_rvs1_unbox = unbox(box(s_rvs1, FType.S), S, None)
  val s_rvd = FType.S.recode(rvd_extract(31,0))

  val d_rvs2_int = rvs2_extract
  val d_rvs2_fp = FType.D.recode(Mux(ctrl_funary0 && ctrl_truncating, rvs2_extract(63, 51) << 51, rvs2_extract))

  val d_rvs1 = FType.D.recode(rvs1_extract)
  val d_rvd = FType.D.recode(rvd_extract)

  val s_isNaN = FType.S.isNaN(s_rvs2_fp) || FType.S.isNaN(s_rvs1)
  val d_isNaN = FType.D.isNaN(d_rvs2_fp) || FType.D.isNaN(d_rvs1)

  val mgt_NaN = ctrl.bool(WritesAsMask) && ctrl.bool(FPMGT) && ((vd_eew64 && d_isNaN) || (io.iss.op.vd_eew32 && s_isNaN)) 
  val mgt_NaN_reg = RegInit(false.B)

  when (io.iss.ready && io.iss.valid && mgt_NaN) {
    mgt_NaN_reg := true.B
  } .elsewhen (io.write.fire()) {
    mgt_NaN_reg := false.B
  }

  // Set req.in1
  when (ctrl_swap12) {
    req.in1 := Mux(vd_eew64, d_rvs1, s_rvs1_unbox)
  } .elsewhen (ctrl_inttofp) {
    req.in1 := Mux((vd_eew64 && !ctrl_widen) || (ctrl_funary0 && ctrl_narrow), d_rvs2_int, s_rvs2_int)  
  } .otherwise {
    req.in1 := Mux((vd_eew64 && !ctrl_widen) || (ctrl_funary0 && ctrl_narrow), d_rvs2_fp, s_rvs2_unbox)
  }    
  
  // Set req.in2
  when (ctrl_swap12) {
    req.in2 := Mux(vd_eew64, d_rvs2_fp, s_rvs2_unbox)
  } .otherwise {
    req.in2 := Mux(vd_eew64, d_rvs1, s_rvs1_unbox)
  }
  
  // Set req.in3
  req.in3 := 0.U

  io_fp_req.bits := req
  io_fp_req.valid := (io.iss.valid && io.iss.ready) && !vfrsqrt7_inst && !vfrec7_inst && !mgt_NaN 

  io_fp_resp.ready := io.write.ready

  // Approximation Instructions
  val rvs2_op_bits = extract(op.rvs2_data, false.B, op.rvs2_eew, op.eidx)(63,0)

  // Reciprocal Sqrt Approximation
  val recSqrt7 = Module(new VFRSQRT7)
  recSqrt7.io.rvs2_input := rvs2_op_bits
  recSqrt7.io.eew := op.rvs2_eew

  // Reciprocal Approximation
  val rec7 = Module(new VFREC7)
  rec7.io.rvs2_input := rvs2_op_bits
  rec7.io.eew := op.rvs2_eew
  rec7.io.frm := op.frm

  val write_bits = Wire(UInt(64.W))

  when (ctrl.bool(WritesAsMask)) {
    when (ctrl.bool(FPMNE) || (ctrl.bool(FPMGT) && !mgt_NaN_reg)) {
      write_bits := Fill(dLen, !io_fp_resp.bits.data(0))
    } .elsewhen (ctrl.bool(FPMGT) && mgt_NaN_reg) {
      write_bits := Fill(dLen, 0.U)
    } .otherwise {
      write_bits := Fill(dLen, io_fp_resp.bits.data(0)) 
    }
  } .elsewhen (vfclass_inst) {
    write_bits := Mux(vd_eew64, Cat(0.U(54.W), io_fp_resp.bits.data(9,0)), Fill(2, Cat(0.U(22.W), io_fp_resp.bits.data(9,0)))) 
  } .elsewhen (ctrl_fptoint) {
    write_bits := Mux(vd_eew64, io_fp_resp.bits.data(63,0), Fill(2, io_fp_resp.bits.data(31,0)))
  } .otherwise {
    write_bits := Mux(vd_eew64, FType.D.ieee(io_fp_resp.bits.data), Fill(2, FType.S.ieee(unbox(io_fp_resp.bits.data, 0.U, Some(FType.S)))))
  }

  val mask_bit = Mux(op.vd_eew64, op.wmask(0), Mux(op.eidx(0), op.wmask(4), op.wmask(0)))

  io.write.valid := (io_fp_resp.fire() || vfrsqrt7_inst || vfrec7_inst || mgt_NaN_reg) && valid
  io.write.bits.eg := op.wvd_eg
  io.write.bits.mask := Mux(ctrl.bool(WritesAsMask), ((1.U(dLen.W) & mask_bit) << (op.eidx % dLen.U)), FillInterleaved(8, op.wmask))
  io.write.bits.data := Mux1H(Seq(vfrsqrt7_inst, vfrec7_inst, io_fp_resp.fire()),
                              Seq(Fill(dLenB >> 3, recSqrt7.io.out), Fill(dLenB >> 3, rec7.io.out), Fill(dLenB >> 3, write_bits)))

  last := io.write.fire()

  io.set_fflags := DontCare
  io.scalar_write.valid := false.B
  io.scalar_write.bits := DontCare
  io.set_vxsat := false.B

  io.acc := io.iss.op.acc
  io.tail := io.iss.op.tail
}
