package saturn.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import saturn.common._
import saturn.insns._

case object SharedFPMiscFactory extends FunctionalUnitFactory {
  def insns = Seq(
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
  ).map(_.elementWise)
  def generate(implicit p: Parameters) = new SharedScalarElementwiseFPMisc()(p)
}

class SharedScalarElementwiseFPMisc(implicit p: Parameters) extends IterativeFunctionalUnit()(p)
    with HasFPUParameters
    with HasSharedFPUIO {

  val fp_req = Wire(Decoupled(new FPInput))
  io_fp_req <> fp_req

  val supported_insns = SharedFPMiscFactory.insns

  io.iss.ready := new VectorDecoder(io.iss.op.funct3, io.iss.op.funct6, 0.U, 0.U, supported_insns, Nil).matched && !valid && io_fp_req.ready

  val ctrl = new VectorDecoder(op.funct3, op.funct6, 0.U, 0.U, supported_insns, Seq(
    FPSwapVdV2, ReadsVD, WritesAsMask, FPSgnj, FPComp, FPSpecRM, FPMNE, FPMGT, Wide2VD, Wide2VS2, Reduction))

  val issued = Reg(Bool())
  val has_wdata = Reg(Bool())
  val wdata = Reg(UInt(64.W))
  when (io.iss.valid && io.iss.ready) {
    issued := false.B
    has_wdata := false.B
  }

  val vs1_eew = op.rvs1_eew
  val vs2_eew = op.rvs2_eew
  val vd_eew  = op.vd_eew
  val vd_eew64 = op.vd_eew64
  val vd_eew32 = op.vd_eew32
  val vd_eew16 = op.vd_eew16
  val eidx = Mux(op.acc, 0.U, op.eidx)

  val ctrl_isDiv = op.opff6.isOneOf(OPFFunct6.fdiv, OPFFunct6.frdiv)
  val ctrl_funary0 = op.opff6.isOneOf(OPFFunct6.funary0)
  val ctrl_funary1 = op.opff6.isOneOf(OPFFunct6.funary1)
  val ctrl_vfclass = ctrl_funary1 && (op.rs1 === 16.U)
  val ctrl_swap12 = op.opff6.isOneOf(OPFFunct6.frdiv)

  val rs1 = op.rs1
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

  io.hazard.valid := valid
  io.hazard.bits.eg := op.wvd_eg

  io_fp_active := valid && issued

  // Create FPInput
  val req = Wire(new FPInput)
  req.ldst := false.B
  req.wen := false.B
  req.ren1 := true.B
  req.ren2 := !(ctrl_funary0 || ctrl_funary1)
  req.ren3 := false.B
  req.swap12 := false.B
  req.swap23 := false.B
  req.typeTagIn := Mux1H(UIntToOH(op.rvs2_eew), Seq(S, H, S, D))
  req.typeTagOut := Mux1H(UIntToOH(op.rvd_eew), Seq(S, H, S, D))
  req.fromint := ctrl_inttofp
  req.toint := (ctrl_fptoint) || ctrl_vfclass || ctrl.bool(WritesAsMask)
  req.fastpipe := ctrl_fptofp || ctrl.bool(FPSgnj) || ctrl.bool(FPComp)
  req.fma := false.B
  req.div := ctrl_isDiv
  req.sqrt := ctrl_funary1 && (rs1 === 0.U)
  req.wflags := !ctrl_vfclass && !ctrl.bool(FPSgnj)
  req.vec := true.B
  req.rm := Mux(ctrl_fptofp && ctrl_round_to_odd, "b110".U, Mux(ctrl_fptoint && ctrl_truncating, 1.U, Mux((!ctrl_isDiv && !ctrl_funary1 && !ctrl_funary0) || ctrl_vfclass, ctrl.uint(FPSpecRM), op.frm)))
  req.fmaCmd := 0.U
  req.typ := Mux(ctrl_funary0, Cat((ctrl_inttofp && ctrl_narrow) || (ctrl_fptoint && ctrl_widen) || (ctrl_single_wide && vd_eew64), !ctrl_signed), 0.U)
  req.fmt := 0.U

  val rvs2_elem = op.rvs2_elem
  val rvs1_elem = op.rvs1_elem
  val rvd_elem  = op.rvd_elem

  val h_rvs2_int = rvs2_elem(15,0)
  val h_rvs2_fp = FType.H.recode(Mux(ctrl_funary0 && ctrl_truncating, rvs2_elem(15,9) << 9, rvs2_elem(15,0)))
  val h_rvs2_unbox = unbox(box(h_rvs2_fp, FType.H), H, None)

  val h_rvs1 = FType.H.recode(rvs1_elem(15,0))
  val h_rvs1_unbox = unbox(box(h_rvs1, FType.H), H, None)
  val h_rvd = FType.H.recode(rvd_elem(15,0))

  val s_rvs2_int = rvs2_elem(31,0)
  val s_rvs2_fp = FType.S.recode(rvs2_elem(31,0))
  val s_rvs2_unbox = unbox(box(s_rvs2_fp, FType.S), S, None)

  val s_rvs1 = FType.S.recode(rvs1_elem(31,0))
  val s_rvs1_unbox = unbox(box(s_rvs1, FType.S), S, None)
  val s_rvd = FType.S.recode(rvd_elem(31,0))

  val d_rvs2_int = rvs2_elem
  val d_rvs2_fp = FType.D.recode(rvs2_elem)

  val d_rvs1 = FType.D.recode(rvs1_elem)
  val d_rvd = FType.D.recode(rvd_elem)

  val h_isNaN = FType.H.isNaN(h_rvs2_fp) || FType.H.isNaN(h_rvs1)
  val s_isNaN = FType.S.isNaN(s_rvs2_fp) || FType.S.isNaN(s_rvs1)
  val d_isNaN = FType.D.isNaN(d_rvs2_fp) || FType.D.isNaN(d_rvs1)

  val mgt_NaN = ctrl.bool(WritesAsMask) && ctrl.bool(FPMGT) && ((vd_eew64 && d_isNaN) || (vd_eew32 && s_isNaN) || (vd_eew16 && h_isNaN))
  val mgt_NaN_reg = RegInit(false.B)

  // Set req.in1
  when (ctrl_swap12) {
    req.in1 := Mux(vd_eew64, d_rvs1, Mux(vd_eew32, s_rvs1_unbox, h_rvs1_unbox))
  } .elsewhen (ctrl_inttofp) {
    req.in1 := rvs2_elem
  } .otherwise {
    req.in1 := Mux(vd_eew64 && (!ctrl_widen || (ctrl_funary0 && ctrl_narrow)), d_rvs2_fp, Mux(vd_eew32 && (!ctrl_widen || (ctrl_funary0 && ctrl_narrow)), s_rvs2_unbox, h_rvs2_unbox))
  }

  // Set req.in2
  when (ctrl_swap12) {
    req.in2 := Mux(vd_eew64, d_rvs2_fp, Mux(vd_eew32, s_rvs2_unbox, h_rvs2_unbox))
  } .otherwise {
    req.in2 := Mux(vd_eew64, d_rvs1, Mux(vd_eew32, s_rvs1_unbox, h_rvs1_unbox))
  }

  // Set req.in3
  req.in3 := 0.U

  fp_req.bits := req
  fp_req.valid := valid && !issued && !vfrsqrt7_inst && !vfrec7_inst && !mgt_NaN
  when (fp_req.fire) { issued := true.B }

  // Approximation Instructions

  // Reciprocal Sqrt Approximation
  val recSqrt7 = Module(new VFRSQRT7)
  recSqrt7.io.rvs2_input := rvs2_elem
  recSqrt7.io.eew := op.rvs2_eew

  // Reciprocal Approximation
  val rec7 = Module(new VFREC7)
  rec7.io.rvs2_input := rvs2_elem
  rec7.io.eew := op.rvs2_eew
  rec7.io.frm := op.frm

  when (io_fp_resp.valid) {
    has_wdata := true.B
    when (ctrl.bool(WritesAsMask)) {
      when (ctrl.bool(FPMNE) || (ctrl.bool(FPMGT) && !mgt_NaN)) {
        wdata := Fill(dLen, !io_fp_resp.bits.data(0))
      } .elsewhen (ctrl.bool(FPMGT) && mgt_NaN) {
        wdata := Fill(dLen, 0.U)
      } .otherwise {
        wdata := Fill(dLen, io_fp_resp.bits.data(0))
      }
    } .elsewhen (vfclass_inst) {
      wdata := Mux(vd_eew64, Cat(0.U(54.W), io_fp_resp.bits.data(9,0)), Fill(2, Cat(0.U(22.W), io_fp_resp.bits.data(9,0))))
    } .elsewhen (ctrl_fptoint) {
      wdata := Mux(vd_eew64, io_fp_resp.bits.data(63,0), Fill(2, io_fp_resp.bits.data(31,0)))
    } .otherwise {
      wdata := Mux(vd_eew64, FType.D.ieee(io_fp_resp.bits.data), Mux(vd_eew32, Fill(2, FType.S.ieee(unbox(io_fp_resp.bits.data, 0.U, Some(FType.S)))),
                                                                               Fill(4, FType.H.ieee(unbox(io_fp_resp.bits.data, H, Some(FType.H))))))
    }
  }

  val mask_write_offset = VecInit.tabulate(4)({ eew =>
    Cat(op.eidx(log2Ceil(dLen)-1, dLenOffBits-eew), 0.U((dLenOffBits-eew).W))
  })(op.vd_eew)
  val mask_write_mask = (VecInit.tabulate(4)({ eew =>
    VecInit(op.wmask.asBools.grouped(1 << eew).map(_.head).toSeq).asUInt
  })(op.vd_eew) << mask_write_offset)(dLen-1,0)

  io.write.valid := (has_wdata || vfrsqrt7_inst || vfrec7_inst || mgt_NaN) && valid
  io.write.bits.eg := op.wvd_eg
  io.write.bits.mask := Mux(ctrl.bool(WritesAsMask), mask_write_mask, FillInterleaved(8, op.wmask))
  io.write.bits.data := Mux1H(Seq(vfrsqrt7_inst, vfrec7_inst, has_wdata),
                              Seq(Fill(dLenB >> 3, recSqrt7.io.out), Fill(dLenB >> 3, rec7.io.out), Fill(dLenB >> 3, wdata)))

  last := io.write.fire

  io.set_fflags := DontCare
  io.scalar_write.valid := false.B
  io.scalar_write.bits := DontCare
  io.set_vxsat := false.B

  io.acc := op.acc
  io.tail := op.tail
}
