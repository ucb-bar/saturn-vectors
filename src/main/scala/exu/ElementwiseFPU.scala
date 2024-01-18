package vector.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import vector.common._

class ElementwiseFPU(implicit p: Parameters) extends IterativeFunctionalUnit()(p) with HasFPUParameters {

  io.iss.sub_dlen := log2Ceil(dLenB).U - io.iss.op.rvd_eew
  io.set_vxsat := false.B

  lazy val ctrl_table = Seq(
      (OPFFunct6.fadd,    Seq(N,Y,Y,N,N,Y,N,N,N,N,Y,N,N,N,N,Y,N,X,X,X,X,N)),
      (OPFFunct6.fsub,    Seq(N,Y,Y,N,N,Y,N,N,N,N,Y,Y,N,N,N,Y,N,X,X,X,X,N)),
      (OPFFunct6.frsub,   Seq(N,Y,Y,N,N,Y,N,N,N,N,Y,N,Y,N,N,Y,N,X,X,X,X,N)),
      (OPFFunct6.fwadd,   Seq(N,Y,Y,N,N,Y,N,N,N,N,Y,N,N,N,N,Y,N,X,X,X,X,N)),
      (OPFFunct6.fwsub,   Seq(N,Y,Y,N,N,Y,N,N,N,N,Y,Y,N,N,N,Y,N,X,X,X,X,N)),
      (OPFFunct6.fwaddw,  Seq(N,Y,Y,N,N,Y,N,N,N,N,Y,N,N,N,N,Y,N,X,X,X,X,N)),
      (OPFFunct6.fwsubw,  Seq(N,Y,Y,N,N,Y,N,N,N,N,Y,Y,N,N,N,Y,N,X,X,X,X,N)),
      (OPFFunct6.fmul,    Seq(N,Y,Y,N,N,N,N,N,N,N,Y,N,N,N,N,Y,N,X,X,X,X,N)),
      (OPFFunct6.fdiv,    Seq(N,Y,Y,N,N,N,N,N,N,N,N,N,N,Y,N,Y,N,X,X,X,X,N)),
      (OPFFunct6.frdiv,   Seq(N,Y,Y,N,Y,N,N,N,N,N,N,N,N,Y,N,Y,N,X,X,X,X,N)),
      (OPFFunct6.fwmul,   Seq(N,Y,Y,N,N,N,N,N,N,N,Y,N,N,N,N,Y,N,X,X,X,X,N)),
      (OPFFunct6.funary1, Seq(N,Y,N,N,N,N,N,N,N,N,N,N,N,N,Y,Y,N,X,X,X,X,N)),
      (OPFFunct6.fmacc,   Seq(N,Y,Y,Y,N,N,N,N,N,N,Y,N,N,N,N,Y,N,X,X,X,X,N)),
      (OPFFunct6.fnmacc,  Seq(N,Y,Y,Y,N,N,N,N,N,N,Y,Y,Y,N,N,Y,N,X,X,X,X,N)),
      (OPFFunct6.fmsac,   Seq(N,Y,Y,Y,N,N,N,N,N,N,Y,Y,N,N,N,Y,N,X,X,X,X,N)),
      (OPFFunct6.fnmsac,  Seq(N,Y,Y,Y,N,N,N,N,N,N,Y,N,Y,N,N,Y,N,X,X,X,X,N)),
      (OPFFunct6.fmadd,   Seq(N,Y,Y,Y,N,N,Y,N,N,N,Y,N,N,N,N,Y,N,X,X,X,X,N)),
      (OPFFunct6.fnmadd,  Seq(N,Y,Y,Y,N,N,Y,N,N,N,Y,Y,Y,N,N,Y,N,X,X,X,X,N)),
      (OPFFunct6.fmsub,   Seq(N,Y,Y,Y,N,N,Y,N,N,N,Y,Y,N,N,N,Y,N,X,X,X,X,N)),
      (OPFFunct6.fnmsub,  Seq(N,Y,Y,Y,N,N,Y,N,N,N,Y,N,Y,N,N,Y,N,X,X,X,X,N)),
      (OPFFunct6.fwmacc,  Seq(N,Y,Y,Y,N,N,N,N,N,N,Y,N,N,N,N,Y,N,X,X,X,X,N)),
      (OPFFunct6.fwnmacc, Seq(N,Y,Y,Y,N,N,N,N,N,N,Y,Y,Y,N,N,Y,N,X,X,X,X,N)),
      (OPFFunct6.fwmsac,  Seq(N,Y,Y,Y,N,N,N,N,N,N,Y,Y,N,N,N,Y,N,X,X,X,X,N)),
      (OPFFunct6.fwnmsac, Seq(N,Y,Y,Y,N,N,N,N,N,N,Y,N,Y,N,N,Y,N,X,X,X,X,N)),
      (OPFFunct6.fmin,    Seq(Y,Y,Y,N,N,N,N,N,N,Y,N,N,N,N,N,Y,Y,N,N,N,N,N)),
      (OPFFunct6.fmax,    Seq(Y,Y,Y,N,N,N,N,N,N,Y,N,N,N,N,N,Y,Y,N,N,Y,N,N)),
      (OPFFunct6.fsgnj,   Seq(Y,Y,Y,N,N,N,N,N,N,Y,N,N,N,N,N,N,Y,N,N,N,N,N)),
      (OPFFunct6.fsgnjn,  Seq(Y,Y,Y,N,N,N,N,N,N,Y,N,N,N,N,N,N,Y,N,N,Y,N,N)),
      (OPFFunct6.fsgnjx,  Seq(Y,Y,Y,N,N,N,N,N,N,Y,N,N,N,N,N,N,Y,N,Y,N,N,N)),
      (OPFFunct6.mfeq,    Seq(Y,Y,Y,N,N,N,N,N,Y,N,N,N,N,N,N,Y,Y,N,Y,N,N,Y)),
      (OPFFunct6.mfne,    Seq(Y,Y,Y,N,N,N,N,N,Y,N,N,N,N,N,N,Y,Y,N,Y,N,Y,Y)),
      (OPFFunct6.mflt,    Seq(Y,Y,Y,N,N,N,N,N,Y,N,N,N,N,N,N,Y,Y,N,N,Y,N,Y)),
      (OPFFunct6.mfle,    Seq(Y,Y,Y,N,N,N,N,N,Y,N,N,N,N,N,N,Y,Y,N,N,N,N,Y)),
      (OPFFunct6.mfgt,    Seq(Y,Y,Y,N,N,N,N,N,Y,N,N,N,N,N,N,Y,Y,N,N,N,Y,Y)),
      (OPFFunct6.mfge,    Seq(Y,Y,Y,N,N,N,N,N,Y,N,N,N,N,N,N,Y,Y,N,N,Y,Y,Y)),
      (OPFFunct6.funary0, Seq(N,Y,N,N,N,N,N,X,X,X,N,X,X,N,N,Y,Y,X,X,X,X,N)),
  )
  
  val ctrl_cmp :: ren1 :: ren2 :: ren3 :: swap12 :: swap23 :: swap13 :: fromint :: toint :: fastpipe :: fma :: fmaCmd0 :: fmaCmd1 :: div :: sqrt :: wflags :: special_rm :: rm2 :: rm1 :: rm0 :: inv_cmp :: writes_mask :: Nil = VecDecode.applyBools(
    io.iss.op.funct3, io.iss.op.funct6,
    Seq.fill(22)(X), ctrl_table)

  def accepts(f3: UInt, f6: UInt): Bool = VecDecode(f3, f6, ctrl_table.map(_._1))

  val vs1_eew = io.iss.op.rvs1_eew
  val vs2_eew = io.iss.op.rvs2_eew
  val vd_eew  = io.iss.op.vd_eew
  val ctrl_widen_vs2 = vs2_eew =/= vd_eew
  val ctrl_widen_vs1 = vs1_eew =/= vd_eew

  val vfclass_inst = op.opff6.isOneOf(OPFFunct6.funary1) && op.rs1 === 16.U
  val vfrsqrt7_inst = op.opff6.isOneOf(OPFFunct6.funary1) && op.rs1 === 4.U
  val vfrec7_inst = op.opff6.isOneOf(OPFFunct6.funary1) && op.rs1 === 5.U

  val is_funary0 = io.iss.op.opff6.isOneOf(OPFFunct6.funary0)
  val rs1 = io.iss.op.rs1
  val ctrl_widen = rs1(3)
  val ctrl_narrow = rs1(4)
  val ctrl_signed = rs1(0)
  val ctrl_out = !rs1(2) && rs1(1)
  val ctrl_truncating = rs1(2) && rs1(1)
  val ctrl_round_to_odd = rs1(0)

  io.hazard.valid := valid
  io.hazard.bits.vat := op.vat
  io.hazard.bits.eg := op.wvd_eg

  val eidx = io.iss.op.eidx
  val in_eew = io.iss.op.rvs2_eew
  val out_eew = io.iss.op.vd_eew
  val is_double = io.iss.op.vd_eew === 3.U 

  val req = Wire(new FPInput)
  req.ldst := false.B
  req.wen := false.B
  req.ren1 := ren1
  req.ren2 := ren2
  req.ren3 := ren3
  req.swap12 := false.B
  req.swap23 := swap23
  req.typeTagIn := Mux(vd_eew === 3.U, D, S)
  req.typeTagOut := Mux(vd_eew === 3.U, D, S)
  req.fromint := Mux(is_funary0, !rs1(2) && rs1(1), fromint)
  req.toint := Mux(is_funary0, (!rs1(2) && !rs1(1)) || (rs1(2) && rs1(1)), Mux(io.iss.op.opff6.isOneOf(OPFFunct6.funary1) && io.iss.op.rs1 === 16.U, true.B, toint))
  req.fastpipe := Mux(is_funary0, rs1(2) && !rs1(1), fastpipe)
  req.fma := fma
  req.div := div
  req.sqrt := sqrt && (io.iss.op.rs1 === 0.U)
  req.wflags := Mux(io.iss.op.opff6.isOneOf(OPFFunct6.funary1) && io.iss.op.rs1 === 16.U, false.B, wflags)
  req.vec := true.B
  
  req.rm := Mux(vfclass_inst, 1.U(3.W), Mux(special_rm, Cat(rm2, Cat(rm1, rm0)), io.iss.op.frm))
  req.fmaCmd := Cat(fmaCmd1, fmaCmd0) 
  req.typ := Mux(is_funary0, Cat(ctrl_widen || ctrl_narrow, !ctrl_signed), 0.U)
  req.fmt := 0.U

  val rvs2_extract = extract(io.iss.op.rvs2_data, false.B, in_eew, eidx)(63, 0)
  val rvs1_extract = extract(io.iss.op.rvs1_data, false.B, in_eew, eidx)(63, 0) 
  val rvd_extract = extract(io.iss.op.rvd_data, false.B, out_eew, eidx)(63, 0) 

  val s_rvs2 = FType.S.recode(rvs2_extract(31,0))
  val s_rvs1 = FType.S.recode(rvs1_extract(31,0))
  val s_rvd = FType.S.recode(rvd_extract(31,0))

  val d_rvs2 = FType.D.recode(rvs2_extract)
  val d_rvs1 = FType.D.recode(rvs1_extract)
  val d_rvd = FType.D.recode(rvd_extract)

  val rvs2_elem = Mux(is_double, d_rvs2, Mux(fma || (io.iss.op.opff6.isOneOf(OPFFunct6.funary1) && io.iss.op.rs1 === 16.U), s_rvs2, unbox(box(s_rvs2, FType.S), S, None)))
  //val rvs2_elem = Mux(is_double, d_rvs2, s_rvs2)
  val rvs1_elem = Mux(is_double, d_rvs1, Mux(fma || (io.iss.op.opff6.isOneOf(OPFFunct6.funary1) && io.iss.op.rs1 === 16.U), s_rvs1, unbox(box(s_rvs1, FType.S), S, None)))
  //val rvs1_elem = Mux(is_double, d_rvs1, s_rvs1)
  val rvd_elem = Mux(is_double, d_rvd, s_rvd)

  // widening circuitry
  val widen_rvs1 = Module(new hardfloat.RecFNToRecFN(8, 24, 11, 53))
  widen_rvs1.io.in := s_rvs1
  widen_rvs1.io.roundingMode := io.iss.op.frm
  widen_rvs1.io.detectTininess := hardfloat.consts.tininess_afterRounding

  val widen_rvs2 = Module(new hardfloat.RecFNToRecFN(8, 24, 11, 53))
  widen_rvs2.io.in := s_rvs2
  widen_rvs2.io.roundingMode := io.iss.op.frm
  widen_rvs2.io.detectTininess := hardfloat.consts.tininess_afterRounding

  req.in1 := Mux(ctrl_widen_vs2, widen_rvs2.io.out, Mux(swap13, rvd_elem, Mux(swap12, rvs1_elem, rvs2_elem)))
  req.in2 := Mux(ctrl_widen_vs1, widen_rvs1.io.out, Mux(swap12, rvs2_elem, rvs1_elem))
  req.in3 := Mux(swap13, rvs2_elem, rvd_elem)

  io.fp_req.bits := req
  io.fp_req.valid := (io.iss.valid && io.iss.ready) && !vfrsqrt7_inst && !vfrec7_inst 

  io.fp_resp.ready := io.write.ready

  io.iss.ready := accepts(io.iss.op.funct3, io.iss.op.funct6) && (!valid || last) && io.fp_req.ready

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

  val final_write_bits = Wire(UInt(64.W))

  when (writes_mask) {
    when (inv_cmp) {
      final_write_bits := Fill(dLen, !io.fp_resp.bits.data(0))
    } .otherwise {
     final_write_bits := Fill(dLen, io.fp_resp.bits.data(0)) 
    }
  } .elsewhen (vfclass_inst) {
    final_write_bits := Mux(vd_eew === 3.U, Cat(0.U(54.W), io.fp_resp.bits.data(9,0)), Fill(2, Cat(0.U(22.W), io.fp_resp.bits.data(9,0)))) 
  } .otherwise {
    final_write_bits := Mux(is_double, FType.D.ieee(io.fp_resp.bits.data), Fill(2, FType.S.ieee(unbox(io.fp_resp.bits.data, 0.U, Some(FType.S)))))
  }

  val mask_bit = Mux(op.vd_eew === 3.U, op.wmask(0), Mux(op.eidx(0), op.wmask(4), op.wmask(0)))
  //val mask_bit = Mux(op.vd_eew === 3.U, op.wmask.asBools(op.eidx % (dLenB >> 3).U), op.wmask.asBools(op.eidx % (dLenB >> 2).U))

  io.write.valid := io.fp_resp.fire() || ((vfrsqrt7_inst || vfrec7_inst) && valid)
  io.write.bits.eg := op.wvd_eg
  io.write.bits.mask := Mux(writes_mask, ((1.U(dLen.W) & mask_bit) << (op.eidx % dLen.U)), FillInterleaved(8, op.wmask))
  //io.write.bits.mask := (1.U(dLen.W) << Mux(op.vs2_eew === 3.U, (op.eidx-1.U) % dLenB.U >> 3.U, dLenB.U >> 2.U))
  //io.write.bits.mask := Mux(writes_mask, FillInterleaved(8, op.wmask) & (1.U(dLen.W) << ((io.iss.op.eidx - 1.U) % dLen.U)), FillInterleaved(8, op.wmask))
  io.write.bits.data := Mux1H(Seq(vfrsqrt7_inst, vfrec7_inst, io.fp_resp.fire()),
                              Seq(Fill(dLenB >> 3, recSqrt7.io.out), Fill(dLenB >> 3, rec7.io.out), Fill(dLenB >> 3, final_write_bits)))
  //io.write.bits.data := Fill(dLenB >> 3, final_write_bits)
  //io.write.bits.data := Fill(dLenB >> 3, Mux(is_double, FType.D.ieee(io.fp_resp.bits.data), Fill(2, FType.S.ieee(unbox(io.fp_resp.bits.data, 0.U, Some(FType.S)))))) 

  last := io.write.fire()

  io.set_fflags := DontCare

  io.scalar_write.valid := false.B
  io.scalar_write.bits := DontCare
}
