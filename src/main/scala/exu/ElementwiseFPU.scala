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
      (OPFFunct6.fadd,    Seq(N,Y,Y,N,N,Y,N,N,N,Y,N,N,N,N,Y)),
      (OPFFunct6.fsub,    Seq(N,Y,Y,N,N,Y,N,N,N,Y,N,Y,N,N,Y)),
      (OPFFunct6.frsub,   Seq(N,Y,Y,N,N,Y,N,N,N,Y,Y,N,N,N,Y)),
      (OPFFunct6.fmul,    Seq(N,Y,Y,N,N,N,N,N,N,Y,N,N,N,N,Y)),
      (OPFFunct6.fdiv,    Seq(N,Y,Y,N,N,N,N,N,N,N,N,N,Y,N,Y)),
      (OPFFunct6.frdiv,   Seq(N,Y,Y,N,Y,N,N,N,N,N,N,N,Y,N,Y)),
      (OPFFunct6.funary1, Seq(N,Y,N,N,N,N,N,N,N,N,N,N,N,Y,Y)),
      (OPFFunct6.fmacc,   Seq(N,Y,Y,Y,N,N,N,N,N,Y,N,N,N,N,Y)),
      (OPFFunct6.fnmacc,  Seq(N,Y,Y,Y,N,N,N,N,N,Y,Y,Y,N,N,Y)),
      (OPFFunct6.fmsac,   Seq(N,Y,Y,Y,N,N,N,N,N,Y,Y,N,N,N,Y)),
      (OPFFunct6.fnmsac,  Seq(N,Y,Y,Y,N,N,N,N,N,Y,N,Y,N,N,Y)),
      (OPFFunct6.fmadd,   Seq(N,Y,Y,Y,N,Y,N,N,N,Y,N,N,N,N,Y)),
      (OPFFunct6.fnmadd,  Seq(N,Y,Y,Y,N,Y,N,N,N,Y,Y,Y,N,N,Y)),
      (OPFFunct6.fmsub,   Seq(N,Y,Y,Y,N,Y,N,N,N,Y,Y,N,N,N,Y)),
      (OPFFunct6.fnmsub,  Seq(N,Y,Y,Y,N,Y,N,N,N,Y,N,Y,N,N,Y)),
      (OPFFunct6.fmin,   Seq(Y,Y,Y,N,N,N,N,N,Y,N,X,X,N,N,Y)),
      (OPFFunct6.fmax,   Seq(Y,Y,Y,N,N,N,N,N,Y,N,X,X,N,N,Y)),
  )
  
  val ctrl_cmp :: ren1 :: ren2 :: ren3 :: swap12 :: swap23 :: fromint :: toint :: fastpipe :: fma :: fmaCmd0 :: fmaCmd1 :: div :: sqrt :: wflags :: Nil = VecDecode.applyBools(
    io.iss.op.funct3, io.iss.op.funct6,
    Seq.fill(15)(X), ctrl_table)

  def accepts(f3: UInt, f6: UInt): Bool = VecDecode(f3, f6, ctrl_table.map(_._1))

  io.hazard.valid := valid
  io.hazard.bits.vat := op.vat
  io.hazard.bits.eg := op.wvd_eg

  val eidx = io.iss.op.eidx
  val in_eew = io.iss.op.rvs2_eew
  val out_eew = io.iss.op.vd_eew
  val is_double = io.iss.op.vd_eew === 3.U 

  val scalar_rs1 = io.iss.op.funct3.isOneOf(OPFVF)

  val req = Wire(new FPInput)
  val tag = io.iss.op.vd_eew === 3.U // if operating on 64b values, then tag with 1.U; otherwise tag with 0.U 
  req.ldst := false.B
  req.wen := false.B
  req.ren1 := ren1
  req.ren2 := ren2
  req.ren3 := ren3
  req.swap12 := swap12
  req.swap23 := swap23
  req.typeTagIn := 1.U
  req.typeTagOut := Mux(io.iss.op.vd_eew === 3.U, 1.U, 0.U)
  req.fromint := fromint
  req.toint := toint
  req.fastpipe := fastpipe
  req.fma := fma
  req.div := div
  req.sqrt := sqrt && (io.iss.op.rs1 === 0.U)
  req.wflags := wflags
  req.vec := true.B
  
  req.rm := Mux(ctrl_cmp, io.iss.op.opff6.isOneOf(OPFFunct6.fmax), io.iss.op.frm)
  req.fmaCmd := Cat(fmaCmd1, fmaCmd0) 
  req.typ := 0.U
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

  val rvs2_elem = Mux(is_double, d_rvs2, s_rvs2)
  val rvs1_elem = Mux(is_double, d_rvs1, s_rvs1)
  val rvd_elem = Mux(is_double, d_rvd, s_rvd)

  req.in1 := rvs1_elem
  req.in2 := rvs2_elem
  req.in3 := rvd_elem

  io.fp_req.bits := req
  io.fp_req.valid := io.iss.valid && io.iss.ready 

  io.fp_resp.ready := io.write.ready

  io.iss.ready := accepts(io.iss.op.funct3, io.iss.op.funct6) && (!valid || last) && io.fp_req.ready

  io.write.valid := io.fp_resp.fire()
  io.write.bits.eg := op.wvd_eg
  io.write.bits.mask := FillInterleaved(8, op.wmask)
  io.write.bits.data := Mux(is_double, FType.D.ieee(io.fp_resp.bits.data), Fill(2, FType.S.ieee(unbox(io.fp_resp.bits.data, 0.U, Some(FType.S))))) 

  last := io.write.fire()

  io.set_fflags := DontCare

  io.scalar_write.valid := false.B
  io.scalar_write.bits := DontCare
}
