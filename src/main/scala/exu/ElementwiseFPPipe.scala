package vector.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import vector.common._

class ElementwiseFPUIO(implicit p: Parameters) extends IterativeFunctionalUnitIO {
  val fp_req = Decoupled(new FPInput()) 
  val fp_resp = Flipped(Decoupled(new FPResult()))
}

class ElementwiseFPU(implicit p: Parameters) extends IterativeFunctionalUnit()(p) with HasFPUParameters {
  val io = IO(new ElementwiseFPUIO)

  val valid = RegInit(false.B)
  val op = Reg(new VectorMicroOp)
  val last = Wire(Bool())

  io.vat.valid := valid && op.last
  io.vat.bits  := op.vat
  io.busy := valid

  when (io.iss.valid && io.iss.ready) {
    valid := true.B
    op := io.iss.op
  } .elsewhen (last) {
    valid := false.B
  }

  io.iss.sub_dlen := log2Ceil(dLenB).U - io.iss.op.rvd_eew
  io.set_vxsat := false.B

  lazy val ctrl_table = Seq(
      (OPFFunct6.fadd,   Seq(N,N,N,N,N,N,N,N,N,N,N,Y)),
      (OPFFunct6.fsub,   Seq(N,N,N,N,N,N,N,Y,N,N,N,Y)),
      (OPFFunct6.fmacc,  Seq(N,N,N,N,N,N,Y,N,N,N,N,Y)),
      (OPFFunct6.fnmacc, Seq(N,N,N,N,N,N,Y,Y,Y,N,N,Y)),
      (OPFFunct6.fmsac,  Seq(N,N,N,N,N,N,Y,Y,N,N,N,Y)),
      (OPFFunct6.fnmsac, Seq(N,N,N,N,N,N,Y,N,Y,N,N,Y)),
  )
  
  val wen :: swap12 :: swap23 :: fromint :: toint :: fastpipe :: fma :: fmaCmd0 :: fmaCmd1 :: div :: sqrt :: wflags :: Nil = VecDecode.applyBools(
    io.iss.op.funct3, io.iss.op.funct6,
    Seq.fill(12)(X), ctrl_table)

  override def accepts(f3: UInt, f6: UInt): Bool = VecDecode(f3, f6, ctrl_table.map(_._1))

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
  req.wen := wen
  req.ren1 := false.B
  req.ren2 := false.B
  req.ren3 := fma // needs to be true for the FPUFMAPipe to work
  req.swap12 := swap12
  req.swap23 := swap23
  req.typeTagIn := 1.U
  req.typeTagOut := Mux(io.iss.op.vd_eew === 3.U, 1.U, 0.U)
  req.fromint := fromint
  req.toint := toint
  req.fastpipe := fastpipe
  req.fma := fma
  req.div := div
  req.sqrt := sqrt
  req.wflags := wflags
  
  req.rm := io.iss.op.frm
  req.fmaCmd := Cat(fmaCmd1, fmaCmd0) 
  req.typ := 0.U
  req.fmt := 0.U

  val rvs2_extract = extract(io.iss.op.rvs2_data, false.B, in_eew, eidx)(63, 0)
  val rvs1_extract = extract(io.iss.op.rvs1_data, false.B, in_eew, eidx)(63, 0) 
  val rvd_extract = extract(io.iss.op.rvd_data, false.B, out_eew, eidx)(63, 0) 

  val s_rvs2 = FType.S.recode(rvs2_extract(31,0))
  val s_rvs1 = Mux(scalar_rs1, FType.S.recode(io.iss.op.frs1_data(31,0)), FType.S.recode(rvs1_extract(31,0)))
  val s_rvd = FType.S.recode(rvd_extract(31,0))

  val d_rvs2 = FType.D.recode(rvs2_extract)
  val d_rvs1 = Mux(scalar_rs1, FType.S.recode(io.iss.op.frs1_data), FType.D.recode(rvs1_extract))
  val d_rvd = FType.D.recode(rvd_extract)

  val rvs2_elem = Mux(is_double, d_rvs2, s_rvs2)
  val rvs1_elem = Mux(is_double, d_rvs1, s_rvs1)
  val rvd_elem = Mux(is_double, d_rvd, s_rvd)

  req.in1 := rvs2_elem
  req.in2 := rvs1_elem
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
}
