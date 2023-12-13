package vector.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import vector.common._

class VFDivSqrt(implicit p: Parameters) extends IterativeFunctionalUnit()(p) with HasFPUParameters {
  io.iss.sub_dlen := log2Ceil(dLenB).U - io.iss.op.rvs1_eew
  io.set_vxsat := false.B

  val divSqrt = Module(new hardfloat.DivSqrtRecF64)

  lazy val ctrl_table = Seq(
    (OPFFunct6.vfdiv     ,   Seq(Y,N)),
    (OPFFunct6.vfrdiv    ,   Seq(Y,Y)),
    (OPFFunct6.vfunary1  ,   Seq(N,X)),
  )
  val ctrl_isDiv :: ctrl_swap12 :: Nil = VecDecode.applyBools(
    io.iss.op.funct3, io.iss.op.funct6,
    Seq.fill(2)(X), ctrl_table
  ) 
  override def accepts(f3: UInt, f6: UInt): Bool = VecDecode(f3, f6, ctrl_table.map(_._1))

  val divSqrt_ready = (ctrl_isDiv && divSqrt.io.inReady_div) || (!ctrl_isDiv && divSqrt.io.inReady_sqrt)

  val is_opfvf = io.iss.op.funct3.isOneOf(OPFVF)
  //val scalar_operand_bits = Mux(io.iss.op.vd_eew === 3.U, io.iss.op.frs1_data)
  val scalar_operand_bits = Mux(io.iss.op.rvd_eew === 3.U, io.iss.op.frs1_data, Fill(2, io.iss.op.frs1_data(31,0)))

  val rvs2_bits = extract(io.iss.op.rvs2_data, false.B, io.iss.op.rvs2_eew, io.iss.op.eidx)(63,0)
  //val rvs1_bits = extract(io.iss.op.rvs1_data, false.B, io.iss.op.rvs1_eew, io.iss.op.eidx)(63,0)
  //val rvs1_bits = Mux(is_opfvf, io.iss.op.frs1_data, extract(io.iss.op.rvs1_data, false.B, io.iss.op.rvs1_eew, io.iss.op.eidx)(63,0))
  val rvs1_bits = Mux(is_opfvf, scalar_operand_bits, extract(io.iss.op.rvs1_data, false.B, io.iss.op.rvs1_eew, io.iss.op.eidx)(63,0))

  divSqrt.io.detectTininess := hardfloat.consts.tininess_afterRounding
  divSqrt.io.roundingMode := io.iss.op.frm

  divSqrt.io.inValid := io.iss.valid && io.iss.ready
  divSqrt.io.sqrtOp := !ctrl_isDiv

  io.hazard.valid := valid
  io.hazard.bits.vat := op.vat
  io.hazard.bits.eg := op.wvd_eg
  io.hazard.bits.widen2 := false.B

  when (io.iss.op.rvs1_eew === 3.U) {
    divSqrt.io.a := Mux(ctrl_swap12, FType.D.recode(rvs1_bits), FType.D.recode(rvs2_bits))
    divSqrt.io.b := Mux(ctrl_swap12, FType.D.recode(rvs2_bits), FType.D.recode(rvs1_bits))
  } .otherwise {
    val narrow_rvs2_bits = rvs2_bits(31,0)
    //val narrow_rvs1_bits = Mux(is_opfvf, io.iss.op.frs1_data(63,32), rvs1_bits(31,0))
    val narrow_rvs1_bits = rvs1_bits(31,0)
    val widen = Seq(FType.S.recode(narrow_rvs2_bits), FType.S.recode(narrow_rvs1_bits)).zip(
      Seq.fill(2)(Module(new hardfloat.RecFNToRecFN(8, 24, 11, 53)))).map { case(input, upconvert) =>
      upconvert.io.in := input
      upconvert.io.roundingMode := io.iss.op.frm
      upconvert.io.detectTininess := hardfloat.consts.tininess_afterRounding
      upconvert
    }

    divSqrt.io.a := Mux(ctrl_swap12, widen(1).io.out, widen(0).io.out) 
    divSqrt.io.b := Mux(ctrl_swap12, widen(0).io.out, widen(1).io.out)
  }

  val divSqrt_valid = divSqrt.io.outValid_div || divSqrt.io.outValid_sqrt

  val narrow = Module(new hardfloat.RecFNToRecFN(11, 53, 8, 24))
  narrow.io.roundingMode := op.frm
  narrow.io.detectTininess := hardfloat.consts.tininess_afterRounding
  narrow.io.in := divSqrt.io.out

  val divSqrt_out = Mux(op.vd_eew === 3.U, FType.D.ieee(divSqrt.io.out), Fill(2, FType.S.ieee(narrow.io.out)))

  val out_buffer = RegEnable(divSqrt_out, divSqrt_valid)
  val out_toWrite = RegInit(false.B)

  when (io.write.fire()) {
    out_toWrite := false.B
  } .elsewhen (divSqrt_valid) {
    out_toWrite := true.B
  }

  io.write.valid := out_toWrite || divSqrt_valid 
  io.write.bits.eg := op.wvd_eg
  io.write.bits.mask := FillInterleaved(8, op.wmask)
  io.write.bits.data := Mux(out_toWrite, out_buffer, divSqrt_out)
  
  io.iss.ready := accepts(io.iss.op.funct3, io.iss.op.funct6) && divSqrt_ready && (!valid || last) 
  last := io.write.fire()
}
