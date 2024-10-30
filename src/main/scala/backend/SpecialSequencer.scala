package saturn.backend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import saturn.common._
import saturn.insns._

class SpecialSequencerIO(implicit p: Parameters) extends SequencerIO(new SpecialMicroOp) {
  val rvs2 = Decoupled(new VectorReadReq)
  val rvm  = Decoupled(new VectorReadReq)

  val acc_data = Decoupled(UInt(dLen.W))
  val acc_init = Output(UInt(dLen.W))
  val acc_init_resp = Input(UInt(dLen.W))
  val acc_fu_resp = Input(Valid(new VectorWrite(dLen)))
  val acc_done = Input(Bool())
}

class SpecialSequencer(exu_insns: Seq[VectorInstruction])(implicit p: Parameters) extends Sequencer[SpecialMicroOp]()(p) {
  def accepts(inst: VectorIssueInst) = {
    val needs_mask = inst.vmu && (!inst.vm && inst.mop =/= mopUnit)
    val needs_index = inst.vmu && inst.mop(0)
    val ctrl = new VectorDecoder(inst, exu_insns, Seq(UsesGatherUnit, Reduction))
    val gather = !inst.vmu && ctrl.bool(UsesGatherUnit)
    val reduction = !inst.vmu && ctrl.bool(Reduction)
    needs_mask || needs_index || gather || reduction
  }

  val io = IO(new SpecialSequencerIO)

  val valid = RegInit(false.B)
  val acc = Reg(Bool())
  val inst  = Reg(new BackendIssueInst)
  val eidx  = Reg(UInt(log2Ceil(maxVLMax).W))
  val rvs2_mask = Reg(UInt(egsTotal.W))
  val rvm_mask = Reg(UInt(egsPerVReg.W))
  val head = Reg(Bool())
  val slide_offset = Reg(UInt((1+log2Ceil(maxVLMax)).W))
  val acc_e0 = Reg(Bool())
  val acc_busy = Reg(Bool())
  val acc_reg = Reg(Vec(dLenB, UInt(8.W)))
  val acc_init_sel = Reg(Vec(6, Bool()))
  val acc_elementwise = Reg(Bool())


  val slide = !inst.vmu && inst.funct3 =/= OPIVV
  val slide_up = !inst.funct6(0)
  val rs2 = Mux(inst.rs1_is_rs2, inst.rs1, inst.rs2)
  val gatherei16 = inst.funct3 === OPIVV && inst.opif6 === OPIFunct6.rgatherei16
  val elementwise = !inst.vmu && !slide
  val vd_eew = inst.vconfig.vtype.vsew + inst.wide_vd

  val renvm = inst.renvm
  val renv2 = Mux(acc, acc_e0, inst.renv2)
  val incr_eew = Mux(inst.vmu, inst.mem_idx_size,
    Mux(gatherei16, 1.U, inst.vconfig.vtype.vsew))
  val eff_vl = Mux(slide,
    Mux(slide_up, inst.vconfig.vl - slide_offset, min(inst.vconfig.vtype.vlMax, inst.vconfig.vl + slide_offset)),
    inst.vconfig.vl
  )(log2Ceil(maxVLMax),0)
  val next_eidx = get_next_eidx(eff_vl, eidx, incr_eew, 0.U, false.B, elementwise)
  val tail = next_eidx === eff_vl

  io.dis.ready := !valid || (tail && io.iss.fire && !io.dis_stall && !acc)

  when (io.dis.fire) {
    val dis_inst = io.dis.bits
    val dis_ctrl = new VectorDecoder(dis_inst, exu_insns, Seq(Reduction, AccInitZeros, AccInitOnes, AccInitPos, AccInitNeg, Slide))
    val offset = Mux(dis_inst.isOpi,
      get_max_offset(Mux(dis_inst.funct3(2), dis_inst.rs1_data, dis_inst.imm5), dis_inst.vconfig.vtype.vsew),
      1.U)
    val slide = !dis_inst.vmu && dis_ctrl.bool(Slide)
    val slide_up = !dis_inst.funct6(0)
    val slide_start = Mux(slide_up, 0.U, offset)
    val vlmax = dis_inst.vconfig.vtype.vlMax
    val slide_no_read = Mux(slide_up,
      dis_inst.vconfig.vl <= offset,
      offset >= vlmax)
    val rs2 = Mux(dis_inst.rs1_is_rs2, dis_inst.rs1, dis_inst.rs2)
    val renv2_arch_mask = get_arch_mask(rs2, dis_inst.emul)

    valid := Mux(!slide, true.B, !slide_no_read)
    inst  := dis_inst
    eidx  := Mux(!slide, dis_inst.vstart, slide_start)
    slide_offset := offset

    rvs2_mask := Mux(dis_inst.renv2, FillInterleaved(egsPerVReg, renv2_arch_mask), 0.U)
    rvm_mask := Mux(dis_inst.renvm, ~(0.U(egsPerVReg.W)), 0.U)
    head := true.B

    val dis_vd_eew = dis_inst.vconfig.vtype.vsew + dis_inst.wide_vd
    val acc_init_fp_pos = dis_inst.opff6 === OPFFunct6.fredmin
    val acc_init_fp_neg = dis_inst.opff6 === OPFFunct6.fredmax

    acc := !dis_inst.vmu && dis_ctrl.bool(Reduction)
    acc_e0 := true.B
    acc_busy := false.B
    acc_reg := Mux1H(Seq(
      (dis_ctrl.bool(AccInitZeros) ,   0.U(dLen.W)),
      (dis_ctrl.bool(AccInitOnes)  , ~(0.U(dLen.W))),
      (dis_ctrl.bool(AccInitPos)   , VecInit.tabulate(4)({sew => Fill(dLenB >> sew, maxPosUInt(sew))})(dis_vd_eew)),
      (dis_ctrl.bool(AccInitNeg)   , VecInit.tabulate(4)({sew => Fill(dLenB >> sew, minNegUInt(sew))})(dis_vd_eew)),
      (acc_init_fp_pos, VecInit.tabulate(4)({sew => Fill(dLenB >> sew, maxPosFPUInt(sew))})(dis_vd_eew)),
      (acc_init_fp_neg, VecInit.tabulate(4)({sew => Fill(dLenB >> sew, minNegFPUInt(sew))})(dis_vd_eew))
    )).asTypeOf(Vec(dLenB, UInt(8.W)))

    acc_init_sel(0) := dis_ctrl.bool(AccInitZeros)
    acc_init_sel(1) := dis_ctrl.bool(AccInitOnes)
    acc_init_sel(2) := dis_ctrl.bool(AccInitPos)
    acc_init_sel(3) := dis_ctrl.bool(AccInitNeg)
    acc_init_sel(4) := acc_init_fp_pos
    acc_init_sel(5) := acc_init_fp_neg
  } .elsewhen (io.iss.fire) {
    valid := !tail
    head := false.B
  }

  io.vat := inst.vat
  io.seq_hazard.valid := valid && (!acc || acc_e0)
  io.seq_hazard.bits.rintent := hazardMultiply(Mux(acc, get_arch_mask(inst.rs1, 0.U), rvs2_mask | rvm_mask))
  io.seq_hazard.bits.wintent := false.B
  io.seq_hazard.bits.vat := inst.vat

  val vs2_read_oh = Mux(renv2, UIntToOH(io.rvs2.bits.eg), 0.U)
  val vm_read_oh  = Mux(renvm, UIntToOH(io.rvm.bits.eg), 0.U)

  val raw_hazard = ((vm_read_oh | vs2_read_oh) & io.older_writes) =/= 0.U
  val data_hazard = raw_hazard

  val oldest = inst.vat === io.vat_head

  io.rvs2.valid := valid && renv2
  io.rvs2.bits.eg := Mux(acc,
    getEgId(inst.rs1, 0.U, vd_eew, false.B),
    getEgId(rs2, eidx, incr_eew, false.B)
  )
  io.rvs2.bits.oldest := oldest

  io.rvm.valid := valid && renvm
  io.rvm.bits.eg := getEgId(0.U, eidx, 0.U, true.B)
  io.rvm.bits.oldest := oldest

  io.busy := valid
  io.head := Mux(acc, acc_e0, head)

  io.iss.valid := valid && !data_hazard && (!renvm || io.rvm.ready) && (!renv2 || io.rvs2.ready) && !acc
  io.iss.bits.renv2     := renv2
  io.iss.bits.renvm     := renvm
  io.iss.bits.rvs2_eew  := incr_eew
  io.iss.bits.sew       := inst.vconfig.vtype.vsew
  io.iss.bits.eidx      := eidx
  io.iss.bits.vl        := eff_vl
  io.iss.bits.vmu       := inst.vmu
  io.iss.bits.tail      := tail
  io.iss.bits.slide     := slide

  io.acc_init := Mux1H(acc_init_sel, Seq(
    0.U(dLen.W),
    ~(0.U(dLen.W)),
    VecInit.tabulate(4)({sew => Fill(dLenB >> sew, maxPosUInt(sew))})(vd_eew),
    VecInit.tabulate(4)({sew => Fill(dLenB >> sew, minNegUInt(sew))})(vd_eew),
    VecInit.tabulate(4)({sew => Fill(dLenB >> sew, maxPosFPUInt(sew))})(vd_eew),
    VecInit.tabulate(4)({sew => Fill(dLenB >> sew, minNegFPUInt(sew))})(vd_eew)
  ))


  when (io.rvs2.fire && !raw_hazard) {
    val v0_mask = eewByteMask(vd_eew)
    val init_resp = io.acc_init_resp.asTypeOf(Vec(dLenB, UInt(8.W)))
    for (i <- 0 until 8) {
      when (v0_mask(i)) {
        acc_reg(i) := init_resp(i)
      }
    }
    acc_e0 := false.B
  }

  io.acc_data.valid := valid && acc && !acc_e0 && !acc_busy
  io.acc_data.bits := acc_reg.asUInt

  when (io.acc_data.fire) {
    acc_busy := true.B
  }

  when (io.acc_done) {
    valid := false.B
  }

  when (io.acc_fu_resp.valid) {
    acc_busy := false.B
    for (i <- 0 until dLenB) when (io.acc_fu_resp.bits.mask(i*8)) { acc_reg(i) := io.acc_fu_resp.bits.data >> (i*8) }
  }

  when (io.iss.fire && !tail) {
    when (next_is_new_eg(eidx, next_eidx, incr_eew, false.B) && vParams.enableChaining.B) {
      rvs2_mask := rvs2_mask & ~vs2_read_oh
    }
    when (next_is_new_eg(eidx, next_eidx, 0.U, true.B) && vParams.enableChaining.B) {
      rvm_mask := rvm_mask & ~UIntToOH(io.rvm.bits.eg)
    }
    eidx := next_eidx
  }


}
