package vector.backend

import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import vector.common._
import vector.insns._

class ExecuteSequencer(supported_insns: Seq[VectorInstruction])(implicit p: Parameters) extends PipeSequencer(new ExecuteMicroOp)(p) {
  def accepts(inst: VectorIssueInst) = !inst.vmu

  val valid = RegInit(false.B)
  val inst  = Reg(new BackendIssueInst)
  val head  = Reg(Bool())
  val wvd_mask  = Reg(UInt(egsTotal.W))
  val rvs1_mask = Reg(UInt(egsTotal.W))
  val rvs2_mask = Reg(UInt(egsTotal.W))
  val rvd_mask  = Reg(UInt(egsTotal.W))
  val rvm_mask  = Reg(UInt(egsPerVReg.W))

  val acc       = Reg(Vec(dLenB, UInt(8.W)))
  val acc_ready = Reg(Bool())
  val acc_tail  = Reg(Bool())
  val acc_tail_id = Reg(UInt(log2Ceil(dLenB).W))

  val vs1_eew  = inst.vconfig.vtype.vsew
  val vs2_eew  = inst.vconfig.vtype.vsew + inst.wide_vs2 - Mux(inst.opmf6 === OPMFunct6.xunary0,
    ~inst.rs1(2,1) + 1.U, 0.U)
  val vs3_eew  = inst.vconfig.vtype.vsew + inst.wide_vd
  val vd_eew   = inst.vconfig.vtype.vsew + inst.wide_vd
  val incr_eew = Seq(
    Mux(inst.renv1, vs1_eew, 0.U),
    Mux(inst.renv2, vs2_eew, 0.U),
    Mux(inst.renvd, vs3_eew, 0.U),
    vd_eew).foldLeft(0.U(2.W)) { case (b, a) => Mux(a > b, a, b) }
  val acc_copy = (vd_eew === 3.U && (dLenB == 8).B) || inst.opff6.isOneOf(OPFFunct6.fredosum, OPFFunct6.fwredosum)
  val acc_last = acc_tail_id + 1.U === log2Ceil(dLenB).U - vd_eew || acc_copy
  val slide    = inst.funct6.isOneOf(OPIFunct6.slideup.litValue.U, OPIFunct6.slidedown.litValue.U)
  val uscalar  = Mux(inst.funct3(2), inst.rs1_data, inst.imm5)
  val sscalar  = Mux(inst.funct3(2), inst.rs1_data, inst.imm5_sext)
  val rgather    = inst.opif6 === OPIFunct6.rgather
  val rgather_ix = rgather && inst.funct3.isOneOf(OPIVX, OPIVI)
  val rgather_v  = rgather && inst.funct3.isOneOf(OPIVV)
  val slide_offset = Mux(inst.isOpi, get_max_offset(uscalar), 1.U)
  val slide_up = !inst.funct6(0)
  val renv1    = Mux(inst.reduction, head, inst.renv1)
  val renv2    = Mux(rgather_ix, head, Mux(inst.reduction, !head && !acc_tail, inst.renv2))
  val renvd    = inst.renvd
  val renvm    = inst.renvm
  val renacc   = inst.reduction
  val ctrl     = new VectorDecoder(inst.funct3, inst.funct6, inst.rs1, inst.rs2, supported_insns,
    Seq(SetsWMask, UsesPermuteSeq))

  val use_wmask = !inst.vm && ctrl.bool(SetsWMask)
  val eidx      = Reg(UInt(log2Ceil(maxVLMax).W))
  val eff_vl    = Mux(inst.scalar_to_vd0, 1.U, inst.vconfig.vl)
  val next_eidx = get_next_eidx(eff_vl, eidx, incr_eew, io.sub_dlen, inst.reads_mask && (inst.writes_mask || !inst.wvd))
  val eidx_tail = next_eidx === eff_vl
  val tail      = Mux(inst.reduction, acc_tail && acc_last, eidx_tail)

  io.dis.ready := !valid || (tail && io.iss.fire)

  when (io.dis.fire) {
    val iss_inst = io.dis.bits
    valid := true.B
    inst := io.dis.bits
    assert(iss_inst.vstart === 0.U)
    eidx := 0.U

    val vd_arch_mask  = get_arch_mask(iss_inst.rd , iss_inst.pos_lmul +& iss_inst.wide_vd                                , 4)
    val vs1_arch_mask = get_arch_mask(iss_inst.rs1, Mux(iss_inst.reads_mask, 0.U, iss_inst.pos_lmul                     ), 3)
    val vs2_arch_mask = get_arch_mask(iss_inst.rs2, Mux(iss_inst.reads_mask, 0.U, iss_inst.pos_lmul +& iss_inst.wide_vs2), 4)

    wvd_mask    := Mux(iss_inst.wvd  , FillInterleaved(egsPerVReg, vd_arch_mask), 0.U)
    rvs1_mask   := Mux(iss_inst.renv1, FillInterleaved(egsPerVReg, vs1_arch_mask), 0.U)
    rvs2_mask   := Mux(iss_inst.renv2, FillInterleaved(egsPerVReg, vs2_arch_mask), 0.U)
    rvd_mask    := Mux(iss_inst.renvd, FillInterleaved(egsPerVReg, vd_arch_mask), 0.U)
    rvm_mask    := Mux(iss_inst.renvm, ~(0.U(egsPerVReg.W)), 0.U)
    head        := true.B
    acc_tail    := false.B
    acc_tail_id := 0.U
    acc_ready   := true.B
  } .elsewhen (io.iss.fire) {
    valid := !tail
    head := false.B
  }

  when (io.acc.valid) {
    acc_ready := true.B
    for (i <- 0 until dLenB) when (io.acc.bits.mask(i*8)) { acc(i) := io.acc.bits.data >> (i*8) }
  }

  io.vat := inst.vat
  io.seq_hazard.valid := valid
  io.seq_hazard.bits.rintent := rvs1_mask | rvs2_mask | rvd_mask | rvm_mask
  io.seq_hazard.bits.wintent := wvd_mask
  io.seq_hazard.bits.vat := inst.vat

  val vs1_read_oh = Mux(renv1   , UIntToOH(io.rvs1.req.bits), 0.U)
  val vs2_read_oh = Mux(renv2   , UIntToOH(io.rvs2.req.bits), 0.U)
  val vd_read_oh  = Mux(renvd   , UIntToOH(io.rvd.req.bits ), 0.U)
  val vm_read_oh  = Mux(renvm   , UIntToOH(io.rvm.req.bits ), 0.U)
  val vd_write_oh = Mux(inst.wvd, UIntToOH(io.iss.bits.wvd_eg), 0.U)

  val raw_hazard = ((vs1_read_oh | vs2_read_oh | vd_read_oh | vm_read_oh) & io.older_writes) =/= 0.U
  val waw_hazard = (vd_write_oh & io.older_writes) =/= 0.U
  val war_hazard = (vd_write_oh & io.older_reads) =/= 0.U
  val data_hazard = raw_hazard || waw_hazard || war_hazard

  val acc_insns = supported_insns.filter(_.props.contains(Reduction.Y))
  val acc_ctrl = new VectorDecoder(inst.funct3, inst.funct6, inst.rs1, inst.rs2, acc_insns, Seq(AccInitZeros, AccInitOnes, AccInitPos, AccInitNeg))
  val acc_init_fp_pos = inst.opff6 === OPFFunct6.fredmin
  val acc_init_fp_neg = inst.opff6 === OPFFunct6.fredmax

  val acc_init = Mux1H(Seq(
    (acc_ctrl.bool(AccInitZeros) ,   0.U(dLen.W)),
    (acc_ctrl.bool(AccInitOnes)  , ~(0.U(dLen.W))),
    (acc_ctrl.bool(AccInitPos)   , VecInit.tabulate(4)({sew => Fill(dLenB >> sew, maxPosUInt(sew))})(vd_eew)),
    (acc_ctrl.bool(AccInitNeg)   , VecInit.tabulate(4)({sew => Fill(dLenB >> sew, minNegUInt(sew))})(vd_eew)),
    (acc_init_fp_pos, VecInit.tabulate(4)({sew => Fill(dLenB >> sew, maxPosFPUInt(sew))})(vd_eew)),
    (acc_init_fp_neg, VecInit.tabulate(4)({sew => Fill(dLenB >> sew, minNegFPUInt(sew))})(vd_eew)),
  ))

  val rgather_eidx = get_max_offset(Mux(rgather_ix, uscalar, io.perm.data & eewBitMask(vs2_eew)))
  val rgather_zero = rgather_eidx >= inst.vconfig.vtype.vlMax
  val rvs2_eidx = Mux(rgather, rgather_eidx, eidx)
  io.rvs1.req.bits := getEgId(inst.rs1, eidx     , vs1_eew, inst.reads_mask)
  io.rvs2.req.bits := getEgId(inst.rs2, rvs2_eidx, vs2_eew, inst.reads_mask)
  io.rvd.req.bits  := getEgId(inst.rd , eidx     , vs3_eew, inst.reads_mask)
  io.rvm.req.bits  := getEgId(0.U     , eidx     , 0.U    , true.B)

  io.rvs1.req.valid := valid && renv1
  io.rvs2.req.valid := valid && renv2
  io.rvd.req.valid  := valid && renvd
  io.rvm.req.valid  := valid && renvm

  val read_perm_buffer = ctrl.bool(UsesPermuteSeq) && (!slide || Mux(slide_up,
    next_eidx > slide_offset,
    eidx +& slide_offset < inst.vconfig.vtype.vlMax))
  io.perm.req.bits.head := Mux(slide,
    Mux(slide_up,
      Mux(eidx < slide_offset, (slide_offset << vs2_eew)(dLenOffBits-1,0), 0.U),
      eidx << vs2_eew),
    0.U)
  io.perm.req.bits.tail := Mux(slide,
    Mux(slide_up,
      Mux(tail, eff_vl << vs2_eew, 0.U),
      Mux(next_eidx + slide_offset < inst.vconfig.vtype.vlMax, 0.U, ((next_eidx - slide_offset) << vs2_eew)(dLenOffBits-1,0))),
    1.U << vs1_eew)
  val slide_down_byte_mask = Mux(slide && !slide_up && next_eidx + slide_offset > inst.vconfig.vtype.vlMax,
    Mux(eidx +& slide_offset >= inst.vconfig.vtype.vlMax,
      0.U,
      ~(0.U(dLenB.W)) >> (0.U(dLenOffBits.W) - ((inst.vconfig.vtype.vlMax - slide_offset) << vs2_eew))(dLenOffBits-1,0)),
    ~(0.U(dLenB.W)))
  val slide_down_bit_mask = FillInterleaved(8, slide_down_byte_mask)
  val iss_valid = (valid &&
    !data_hazard &&
    !(renv1 && !io.rvs1.req.ready) &&
    !(renv2 && !io.rvs2.req.ready) &&
    !(renvd && !io.rvd.req.ready) &&
    !(renvm && !io.rvm.req.ready) &&
    !(read_perm_buffer && !io.perm.req.ready) &&
    !(renacc && !acc_ready && !io.acc.valid)
  )
  io.perm.req.valid := iss_valid && read_perm_buffer
  io.iss.valid := iss_valid && !(inst.reduction && head)

  io.iss.bits.rvs1_data := io.rvs1.resp
  io.iss.bits.rvs2_data := io.rvs2.resp
  io.iss.bits.rvd_data  := io.rvd.resp
  io.iss.bits.rvs1_eew  := vs1_eew
  io.iss.bits.rvs2_eew  := vs2_eew
  io.iss.bits.rvd_eew   := vs3_eew
  io.iss.bits.vd_eew    := vd_eew
  io.iss.bits.eidx      := eidx
  io.iss.bits.vl        := inst.vconfig.vl
  io.iss.bits.wvd_eg    := getEgId(inst.rd, Mux(inst.reduction, 0.U, eidx), vd_eew, inst.writes_mask)
  io.iss.bits.rs1       := inst.rs1
  io.iss.bits.rs2       := inst.rs2
  io.iss.bits.rd        := inst.rd
  io.iss.bits.funct3    := inst.funct3
  io.iss.bits.funct6    := inst.funct6
  io.iss.bits.tail      := tail
  io.iss.bits.head      := head
  io.iss.bits.acc       := inst.reduction
  io.iss.bits.vat       := inst.vat
  io.iss.bits.vm        := inst.vm
  io.iss.bits.rm        := inst.rm

  val dlen_mask = ~(0.U(dLenB.W))
  val head_mask = dlen_mask << (eidx << vd_eew)(dLenOffBits-1,0)
  val tail_mask = dlen_mask >> (0.U(dLenOffBits.W) - (next_eidx << vd_eew)(dLenOffBits-1,0))
  val slide1up_mask = Mux(head, eewByteMask(vs2_eew), 0.U)
  val slideup_mask = Mux(slide && slide_up && eidx < slide_offset,
    Mux(next_eidx <= slide_offset, 0.U, dlen_mask << (slide_offset << vd_eew)(dLenOffBits-1,0)) | slide1up_mask,
    dlen_mask)
  val full_tail_mask = Mux(tail,
    ~(0.U(dLen.W)) >> (0.U(log2Ceil(dLen).W) - eff_vl(log2Ceil(dLen)-1,0)),
    ~(0.U(dLen.W))
  )
  val vm_off    = ((1 << dLenOffBits) - 1).U(log2Ceil(dLen).W)
  val vm_eidx   = (eidx & ~(vm_off >> vd_eew))(log2Ceil(dLen)-1,0)
  val vm_resp   = (io.rvm.resp >> vm_eidx)(dLenB-1,0)
  val vm_mask   = Mux(use_wmask,
    VecInit.tabulate(4)({ sew => FillInterleaved(1 << sew, vm_resp)(dLenB-1,0) })(vd_eew),
    ~(0.U(dLenB.W))
  )
  val acc_mask  = Mux(acc_last,
    eewByteMask(vd_eew),
    VecInit.tabulate(log2Ceil(dLenB))(i => ~(0.U((dLen>>i).W)))(acc_tail_id))
  io.iss.bits.wmask := Mux(inst.reduction && acc_tail,
    acc_mask,
    head_mask & tail_mask & vm_mask & slideup_mask)

  io.iss.bits.rmask := Mux(inst.vm, ~(0.U(dLenB.W)), vm_resp)
  io.iss.bits.rvm_data := Mux(inst.vm, ~(0.U(dLen.W)), io.rvm.resp)
  io.iss.bits.full_tail_mask := full_tail_mask

  when (inst.funct3.isOneOf(OPIVI, OPIVX, OPMVX, OPFVF)) {
    io.iss.bits.rvs1_data := dLenSplat(sscalar, vs1_eew)
  }
  when (inst.reduction) {
    val bypass_mask = Mux(io.acc.valid, io.acc.bits.mask, 0.U)
    val acc_bypass = (bypass_mask & io.acc.bits.data) | (~bypass_mask & acc.asUInt)
    when (inst.opff6.isOneOf(OPFFunct6.fredosum, OPFFunct6.fwredosum) && !acc_tail) {
      io.iss.bits.rvs2_data := VecInit.tabulate(4)({sew =>
        if (sew == 3 && dLenOffBits == 3) { io.rvs2.resp } else {
          io.rvs2.resp.asTypeOf(Vec(dLenB >> sew, UInt((8 << sew).W)))(eidx(dLenOffBits-sew-1,0))
        }
      })(vd_eew)
      val mask_bit = Mux(use_wmask, (io.rvm.resp >> eidx(log2Ceil(dLen)-1,0))(0), true.B)
      io.iss.bits.wmask := VecInit.tabulate(4)({sew => Fill(1 << sew, mask_bit)})(vd_eew)
    }
    when (acc_tail) {
      val folded = VecInit.tabulate(log2Ceil(dLenB))(i => {
        val start = dLen >> (1 + i)
        acc_bypass(2*start-1,start)
      })(acc_tail_id)
      io.iss.bits.rvs1_data := Mux(acc_copy, acc_init, folded)
      io.iss.bits.rvs1_eew := vd_eew
      io.iss.bits.rvs2_data := acc_bypass
      io.iss.bits.rvs2_eew  := vd_eew
    } .elsewhen (head) {
      io.iss.bits.rvs1_eew := vs1_eew
      io.iss.bits.rvs2_data := acc_init
    } .otherwise {
      io.iss.bits.rvs1_data := acc_bypass
      io.iss.bits.rvs1_eew := vd_eew
    }
  }
  when (rgather_v) {
    io.iss.bits.rvs1_data := rgather_eidx
  }
  when (rgather_zero) {
    io.iss.bits.rvs2_data := 0.U
  }
  when (slide) {
    io.iss.bits.rvs2_data := io.perm.data & slide_down_bit_mask
  }

  when (iss_valid && inst.reduction && head) {
    val v0_mask = eewBitMask(vd_eew)
    acc := ((acc_init & ~v0_mask.pad(dLen)) | (io.rvs1.resp & v0_mask)).asTypeOf(Vec(dLenB, UInt(8.W)))
    head := false.B
  }

  when (io.iss.fire && !tail) {
    when (next_is_new_eg(eidx, next_eidx, vd_eew, inst.writes_mask) && !(inst.reduction && !acc_tail)) {
      val wvd_clr_mask = UIntToOH(io.iss.bits.wvd_eg)
      wvd_mask  := wvd_mask  & ~wvd_clr_mask
    }
    when (next_is_new_eg(eidx, next_eidx, vs2_eew, inst.reads_mask) && !(inst.reduction && head) && !rgather_v) {
      rvs2_mask := rvs2_mask & ~UIntToOH(io.rvs2.req.bits)
    }
    when (rgather_ix) {
      rvs2_mask := 0.U
    }
    when (next_is_new_eg(eidx, next_eidx, vs1_eew, inst.reads_mask)) {
      rvs1_mask := rvs1_mask & ~UIntToOH(io.rvs1.req.bits)
    }
    when (next_is_new_eg(eidx, next_eidx, vs3_eew, false.B)) {
      rvd_mask  := rvd_mask  & ~UIntToOH(io.rvd.req.bits)
    }
    when (next_is_new_eg(eidx, next_eidx, 0.U    , true.B)) {
      rvm_mask  := rvm_mask  & ~UIntToOH(io.rvm.req.bits)
    }
    acc_ready := false.B
    when (eidx_tail) { acc_tail := true.B }
    when (acc_tail) { acc_tail_id := acc_tail_id + 1.U }
    eidx := next_eidx
  }

  io.busy := valid
}
