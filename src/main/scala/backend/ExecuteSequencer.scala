package saturn.backend

import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import saturn.common._
import saturn.insns._

class ExecuteSequencer(supported_insns: Seq[VectorInstruction])(implicit p: Parameters) extends PipeSequencer(new ExecuteMicroOp)(p) {
  def accepts(inst: VectorIssueInst) = !inst.vmu

  val valid = RegInit(false.B)
  val inst  = Reg(new BackendIssueInst)
  val head  = Reg(Bool())
  val reduction_head = Reg(Bool())
  val wvd_mask  = Reg(UInt(egsTotal.W))
  val rvs1_mask = Reg(UInt(egsTotal.W))
  val rvs2_mask = Reg(UInt(egsTotal.W))
  val rvd_mask  = Reg(UInt(egsTotal.W))
  val rvm_mask  = Reg(UInt(egsPerVReg.W))
  val slide     = Reg(Bool())
  val slide_up  = Reg(Bool())
  val slide1    = Reg(Bool())
  val slide_offset = Reg(UInt((1+log2Ceil(maxVLMax)).W))
  val perm_head = Reg(UInt(dLenOffBits.W))
  val perm_tail = Reg(UInt(dLenOffBits.W))

  val acc       = Reg(Vec(dLenB, UInt(8.W)))
  val acc_ready = Reg(Bool())
  val acc_tail  = Reg(Bool())
  val acc_tail_id = Reg(UInt(log2Ceil(dLenB).W))

  val mvnrr    = inst.funct3 === OPIVI && inst.opif6 === OPIFunct6.mvnrr
  val rgatherei16 = inst.funct3 === OPIVV && inst.opif6 === OPIFunct6.rgatherei16
  val compress = inst.opmf6 === OPMFunct6.compress
  val vs1_eew  = Mux(rgatherei16, 1.U, inst.vconfig.vtype.vsew)
  val vs2_eew  = inst.vconfig.vtype.vsew + inst.wide_vs2 - Mux(inst.opmf6 === OPMFunct6.xunary0,
    ~inst.rs1(2,1) + 1.U, 0.U)
  val vs3_eew  = inst.vconfig.vtype.vsew + inst.wide_vd
  val vd_eew   = inst.vconfig.vtype.vsew + inst.wide_vd
  val incr_eew = Seq(
    Mux(inst.renv1, vs1_eew, 0.U),
    Mux(inst.renv2, vs2_eew, 0.U),
    Mux(inst.renvd, vs3_eew, 0.U),
    vd_eew).foldLeft(0.U(2.W)) { case (b, a) => Mux(a > b, a, b) }
  val acc_elementwise_opcodes = (Seq(OPFFunct6.fredosum, OPFFunct6.fwredosum) ++
    (if (vParams.useScalarFPMisc) Seq(OPFFunct6.fredmax, OPFFunct6.fredmin) else Nil) ++
    (if (vParams.useScalarFPFMA) Seq(OPFFunct6.fredusum, OPFFunct6.fwredusum) else Nil)
  )
  val acc_copy = (vd_eew === 3.U && (dLenB == 8).B) || inst.opff6.isOneOf(acc_elementwise_opcodes)
  val acc_last = acc_tail_id + 1.U === log2Ceil(dLenB).U - vd_eew || acc_copy
  val uscalar  = Mux(inst.funct3(2), inst.rs1_data, inst.imm5)
  val sscalar  = Mux(inst.funct3(2), inst.rs1_data, inst.imm5_sext)
  val rgather    = inst.opif6 === OPIFunct6.rgather
  val rgather_ix = rgather && inst.funct3.isOneOf(OPIVX, OPIVI)
  val rgather_v  = rgather && inst.funct3.isOneOf(OPIVV)
  val renv1    = Mux(inst.reduction, reduction_head, inst.renv1)
  val renv2    = Mux(rgather_ix, head, Mux(inst.reduction, !reduction_head && !acc_tail, inst.renv2))
  val renvd    = inst.renvd
  val renvm    = inst.renvm
  val renacc   = inst.reduction
  val ctrl     = new VectorDecoder(inst.funct3, inst.funct6, inst.rs1, inst.rs2, supported_insns,
    Seq(SetsWMask, UsesPermuteSeq, FPAdd, FPComp, Elementwise))

  val use_wmask = !inst.vm && ctrl.bool(SetsWMask)
  val eidx      = Reg(UInt(log2Ceil(maxVLMax).W))
  val eff_vl    = Mux(mvnrr, ((vLen/8).U >> vd_eew) << inst.emul, Mux(inst.scalar_to_vd0, 1.U, inst.vconfig.vl))
  val increments_as_mask = (!inst.renv1 || inst.reads_vs1_mask) && (!inst.renv2 || inst.reads_vs2_mask) && (!inst.wvd || inst.writes_mask)
  val next_eidx = get_next_eidx(eff_vl, eidx, incr_eew, 0.U, increments_as_mask, ctrl.bool(Elementwise))
  val eidx_tail = next_eidx === eff_vl
  val tail      = Mux(inst.reduction, acc_tail && acc_last, eidx_tail)

  io.dis.ready := (!valid || (tail && io.iss.fire)) && new VectorDecoder(io.dis.bits.funct3, io.dis.bits.funct6, io.dis.bits.rs1, io.dis.bits.rs2, supported_insns, Nil).matched

  when (io.dis.fire) {
    val dis_inst = io.dis.bits
    valid := true.B
    inst := io.dis.bits
    assert(dis_inst.vstart === 0.U)
    eidx := 0.U

    val vd_arch_mask  = get_arch_mask(dis_inst.rd , dis_inst.emul +& dis_inst.wide_vd)
    val vs1_arch_mask = get_arch_mask(dis_inst.rs1, Mux(dis_inst.reads_vs1_mask, 0.U, dis_inst.emul))
    val vs2_arch_mask = get_arch_mask(dis_inst.rs2, Mux(dis_inst.reads_vs2_mask, 0.U, dis_inst.emul +& dis_inst.wide_vs2))

    wvd_mask    := Mux(dis_inst.wvd  , FillInterleaved(egsPerVReg, vd_arch_mask), 0.U)
    rvs1_mask   := Mux(dis_inst.renv1, FillInterleaved(egsPerVReg, vs1_arch_mask), 0.U)
    rvs2_mask   := Mux(dis_inst.renv2, FillInterleaved(egsPerVReg, vs2_arch_mask), 0.U)
    rvd_mask    := Mux(dis_inst.renvd, FillInterleaved(egsPerVReg, vd_arch_mask), 0.U)
    rvm_mask    := Mux(dis_inst.renvm, ~(0.U(egsPerVReg.W)), 0.U)
    head        := true.B
    reduction_head := true.B
    acc_tail    := false.B
    acc_tail_id := 0.U
    acc_ready   := true.B

    val dis_slide = (dis_inst.funct6.isOneOf(OPIFunct6.slideup.litValue.U, OPIFunct6.slidedown.litValue.U)
      && dis_inst.funct3 =/= OPIVV)
    val dis_slide_up     = !dis_inst.funct6(0)
    val dis_vl           = dis_inst.vconfig.vl
    val dis_sew          = dis_inst.vconfig.vtype.vsew
    val dis_vlmax        = dis_inst.vconfig.vtype.vlMax
    val dis_next_eidx    = get_next_eidx(dis_vl, 0.U, dis_sew, 0.U, false.B, false.B)
    val dis_slide1       = !dis_inst.isOpi
    val dis_uscalar      = Mux(dis_inst.funct3(2), dis_inst.rs1_data, dis_inst.imm5)
    val dis_slide_offset = Mux(!dis_slide1, get_max_offset(dis_uscalar), 1.U)
    val dis_tail         = dis_next_eidx === dis_vl
    val dis_rgather_eew  = Mux(dis_inst.opif6 === OPIFunct6.rgatherei16, 1.U, dis_sew)
    slide        := dis_slide
    when (dis_slide) {
      slide_up     := dis_slide_up
      slide1       := dis_slide1
      slide_offset := dis_slide_offset
    }
    perm_head    := Mux(dis_slide && dis_slide_up,
      (dis_slide_offset << dis_sew)(dLenOffBits-1,0),
      0.U)
    perm_tail   := Mux(dis_slide,
      Mux(dis_slide_up,
        Mux(dis_tail, dis_vl << dis_sew, 0.U),
        (Mux(dis_next_eidx + dis_slide_offset <= dis_vlmax, dis_next_eidx, dis_vlmax - dis_slide_offset) << dis_sew)(dLenOffBits-1,0)
      ),
      1.U << dis_rgather_eew)
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
  io.seq_hazard.bits.rintent := hazardMultiply(rvs1_mask | rvs2_mask | rvd_mask | rvm_mask)
  io.seq_hazard.bits.wintent := hazardMultiply(wvd_mask)
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

  val rgather_eidx = get_max_offset(Mux(rgather_ix && rgather, uscalar, io.perm.data & eewBitMask(vs1_eew)))
  val rgather_zero = rgather_eidx >= inst.vconfig.vtype.vlMax
  val rvs2_eidx = Mux(rgather || rgatherei16, rgather_eidx, eidx)
  io.rvs1.req.bits := getEgId(inst.rs1, eidx     , vs1_eew, inst.reads_vs1_mask)
  io.rvs2.req.bits := getEgId(inst.rs2, rvs2_eidx, vs2_eew, inst.reads_vs2_mask)
  io.rvd.req.bits  := getEgId(inst.rd , eidx     , vs3_eew, false.B)
  io.rvm.req.bits  := getEgId(0.U     , eidx     , 0.U    , true.B)

  io.rvs1.req.valid := valid && renv1
  io.rvs2.req.valid := valid && renv2
  io.rvd.req.valid  := valid && renvd
  io.rvm.req.valid  := valid && renvm

  val read_perm_buffer = ctrl.bool(UsesPermuteSeq) && (!slide || Mux(slide_up,
    next_eidx > slide_offset,
    eidx +& slide_offset < inst.vconfig.vtype.vlMax))

  io.perm.req.bits.head := perm_head
  io.perm.req.bits.tail := perm_tail

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
    !(renacc && !acc_ready)
  )
  io.perm.req.valid := iss_valid && read_perm_buffer && io.iss.ready
  io.iss.valid := iss_valid && !(inst.reduction && reduction_head)

  io.iss.bits.rvs1_data := io.rvs1.resp
  io.iss.bits.rvs2_data := io.rvs2.resp
  io.iss.bits.rvd_data  := io.rvd.resp
  io.iss.bits.rvs1_elem := extractElem(io.rvs1.resp, vs1_eew, eidx)
  io.iss.bits.rvs2_elem := extractElem(io.rvs2.resp, vs2_eew, eidx)
  io.iss.bits.rvd_elem  := extractElem(io.rvd.resp , vs3_eew, eidx)
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
  val slide1up_mask = Mux(head && !inst.isOpi, eewByteMask(vs2_eew), 0.U)
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
    io.iss.bits.rvs1_elem := sscalar
    io.iss.bits.rvs1_data := dLenSplat(sscalar, vs1_eew)
  }

  when (inst.reduction) {
    val acc_bits = acc.asUInt
    val elementwise_acc = inst.opff6.isOneOf(OPFFunct6.fredosum, OPFFunct6.fwredosum) || (
      vParams.useScalarFPMisc.B && ctrl.bool(FPComp) && inst.isOpf
    ) || (
      vParams.useScalarFPFMA.B && ctrl.bool(FPAdd) && inst.isOpf
    )

    when (elementwise_acc && !acc_tail) {
      io.iss.bits.rvs2_data := io.iss.bits.rvs2_elem
      val mask_bit = Mux(use_wmask, (io.rvm.resp >> eidx(log2Ceil(dLen)-1,0))(0), true.B)
      io.iss.bits.wmask := VecInit.tabulate(4)({sew => Fill(1 << sew, mask_bit)})(vd_eew)
    }
    when (acc_tail) {
      val folded = VecInit.tabulate(log2Ceil(dLenB))(i => {
        val start = dLen >> (1 + i)
        acc_bits(2*start-1,start)
      })(acc_tail_id)
      io.iss.bits.rvs1_elem := Mux(acc_copy, acc_init, folded)
      io.iss.bits.rvs1_data := Mux(acc_copy, acc_init, folded)
      io.iss.bits.rvs1_eew := vd_eew
      io.iss.bits.rvs2_elem := acc_bits
      io.iss.bits.rvs2_data := acc_bits
      io.iss.bits.rvs2_eew  := vd_eew
    } .otherwise {
      io.iss.bits.rvs1_elem := acc_bits
      io.iss.bits.rvs1_data := acc_bits
      io.iss.bits.rvs1_eew  := vd_eew
    }
  }
  when (rgather_v || rgatherei16) {
    io.iss.bits.rvs1_elem := rgather_eidx
    io.iss.bits.rvs1_data := rgather_eidx
  }
  when (rgather_zero && (rgather || rgatherei16)) {
    io.iss.bits.rvs2_elem := 0.U
    io.iss.bits.rvs2_data := 0.U
  }
  when (slide) {
    io.iss.bits.rvs2_elem := io.perm.data & slide_down_bit_mask
    io.iss.bits.rvs2_data := io.perm.data & slide_down_bit_mask
  }

  when (iss_valid && inst.reduction && reduction_head) {
    val v0_mask = eewBitMask(vd_eew)
    acc := ((acc_init & ~v0_mask.pad(dLen)) | (io.rvs1.resp & v0_mask)).asTypeOf(Vec(dLenB, UInt(8.W)))
    reduction_head := false.B
  }

  when (io.iss.fire && !tail) {
    when (next_is_new_eg(eidx, next_eidx, vd_eew, inst.writes_mask) && !inst.reduction && !compress) {
      val wvd_clr_mask = UIntToOH(io.iss.bits.wvd_eg)
      wvd_mask  := wvd_mask  & ~wvd_clr_mask
    }
    when (next_is_new_eg(eidx, next_eidx, vs2_eew, inst.reads_vs2_mask) && !(inst.reduction && head) && !rgather_v && !rgatherei16) {
      rvs2_mask := rvs2_mask & ~UIntToOH(io.rvs2.req.bits)
    }
    when (rgather_ix) {
      rvs2_mask := 0.U
    }
    when (next_is_new_eg(eidx, next_eidx, vs1_eew, inst.reads_vs1_mask)) {
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

    when (ctrl.bool(UsesPermuteSeq) && slide) {
      val next_next_eidx = get_next_eidx(eff_vl, next_eidx, incr_eew, 0.U, increments_as_mask, ctrl.bool(Elementwise))
      val next_tail = next_next_eidx === eff_vl
      perm_head := Mux(slide_up,
        Mux(next_eidx < slide_offset, (slide_offset << vs2_eew)(dLenOffBits-1,0), 0.U),
        next_eidx << vs2_eew)
      perm_tail := Mux(slide_up,
        Mux(next_tail, eff_vl << vs2_eew, 0.U),
        (Mux(next_next_eidx + slide_offset <= inst.vconfig.vtype.vlMax, next_next_eidx, inst.vconfig.vtype.vlMax - slide_offset) << vs2_eew)(dLenOffBits-1,0))
    }
  }

  io.busy := valid
}
