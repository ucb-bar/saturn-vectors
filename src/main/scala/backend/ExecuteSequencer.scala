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

class ExecuteSequencerIO(maxDepth: Int, nFUs: Int)(implicit p: Parameters) extends SequencerIO(new ExecuteMicroOp(nFUs)) {
  val rvs1 = Decoupled(new VectorReadReq)
  val rvs2 = Decoupled(new VectorReadReq)
  val rvd  = Decoupled(new VectorReadReq)
  val rvm  = Decoupled(new VectorReadReq)
  val pipe_write_req  = new VectorPipeWriteReqIO(maxDepth)
  val vgu = Flipped(new GatherToExecuteIO)

  val acc_valid = Input(Bool())
  val acc_ready = Output(Bool())
}

class ExecuteSequencer(supported_insns: Seq[VectorInstruction], maxPipeDepth: Int, nFUs: Int)(implicit p: Parameters) extends Sequencer[ExecuteMicroOp]()(p) {
  def usesPerm = supported_insns.count(_.props.contains(UsesGatherUnit.Y)) > 0
  def usesAcc = supported_insns.count(_.props.contains(Reduction.Y)) > 0
  def usesRvd = supported_insns.count(_.props.contains(ReadsVD.Y)) > 0
  def usesCompress = supported_insns.count(_.props.contains(F6(OPMFunct6.compress))) > 0

  def accepts(inst: VectorIssueInst) = !inst.vmu && new VectorDecoder(inst, supported_insns, Nil).matched

  val io = IO(new ExecuteSequencerIO(maxPipeDepth, nFUs))

  val valid = RegInit(false.B)
  val inst  = Reg(new BackendIssueInst)
  val head  = Reg(Bool())
  val wvd_mask  = Reg(UInt(egsTotal.W))
  val rvs1_mask = Reg(UInt(egsTotal.W))
  val rvs2_mask = Reg(UInt(egsTotal.W))
  val rvd_mask  = Reg(UInt(egsTotal.W))
  val rvm_mask  = Reg(UInt(egsPerVReg.W))
  val vs1_eew   = Reg(UInt(2.W))
  val vs2_eew   = Reg(UInt(2.W))
  val vs3_eew   = Reg(UInt(2.W))
  val vd_eew    = Reg(UInt(2.W))
  val slide     = Reg(Bool())
  val slide_up  = Reg(Bool())
  val slide1    = Reg(Bool())
  val slide_offset = Reg(UInt((1+log2Ceil(maxVLMax)).W))
  val slide_head   = Reg(UInt(dLenOffBits.W))
  val slide_tail   = Reg(UInt(dLenOffBits.W))
  val sets_wmask   = Reg(Bool())
  val uses_perm    = Reg(Bool())
  val elementwise  = Reg(Bool())
  val zext_imm5    = Reg(Bool())
  val pipelined    = Reg(Bool())
  val pipe_stages  = Reg(UInt(log2Ceil(maxPipeDepth).W))
  val fu_sel       = Reg(UInt(nFUs.W))
  val eff_vl       = Reg(UInt((1+log2Ceil(maxVLMax)).W))
  val next_eidx    = Reg(UInt((1+log2Ceil(maxVLMax)).W))
  val rgatherei16  = Reg(Bool())
  val mvnrr        = Reg(Bool())
  val incr_eew     = Reg(UInt(2.W))
  val increments_as_mask = Reg(Bool())
  val eidx      = Reg(UInt(log2Ceil(maxVLMax).W))
  val acc_fold  = Reg(Bool())
  val acc_fold_id = Reg(UInt(log2Ceil(dLenB).W))

  val compress = inst.opmf6 === OPMFunct6.compress && usesCompress.B
  val acc_copy = (vd_eew === 3.U && (dLenB == 8).B) || elementwise
  val acc_last = acc_fold_id + 1.U === log2Ceil(dLenB).U - vd_eew || acc_copy
  val uscalar  = Mux(inst.funct3(2), inst.rs1_data, inst.imm5)
  val sscalar  = Mux(inst.funct3(2), inst.rs1_data, inst.imm5_sext)
  val rgather    = inst.opif6 === OPIFunct6.rgather && usesPerm.B
  val rgather_ix = rgather && inst.funct3.isOneOf(OPIVX, OPIVI)
  val rgather_v  = rgather && inst.funct3.isOneOf(OPIVV)
  val renv1    = inst.renv1 && !inst.reduction
  val renv2    = Mux(rgather_ix, head, inst.renv2) && (!inst.reduction || !acc_fold)
  val renvd    = inst.renvd && usesRvd.B
  val renvm    = inst.renvm
  val renacc   = inst.reduction && usesAcc.B

  val use_wmask = !inst.vm && sets_wmask
  val eidx_tail = next_eidx === eff_vl
  val tail      = Mux(inst.reduction && usesAcc.B, acc_fold && acc_last, eidx_tail)

  io.dis.ready := (!valid || (tail && io.iss.fire)) && !io.dis_stall

  when (io.dis.fire) {
    val dis_inst = io.dis.bits

    val dis_ctrl = Wire(new VectorDecodedControl(supported_insns, Seq(
      SetsWMask, UsesGatherUnit, Elementwise, UsesNarrowingSext, ZextImm5,
      PipelinedExecution, PipelineStagesMinus1, FUSel(nFUs)
    ))).decode(dis_inst)

    val dis_slide = (dis_inst.funct6.isOneOf(OPIFunct6.slideup.litValue.U, OPIFunct6.slidedown.litValue.U)
      && dis_inst.funct3 =/= OPIVV) && usesPerm.B
    val dis_slide_up     = !dis_inst.funct6(0)
    val dis_vl           = dis_inst.vconfig.vl
    val dis_sew          = dis_inst.vconfig.vtype.vsew
    val dis_vlmax        = dis_inst.vconfig.vtype.vlMax
    val dis_next_eidx    = get_next_eidx(dis_vl, 0.U, dis_sew, 0.U, false.B, false.B, dLen)
    val dis_slide1       = !dis_inst.isOpi
    val dis_uscalar      = Mux(dis_inst.funct3(2), dis_inst.rs1_data, dis_inst.imm5)
    val dis_slide_offset = Mux(!dis_slide1, get_max_offset(dis_uscalar, dis_sew), 1.U)
    val dis_tail         = dis_next_eidx === dis_vl
    val dis_rgatherei16  = dis_inst.funct3 === OPIVV && dis_inst.opif6 === OPIFunct6.rgatherei16 && usesPerm.B
    val dis_rgather_eew  = Mux(dis_inst.opif6 === OPIFunct6.rgatherei16, 1.U, dis_sew)
    val dis_mvnrr         = dis_inst.funct3 === OPIVI && dis_inst.opif6 === OPIFunct6.mvnrr
    val dis_vd_arch_mask  = get_arch_mask(dis_inst.rd , dis_inst.emul +& dis_inst.wide_vd)
    val dis_vs1_arch_mask = get_arch_mask(dis_inst.rs1, Mux(dis_inst.reads_vs1_mask, 0.U, dis_inst.emul))
    val dis_vs2_arch_mask = get_arch_mask(dis_inst.rs2, Mux(dis_inst.reads_vs2_mask, 0.U, dis_inst.emul +& dis_inst.wide_vs2))
    val dis_eff_vl        = WireInit(dis_inst.vconfig.vl)
    val dis_increments_as_mask = (
      (!dis_inst.renv1 || dis_inst.reads_vs1_mask) &&
      (!dis_inst.renv2 || dis_inst.reads_vs2_mask) &&
      (!dis_inst.wvd || dis_inst.writes_mask)
    )
    val dis_vs1_eew = WireInit((dis_inst.vconfig.vtype.vsew + (dis_inst.reduction && dis_inst.wide_vd))(1,0))
    val dis_vs2_eew = WireInit((dis_inst.vconfig.vtype.vsew + dis_inst.wide_vs2)(1,0))
    val dis_vs3_eew = WireInit((dis_inst.vconfig.vtype.vsew + dis_inst.wide_vd)(1,0))
    val dis_vd_eew  = WireInit((dis_inst.vconfig.vtype.vsew + dis_inst.wide_vd)(1,0))
    val dis_incr_eew = Seq(
      Mux(dis_inst.renv1, dis_vs1_eew, 0.U),
      Mux(dis_inst.renv2, dis_vs2_eew, 0.U),
      Mux(dis_inst.renvd, dis_vs3_eew, 0.U),
      dis_vd_eew).foldLeft(0.U(2.W)) { case (b, a) => Mux(a > b, a, b) }


    assert(dis_inst.vstart === 0.U)
    valid         := true.B
    inst          := io.dis.bits
    eidx          := 0.U
    wvd_mask      := Mux(dis_inst.wvd               , FillInterleaved(egsPerVReg, dis_vd_arch_mask), 0.U)
    rvs1_mask     := Mux(dis_inst.renv1             , FillInterleaved(egsPerVReg, dis_vs1_arch_mask), 0.U)
    rvs2_mask     := Mux(dis_inst.renv2             , FillInterleaved(egsPerVReg, dis_vs2_arch_mask), 0.U)
    rvd_mask      := Mux(dis_inst.renvd && usesRvd.B, FillInterleaved(egsPerVReg, dis_vd_arch_mask), 0.U)
    rvm_mask      := Mux(dis_inst.renvm             , ~(0.U(egsPerVReg.W)), 0.U)
    head          := true.B
    acc_fold      := false.B
    acc_fold_id   := 0.U
    sets_wmask    := dis_ctrl.bool(SetsWMask)
    uses_perm     := dis_ctrl.bool(UsesGatherUnit) && usesPerm.B
    elementwise   := dis_ctrl.bool(Elementwise)
    zext_imm5     := dis_ctrl.bool(ZextImm5)
    pipelined     := dis_ctrl.bool(PipelinedExecution)
    pipe_stages   := dis_ctrl.uint(PipelineStagesMinus1)
    fu_sel        := dis_ctrl.uint(FUSel(nFUs))
    slide         := dis_slide
    rgatherei16   := dis_rgatherei16
    mvnrr         := dis_mvnrr
    vs1_eew       := dis_vs1_eew
    vs2_eew       := dis_vs2_eew
    vs3_eew       := dis_vs3_eew
    vd_eew        := dis_vd_eew
    eff_vl        := dis_eff_vl
    incr_eew      := dis_incr_eew
    next_eidx     := get_next_eidx(dis_eff_vl, 0.U, dis_incr_eew, 0.U, dis_increments_as_mask, dis_ctrl.bool(Elementwise), dLen)
    increments_as_mask := dis_increments_as_mask

    when (dis_mvnrr) {
      dis_eff_vl := ((vLen/8).U >> dis_inst.vconfig.vtype.vsew) << dis_inst.emul
    }
    when (dis_inst.scalar_to_vd0) {
      dis_eff_vl := 1.U
    }
    when (dis_rgatherei16) {
      dis_vs1_eew := 1.U
    }
    when (dis_ctrl.bool(UsesNarrowingSext)) {
      dis_vs2_eew := dis_inst.vconfig.vtype.vsew - (~dis_inst.rs1(2,1) + 1.U)
    }
    when (dis_slide) {
      slide_up     := dis_slide_up
      slide1       := dis_slide1
      slide_offset := dis_slide_offset
    }

    slide_head    := Mux(dis_slide_up,
      (dis_slide_offset << dis_sew)(dLenOffBits-1,0),
      0.U)
    slide_tail   := Mux(dis_slide_up,
      Mux(dis_tail, dis_vl << dis_sew, 0.U),
      (Mux(dis_next_eidx + dis_slide_offset <= dis_vlmax,
        dis_next_eidx, dis_vlmax - dis_slide_offset) << dis_sew
      )(dLenOffBits-1,0)
    )
  } .elsewhen (io.iss.fire) {
    valid := !tail
    head := false.B
  }

  io.vat := inst.vat
  io.seq_hazard.valid := valid
  io.seq_hazard.bits.rintent := hazardMultiply(rvs1_mask | rvs2_mask | rvd_mask | rvm_mask)
  io.seq_hazard.bits.wintent := hazardMultiply(wvd_mask)
  io.seq_hazard.bits.vat := inst.vat

  val vs1_read_oh = Mux(renv1   , UIntToOH(io.rvs1.bits.eg), 0.U)
  val vs2_read_oh = Mux(renv2   , UIntToOH(io.rvs2.bits.eg), 0.U)
  val vd_read_oh  = Mux(renvd   , UIntToOH(io.rvd.bits.eg ), 0.U)
  val vm_read_oh  = Mux(renvm   , UIntToOH(io.rvm.bits.eg ), 0.U)
  val vd_write_oh = Mux(inst.wvd, UIntToOH(io.iss.bits.wvd_eg), 0.U)

  val raw_hazard = ((vs1_read_oh | vs2_read_oh | vd_read_oh | vm_read_oh) & io.older_writes) =/= 0.U
  val waw_hazard = (vd_write_oh & io.older_writes) =/= 0.U
  val war_hazard = (vd_write_oh & io.older_reads) =/= 0.U
  val data_hazard = raw_hazard || waw_hazard || war_hazard

  val rgatherv_e0_eidx = io.vgu.gather_eidx.bits
  val rgather_eidx = Mux(rgather_ix, uscalar, rgatherv_e0_eidx)
  val rgather_zero = rgather_eidx >= inst.vconfig.vtype.vlMax
  io.rvs1.bits.eg := getEgId(inst.rs1, eidx     , vs1_eew, inst.reads_vs1_mask)
  io.rvs2.bits.eg := Mux(rgather || rgatherei16,
    getEgId(inst.rs2, rgather_eidx, vs2_eew, false.B),
    getEgId(inst.rs2, eidx        , vs2_eew, inst.reads_vs2_mask)
  )
  io.rvd.bits.eg  := getEgId(inst.rd , eidx     , vs3_eew, false.B)
  io.rvm.bits.eg  := getEgId(0.U     , eidx     , 0.U    , true.B)

  io.rvs1.valid := valid && renv1
  io.rvs2.valid := valid && renv2
  io.rvd.valid  := valid && renvd
  io.rvm.valid  := valid && renvm

  // Oldest read requests get priority
  val oldest = inst.vat === io.vat_head
  io.rvs1.bits.oldest := oldest
  io.rvs2.bits.oldest := oldest
  io.rvd.bits.oldest  := oldest
  io.rvm.bits.oldest  := oldest

  val exu_scheduler = Module(new PipeScheduler(1, maxPipeDepth))
  exu_scheduler.io.reqs(0).request := valid && pipelined
  exu_scheduler.io.reqs(0).fire := io.iss.fire
  exu_scheduler.io.reqs(0).depth := pipe_stages

  val wvd_eg = getEgId(inst.rd, Mux(inst.reduction, 0.U, eidx), vd_eew, inst.writes_mask)
  io.pipe_write_req.request := valid && pipelined && exu_scheduler.io.reqs(0).available
  io.pipe_write_req.bank_sel := (if (vrfBankBits == 0) 1.U else UIntToOH(wvd_eg(vrfBankBits-1,0)))
  io.pipe_write_req.pipe_depth := pipe_stages
  io.pipe_write_req.oldest := oldest
  io.pipe_write_req.fire := io.iss.fire

  when (compress) { // The destination is not known at this poit
    io.pipe_write_req.bank_sel := ~(0.U(vParams.vrfBanking.W))
  }

  val read_slide_buffer = slide && Mux(slide_up,
    next_eidx > slide_offset,
    eidx +& slide_offset < inst.vconfig.vtype.vlMax)
  val read_eidx_buffer = rgather_v || rgatherei16

  io.vgu.slide_req.bits.head := (if (usesPerm) slide_head else 0.U)
  io.vgu.slide_req.bits.tail := (if (usesPerm) slide_tail else 0.U)

  val iss_valid = (valid &&
    !data_hazard &&
    !(renv1 && !io.rvs1.ready) &&
    !(renv2 && !io.rvs2.ready) &&
    !(renvd && !io.rvd.ready) &&
    !(renvm && !io.rvm.ready) &&
    !(read_slide_buffer && !io.vgu.slide_req.ready) &&
    !(read_eidx_buffer && !io.vgu.gather_eidx.valid) &&
    !(pipelined && !io.pipe_write_req.available) &&
    !(pipelined && !exu_scheduler.io.reqs(0).available) &&
    !(renacc && !io.acc_valid)
  )
  io.vgu.slide_req.valid := iss_valid && read_slide_buffer && io.iss.ready && usesPerm.B
  io.vgu.gather_eidx.ready := iss_valid && read_eidx_buffer && io.iss.ready && usesPerm.B
  io.iss.valid := iss_valid
  io.acc_ready := iss_valid && renacc && usesAcc.B

  io.iss.bits.rvs1_eew  := vs1_eew
  io.iss.bits.rvs2_eew  := vs2_eew
  io.iss.bits.rvd_eew   := vs3_eew
  io.iss.bits.vd_eew    := vd_eew
  io.iss.bits.sew       := inst.vconfig.vtype.vsew
  io.iss.bits.altfmt    := inst.vconfig.vtype.altfmt
  io.iss.bits.eidx      := eidx
  io.iss.bits.vl        := inst.vconfig.vl
  io.iss.bits.wvd_eg    := wvd_eg
  io.iss.bits.rs1       := inst.rs1
  io.iss.bits.rs2       := inst.rs2
  io.iss.bits.rd        := inst.rd
  io.iss.bits.funct3    := inst.funct3
  io.iss.bits.funct6    := inst.funct6
  io.iss.bits.tail      := tail
  io.iss.bits.head      := head
  io.iss.bits.vat       := inst.vat
  io.iss.bits.vm        := inst.vm
  io.iss.bits.rm        := inst.rm
  io.iss.bits.iterative := !pipelined
  io.iss.bits.pipe_depth := pipe_stages
  io.iss.bits.fu_sel    := fu_sel

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

  io.iss.bits.use_wmask := use_wmask
  io.iss.bits.eidx_mask := head_mask & tail_mask & slideup_mask
  io.iss.bits.full_tail_mask := full_tail_mask

  val slide_down_byte_mask = Mux(slide && !slide_up && next_eidx +& slide_offset > inst.vconfig.vtype.vlMax,
    Mux(eidx +& slide_offset >= inst.vconfig.vtype.vlMax,
      0.U,
      ~(0.U(dLenB.W)) >> (0.U(dLenOffBits.W) - ((inst.vconfig.vtype.vlMax - slide_offset) << vs2_eew))(dLenOffBits-1,0)),
    ~(0.U(dLenB.W)))
  val slide_down_bit_mask = FillInterleaved(8, slide_down_byte_mask)
  io.iss.bits.use_slide_rvs2 := slide
  io.iss.bits.slide_data := io.vgu.slide_data & slide_down_bit_mask

  io.iss.bits.use_scalar_rvs1 := inst.funct3.isOneOf(OPIVI, OPIVX, OPMVX, OPFVF) || rgather_v || rgatherei16
  io.iss.bits.scalar := Mux(rgather_v || rgatherei16,
    rgather_eidx,
    Mux(zext_imm5, uscalar, sscalar))
  io.iss.bits.use_zero_rvs2 := rgather_zero && (rgather || rgatherei16)

  io.iss.bits.acc       := inst.reduction && usesAcc.B
  io.iss.bits.acc_copy  := acc_copy
  io.iss.bits.acc_fold  := acc_fold
  io.iss.bits.acc_fold_id := acc_fold_id
  io.iss.bits.acc_ew      := elementwise


  when (io.iss.fire && !tail) {
    if (vParams.enableChaining) {
      when (next_is_new_eg(eidx, next_eidx, vd_eew, inst.writes_mask) && !inst.reduction && !compress) {
        val wvd_clr_mask = UIntToOH(io.iss.bits.wvd_eg)
        wvd_mask  := wvd_mask  & ~wvd_clr_mask
      }
      when (next_is_new_eg(eidx, next_eidx, vs2_eew, inst.reads_vs2_mask) && !(inst.reduction && head) && !rgather_v && !rgatherei16) {
        rvs2_mask := rvs2_mask & ~UIntToOH(io.rvs2.bits.eg)
      }
      when (rgather_ix) {
        rvs2_mask := 0.U
      }
      when (next_is_new_eg(eidx, next_eidx, vs1_eew, inst.reads_vs1_mask)) {
        rvs1_mask := rvs1_mask & ~UIntToOH(io.rvs1.bits.eg)
      }
      when (next_is_new_eg(eidx, next_eidx, vs3_eew, false.B)) {
        rvd_mask  := rvd_mask  & ~UIntToOH(io.rvd.bits.eg)
      }
      when (next_is_new_eg(eidx, next_eidx, 0.U    , true.B)) {
        rvm_mask  := rvm_mask  & ~UIntToOH(io.rvm.bits.eg)
      }
    }

    eidx := next_eidx
    val next_next_eidx = get_next_eidx(eff_vl, next_eidx, incr_eew, 0.U, increments_as_mask, elementwise, dLen)
    next_eidx := next_next_eidx

    if (usesAcc) {
      when (eidx_tail) { acc_fold := true.B }
      when (acc_fold) { acc_fold_id := acc_fold_id + 1.U }
    }


    if (usesPerm) {
      when (uses_perm && slide) {
        val next_tail = next_next_eidx === eff_vl
        slide_head := Mux(slide_up,
          Mux(next_eidx < slide_offset, (slide_offset << vs2_eew)(dLenOffBits-1,0), 0.U),
          next_eidx << vs2_eew)
        slide_tail := Mux(slide_up,
          Mux(next_tail, eff_vl << vs2_eew, 0.U),
          (Mux(next_next_eidx + slide_offset <= inst.vconfig.vtype.vlMax, next_next_eidx, inst.vconfig.vtype.vlMax - slide_offset) << vs2_eew)(dLenOffBits-1,0))
      }
    }
  }

  io.busy := valid
  io.head := head

  if (!usesRvd) { rvd_mask := 0.U }
}
