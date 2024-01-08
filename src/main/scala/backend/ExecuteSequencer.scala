package vector.backend

import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import vector.common._

class ExecuteIssueInst(implicit p: Parameters) extends VectorIssueInst()(p) {
  val reduction = Bool()    // only writes vd[0]
  val wide_vd = Bool()      // vd reads/writes at 2xSEW
  val wide_vs2 = Bool()     // vs2 reads at 2xSEW
  val writes_mask = Bool()  // writes dest as a mask
  val reads_mask = Bool()   // vs1/vs2 read as mask

  val renv1 = Bool()
  val renv2 = Bool()
  val renvd = Bool()
  val renvm = Bool()
  val wvd = Bool()
}

class ExecuteSequencer(implicit p: Parameters) extends PipeSequencer()(p) {
  val decode_table = Seq(
    (OPMFunct6.waddu    , Seq(Y,N,N,N)),
    (OPMFunct6.wadd     , Seq(Y,N,N,N)),
    (OPMFunct6.wsubu    , Seq(Y,N,N,N)),
    (OPMFunct6.wsub     , Seq(Y,N,N,N)),
    (OPMFunct6.wadduw   , Seq(Y,Y,N,N)),
    (OPMFunct6.waddw    , Seq(Y,Y,N,N)),
    (OPMFunct6.wsubuw   , Seq(Y,Y,N,N)),
    (OPMFunct6.wsubw    , Seq(Y,Y,N,N)),
    (OPIFunct6.nsra     , Seq(N,Y,N,N)),
    (OPIFunct6.nsrl     , Seq(N,Y,N,N)),
    (OPIFunct6.madc     , Seq(N,N,Y,N)),
    (OPIFunct6.msbc     , Seq(N,N,Y,N)),
    (OPIFunct6.mseq     , Seq(N,N,Y,N)),
    (OPIFunct6.msne     , Seq(N,N,Y,N)),
    (OPIFunct6.msltu    , Seq(N,N,Y,N)),
    (OPIFunct6.mslt     , Seq(N,N,Y,N)),
    (OPIFunct6.msleu    , Seq(N,N,Y,N)),
    (OPIFunct6.msle     , Seq(N,N,Y,N)),
    (OPIFunct6.msgtu    , Seq(N,N,Y,N)),
    (OPIFunct6.msgt     , Seq(N,N,Y,N)),
    (OPMFunct6.wmul     , Seq(Y,N,N,N)),
    (OPMFunct6.wmulu    , Seq(Y,N,N,N)),
    (OPMFunct6.wmulsu   , Seq(Y,N,N,N)),
    (OPMFunct6.wmaccu   , Seq(Y,N,N,N)),
    (OPMFunct6.wmacc    , Seq(Y,N,N,N)),
    (OPMFunct6.wmaccsu  , Seq(Y,N,N,N)),
    (OPMFunct6.wmaccus  , Seq(Y,N,N,N)),
    (OPIFunct6.nclip    , Seq(N,Y,N,N)),
    (OPIFunct6.nclipu   , Seq(N,Y,N,N)),
    (OPFFunct6.fwadd    , Seq(Y,N,N,N)),
    (OPFFunct6.fwsub    , Seq(Y,N,N,N)),
    (OPFFunct6.fwaddw   , Seq(Y,Y,N,N)),
    (OPFFunct6.fwsubw   , Seq(Y,Y,N,N)),
    (OPFFunct6.fwmul    , Seq(Y,N,N,N)),
    (OPFFunct6.fwmacc   , Seq(Y,N,N,N)),
    (OPFFunct6.fwnmacc  , Seq(Y,N,N,N)),
    (OPFFunct6.fwmsac   , Seq(Y,N,N,N)),
    (OPFFunct6.fwnmsac  , Seq(Y,N,N,N)),
    (OPFFunct6.mfeq     , Seq(N,N,Y,N)),
    (OPFFunct6.mfne     , Seq(N,N,Y,N)),
    (OPFFunct6.mflt     , Seq(N,N,Y,N)),
    (OPFFunct6.mfle     , Seq(N,N,Y,N)),
    (OPFFunct6.mfgt     , Seq(N,N,Y,N)),
    (OPFFunct6.mfge     , Seq(N,N,Y,N)),
    (OPMFunct6.mandnot  , Seq(N,N,Y,Y)),
    (OPMFunct6.mand     , Seq(N,N,Y,Y)),
    (OPMFunct6.mor      , Seq(N,N,Y,Y)),
    (OPMFunct6.mxor     , Seq(N,N,Y,Y)),
    (OPMFunct6.mornot   , Seq(N,N,Y,Y)),
    (OPMFunct6.mnand    , Seq(N,N,Y,Y)),
    (OPMFunct6.mnor     , Seq(N,N,Y,Y)),
    (OPMFunct6.mxnor    , Seq(N,N,Y,Y)),
    (OPIFunct6.wredsum  , Seq(Y,N,N,N)),
    (OPIFunct6.wredsumu , Seq(Y,N,N,N)),
    (OPFFunct6.fwredosum, Seq(Y,N,N,N)),
    (OPFFunct6.fwredusum, Seq(Y,N,N,N)),
    (OPMFunct6.munary0  , Seq(N,N,N,Y))
  )

  def issQEntries = vParams.vxissqEntries
  val issq = Module(new DCEQueue(new ExecuteIssueInst, issQEntries, pipe=true))

  def accepts(inst: VectorIssueInst) = !inst.vmu
  io.dis.ready := !accepts(io.dis.bits) || issq.io.enq.ready
  issq.io.enq.valid := io.dis.valid && accepts(io.dis.bits)
  issq.io.enq.bits.viewAsSupertype(new VectorIssueInst) := io.dis.bits

  val dis_wide_vd :: dis_wide_vs2 :: dis_writes_mask :: dis_reads_mask :: Nil = VecDecode.applyBools(
    io.dis.bits.funct3, io.dis.bits.funct6, Seq.fill(4)(false.B), decode_table)

  issq.io.enq.bits.reduction   := (
    (io.dis.bits.isOpm && io.dis.bits.funct6 <= OPMFunct6.redmax.asUInt) ||
    io.dis.bits.opif6.isOneOf(OPIFunct6.wredsum, OPIFunct6.wredsumu) ||
    io.dis.bits.opff6.isOneOf(OPFFunct6.fredusum, OPFFunct6.fredosum, OPFFunct6.fwredusum, OPFFunct6.fwredosum, OPFFunct6.fredmax, OPFFunct6.fredmin)
  )
  issq.io.enq.bits.wide_vd     := dis_wide_vd
  when (io.dis.bits.funct3.isOneOf(OPFVV) && io.dis.bits.opff6 === OPFFunct6.funary0 && io.dis.bits.rs1(3)) {
   issq.io.enq.bits.wide_vd    := true.B
  }
  issq.io.enq.bits.wide_vs2    := dis_wide_vs2
  when (io.dis.bits.funct3.isOneOf(OPFVV) && io.dis.bits.opff6 === OPFFunct6.funary0 && io.dis.bits.rs1(4)) {
    issq.io.enq.bits.wide_vs2  := true.B
  }
  issq.io.enq.bits.writes_mask := dis_writes_mask
  when (io.dis.bits.funct3 === OPMVV && io.dis.bits.opmf6.isOneOf(OPMFunct6.munary0) && !io.dis.bits.rs1(4)) {
    issq.io.enq.bits.writes_mask := true.B
  }
  issq.io.enq.bits.renv1       := io.dis.bits.funct3.isOneOf(OPIVV, OPFVV, OPMVV)
  when ((io.dis.bits.funct3 === OPFVV && (io.dis.bits.opff6 === OPFFunct6.funary1)) ||
        (io.dis.bits.funct3 === OPMVV && (io.dis.bits.opmf6.isOneOf(OPMFunct6.wrxunary0, OPMFunct6.munary0)))
  ) {
    issq.io.enq.bits.renv1       := false.B
  }
  issq.io.enq.bits.renv2 := true.B
  when (((io.dis.bits.opif6 === OPIFunct6.merge || io.dis.bits.opff6 === OPFFunct6.fmerge) && io.dis.bits.vm) ||
    (io.dis.bits.opmf6 === OPMFunct6.wrxunary0 && io.dis.bits.funct3 === OPMVX) ||
    (io.dis.bits.opff6 === OPFFunct6.wrfunary0 && io.dis.bits.funct3 === OPFVF)) {
    issq.io.enq.bits.renv2 := false.B
  }
  issq.io.enq.bits.reads_mask  := dis_reads_mask
  when (io.dis.bits.funct3.isOneOf(OPMVV) && io.dis.bits.opmf6 === OPMFunct6.wrxunary0 && io.dis.bits.rs1(4)) {
    issq.io.enq.bits.reads_mask := true.B
  }
  issq.io.enq.bits.renvd       := io.dis.bits.opmf6.isOneOf(
    OPMFunct6.macc, OPMFunct6.nmsac, OPMFunct6.madd, OPMFunct6.nmsub,
    OPMFunct6.wmaccu, OPMFunct6.wmacc, OPMFunct6.wmaccsu, OPMFunct6.wmaccus) || io.dis.bits.opff6.isOneOf(
    OPFFunct6.fmacc, OPFFunct6.fnmacc, OPFFunct6.fmsac, OPFFunct6.fnmsac,
    OPFFunct6.fmadd, OPFFunct6.fnmadd, OPFFunct6.fmsub, OPFFunct6.fnmsub,
    OPFFunct6.fwmacc, OPFFunct6.fwnmacc, OPFFunct6.fwmsac, OPFFunct6.fwnmsac)
  issq.io.enq.bits.renvm       := !io.dis.bits.vm || io.dis.bits.opif6 === OPIFunct6.merge || io.dis.bits.opff6 === OPFFunct6.fmerge
  issq.io.enq.bits.wvd := !(io.dis.bits.opmf6 === OPMFunct6.wrxunary0 && io.dis.bits.funct3 === OPMVV)

  for (i <- 0 until issQEntries) {
    val inst = issq.io.peek(i).bits
    io.iss_hazards(i).valid    := issq.io.peek(i).valid
    io.iss_hazards(i).bits.vat := inst.vat
    val vd_arch_mask  = get_arch_mask(inst.rd , Mux(inst.reduction , 0.U, inst.pos_lmul +& inst.wide_vd ), 4)
    val vs1_arch_mask = get_arch_mask(inst.rs1, Mux(inst.reads_mask, 0.U, inst.pos_lmul                 ), 3)
    val vs2_arch_mask = get_arch_mask(inst.rs2, Mux(inst.reads_mask, 0.U, inst.pos_lmul +& inst.wide_vs2), 4)
    io.iss_hazards(i).bits.rintent := Seq(
      (inst.renv1, vs1_arch_mask),
      (inst.renv2, vs2_arch_mask),
      (inst.renv2, vd_arch_mask),
      (inst.renvm, 1.U)
    ).map(t => Mux(t._1, t._2, 0.U)).reduce(_|_)
    io.iss_hazards(i).bits.wintent := Mux(inst.wvd, vd_arch_mask, 0.U)
  }

  val valid = RegInit(false.B)
  val inst  = Reg(new ExecuteIssueInst)
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
  val renv1    = Mux(inst.reduction, head, inst.renv1)
  val renv2    = Mux(inst.reduction, !head && !acc_tail, inst.renv2)
  val renvd    = inst.renvd
  val renvm    = inst.renvm
  val renacc   = inst.reduction

  val use_wmask = !inst.vm && !(
    inst.opif6.isOneOf(OPIFunct6.adc, OPIFunct6.madc, OPIFunct6.sbc, OPIFunct6.msbc, OPIFunct6.merge) ||
    inst.opff6.isOneOf(OPFFunct6.fmerge)
  )

  val eidx      = Reg(UInt(log2Ceil(maxVLMax).W))
  val eff_vl    = Mux((inst.funct3 === OPMVX && inst.opmf6 === OPMFunct6.wrxunary0) || (inst.funct3 === OPFVF && inst.opff6 === OPFFunct6.wrfunary0),
    1.U, // vmv.s.x
    inst.vconfig.vl)
  val next_eidx = get_next_eidx(eff_vl, eidx, incr_eew, io.sub_dlen, inst.reads_mask && (inst.writes_mask || !inst.wvd))
  val eidx_tail = next_eidx === eff_vl
  val tail      = Mux(inst.reduction, acc_tail && acc_last, eidx_tail)

  issq.io.deq.ready := !valid || (tail && io.iss.fire)

  when (issq.io.deq.fire) {
    val iss_inst = issq.io.deq.bits
    valid := true.B
    inst := issq.io.deq.bits
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

  val acc_init_table = Seq(
    (OPMFunct6.redsum   , Seq(Y,N,N,N)),
    (OPIFunct6.wredsumu , Seq(Y,N,N,N)),
    (OPIFunct6.wredsum  , Seq(Y,N,N,N)),
    (OPMFunct6.redand   , Seq(N,Y,N,N)),
    (OPMFunct6.redor    , Seq(Y,N,N,N)),
    (OPMFunct6.redxor   , Seq(Y,N,N,N)),
    (OPMFunct6.redminu  , Seq(N,Y,N,N)),
    (OPMFunct6.redmin   , Seq(N,N,Y,N)),
    (OPMFunct6.redmaxu  , Seq(Y,N,N,N)),
    (OPMFunct6.redmax   , Seq(N,N,N,Y)),
    (OPFFunct6.fredmax  , Seq(N,N,N,N)),
    (OPFFunct6.fredmin  , Seq(N,N,N,N)),
    (OPFFunct6.fredusum , Seq(Y,N,N,N)),
    (OPFFunct6.fredosum , Seq(Y,N,N,N)),
    (OPFFunct6.fwredusum, Seq(Y,N,N,N)),
    (OPFFunct6.fwredosum, Seq(Y,N,N,N)),

  )
  val acc_init_zeros :: acc_init_ones :: acc_init_pos :: acc_init_neg :: Nil = VecDecode.applyBools(
    inst.funct3, inst.funct6, Seq.fill(4)(X), acc_init_table)
  val acc_init_fp_pos = inst.opff6 === OPFFunct6.fredmin
  val acc_init_fp_neg = inst.opff6 === OPFFunct6.fredmax

  val acc_init = Mux1H(Seq(
    (acc_init_zeros ,   0.U(dLen.W)),
    (acc_init_ones  , ~(0.U(dLen.W))),
    (acc_init_pos   , VecInit.tabulate(4)({sew => Fill(dLenB >> sew, maxPosUInt(sew))})(vd_eew)),
    (acc_init_neg   , VecInit.tabulate(4)({sew => Fill(dLenB >> sew, minNegUInt(sew))})(vd_eew)),
    (acc_init_fp_pos, VecInit.tabulate(4)({sew => Fill(dLenB >> sew, maxPosFPUInt(sew))})(vd_eew)),
    (acc_init_fp_neg, VecInit.tabulate(4)({sew => Fill(dLenB >> sew, minNegFPUInt(sew))})(vd_eew)),
  ))

  io.rvs1.req.bits := getEgId(inst.rs1, eidx, vs1_eew, inst.reads_mask)
  io.rvs2.req.bits := getEgId(inst.rs2, eidx, vs2_eew, inst.reads_mask)
  io.rvd.req.bits  := getEgId(inst.rd , eidx, vs3_eew, inst.reads_mask)
  io.rvm.req.bits  := getEgId(0.U     , eidx, 0.U    , true.B)

  io.rvs1.req.valid := valid && renv1
  io.rvs2.req.valid := valid && renv2
  io.rvd.req.valid  := valid && renvd
  io.rvm.req.valid  := valid && renvm

  val iss_valid = (valid &&
    !data_hazard &&
    !(renv1 && !io.rvs1.req.ready) &&
    !(renv2 && !io.rvs2.req.ready) &&
    !(renvd && !io.rvd.req.ready) &&
    !(renvm && !io.rvm.req.ready) &&
    !(renacc && !acc_ready && !io.acc.valid)
  )
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
  val full_tail_mask = Mux(tail,
    ~(0.U(dLen.W)) >> (0.U(log2Ceil(dLen).W) - eff_vl(log2Ceil(dLen)-1,0)),
    ~(0.U(dLen.W))
  )
  val vm_off    = ((1 << dLenOffBits) - 1).U(log2Ceil(dLen).W)
  val vm_eidx   = (eidx & ~(vm_off >> vd_eew))(log2Ceil(dLen)-1,0)
  val vm_resp   = (io.rvm.resp >> vm_eidx)
  val vm_mask   = Mux(use_wmask,
    VecInit.tabulate(4)({ sew => FillInterleaved(1 << sew, vm_resp) })(vd_eew),
    ~(0.U(dLenB.W))
  )
  val acc_mask  = Mux(acc_last,
    eewByteMask(vd_eew),
    VecInit.tabulate(log2Ceil(dLenB))(i => ~(0.U((dLen>>i).W)))(acc_tail_id))
  io.iss.bits.wmask := Mux(inst.reduction && acc_tail,
    acc_mask,
    head_mask & tail_mask & vm_mask)

  io.iss.bits.rmask := Mux(inst.vm, ~(0.U(dLenB.W)), vm_resp)
  io.iss.bits.rvm_data := Mux(inst.vm, ~(0.U(dLen.W)), io.rvm.resp)
  io.iss.bits.full_tail_mask := full_tail_mask

  when (inst.funct3.isOneOf(OPIVI, OPIVX, OPMVX, OPFVF) && !inst.vmu) {
    val rs1_data = Mux(inst.funct3 === OPIVI, Cat(Fill(59, inst.imm5(4)), inst.imm5), inst.rs1_data)
    io.iss.bits.rvs1_data := dLenSplat(rs1_data, vs1_eew)
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
    when (next_is_new_eg(eidx, next_eidx, vs2_eew, inst.reads_mask) && !(inst.reduction && head)) {
      rvs2_mask := rvs2_mask & ~UIntToOH(io.rvs2.req.bits)
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
