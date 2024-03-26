package saturn.backend

import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import saturn.mem.{VectorMemIO, MaskIndex, VectorMemUnit, ScalarMemOrderCheckIO}
import saturn.exu._
import saturn.common._
import saturn.insns._

class VectorBackend(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val issue = Flipped(Decoupled(new VectorIssueInst))

    val dmem = new VectorMemIO
    val scalar_check = new ScalarMemOrderCheckIO

    val backend_busy = Output(Bool())
    val mem_busy = Output(Bool())

    val index_access = new VectorIndexAccessIO
    val mask_access = new VectorMaskAccessIO

    val scalar_resp = Decoupled(new ScalarWrite)

    val set_vxsat = Output(Bool())
    val set_fflags = Output(Valid(UInt(5.W)))

    val fp_req = Decoupled(new FPInput())
    val fp_resp = Flipped(Decoupled(new FPResult()))
  })

  require(vLen >= 64)
  require(xLen == 64)
  require(vLen >= dLen)
  require(vLen % dLen == 0)

  val vmu = Module(new VectorMemUnit)
  vmu.io.dmem <> io.dmem
  vmu.io.scalar_check <> io.scalar_check

  val vdq = Module(new DCEQueue(new VectorIssueInst, vParams.vdqEntries))

  val vat_valids = RegInit(VecInit.fill(1 << vParams.vatSz)(false.B))
  val vat_tail = RegInit(0.U(vParams.vatSz.W))
  def vatOlder(i0: UInt, i1: UInt) = cqOlder(i0, i1, vat_tail)
  val vat_available = !vat_valids(vat_tail)
  val vat_available_count = PopCount(~vat_valids.asUInt)


  when (vdq.io.enq.fire) {
    assert(!vat_valids(vat_tail))
    vat_valids(vat_tail) := true.B
    vat_tail := vat_tail + 1.U
  }

  val issue_inst = WireInit(io.issue.bits)
  issue_inst.vat := vat_tail

  io.issue.ready   := vat_available && vdq.io.enq.ready && (!issue_inst.vmu || vmu.io.enq.ready)
  vdq.io.enq.valid := vat_available && io.issue.valid   && (!issue_inst.vmu || vmu.io.enq.ready)
  vmu.io.enq.valid := vat_available && io.issue.valid   && vdq.io.enq.ready && issue_inst.bits(6,0).isOneOf(opcLoad, opcStore)

  val scalar_resp_arb = Module(new Arbiter(new ScalarWrite, 2))
  io.scalar_resp <> Queue(scalar_resp_arb.io.out)
  scalar_resp_arb.io.in(1).valid := false.B
  scalar_resp_arb.io.in(1).bits.fp := false.B
  scalar_resp_arb.io.in(1).bits.rd := io.issue.bits.rd
  scalar_resp_arb.io.in(1).bits.data := 0.U
  scalar_resp_arb.io.in(1).bits.size := 3.U

  when ((io.issue.bits.funct3 === OPMVV && io.issue.bits.opmf6 === OPMFunct6.wrxunary0 && io.issue.bits.rs1 === 0.U) ||
        (io.issue.bits.funct3 === OPFVV && io.issue.bits.opff6 === OPFFunct6.wrfunary0 && io.issue.bits.rs1 === 0.U)) {
    issue_inst.vconfig.vl := 1.U
    issue_inst.vstart := 0.U
  }


  when (issue_inst.vconfig.vl <= issue_inst.vstart && !(issue_inst.funct3 === OPIVI && issue_inst.opif6 === OPIFunct6.mvnrr)) {
    io.issue.ready := true.B
    vdq.io.enq.valid := false.B
    vmu.io.enq.valid := false.B
    when (io.issue.bits.funct3 === OPMVV && io.issue.bits.opmf6 === OPMFunct6.wrxunary0) {
      io.issue.ready := scalar_resp_arb.io.in(1).ready
      scalar_resp_arb.io.in(1).valid := io.issue.valid
      scalar_resp_arb.io.in(1).bits.data := Mux(io.issue.bits.rs1(0),
        ~(0.U(xLen.W)), // vfirst
        0.U // vpopc
      )
    }
  }

  vdq.io.enq.bits := issue_inst

  vmu.io.enq.bits.vat := issue_inst.vat
  vmu.io.enq.bits.base_offset := issue_inst.rs1_data
  vmu.io.enq.bits.stride := issue_inst.rs2_data
  vmu.io.enq.bits.page := issue_inst.page
  vmu.io.enq.bits.vstart := issue_inst.vstart
  vmu.io.enq.bits.segstart := issue_inst.segstart
  vmu.io.enq.bits.segend := issue_inst.segend
  vmu.io.enq.bits.vl := issue_inst.vconfig.vl
  vmu.io.enq.bits.mop := issue_inst.mop
  vmu.io.enq.bits.vm := issue_inst.vm
  vmu.io.enq.bits.nf := issue_inst.nf
  vmu.io.enq.bits.idx_size := issue_inst.mem_idx_size
  vmu.io.enq.bits.elem_size := Mux(issue_inst.bits(26), issue_inst.vconfig.vtype.vsew, issue_inst.bits(13,12))
  vmu.io.enq.bits.whole_reg := issue_inst.umop === lumopWhole && issue_inst.mop === mopUnit
  vmu.io.enq.bits.store := issue_inst.bits(5)

  vmu.io.vat_tail := vat_tail

  val integerFUs = Seq(
    (() => new IntegerPipe, "vintfu"),
    (() => new BitwisePipe, "vbitfu"),
    (() => new IterativeIntegerDivider(vParams.useIterativeIMul), "vdivfu"),
    (() => new MaskUnit, "vmaskfu"),
    (() => new PermuteUnit, "vpermfu"),
  ) 

  val integerMul = (!vParams.useIterativeIMul).option((if (vParams.useSegmentedIMul)
    (() => new SegmentedMultiplyPipe(vParams.imaPipeDepth), "vsegmulfu")
  else
    (() => new ElementwiseMultiplyPipe(vParams.imaPipeDepth), "velemmulfu"))
  )

  val fpFMA = if (vParams.useScalarFPFMA) {
    (() => new SharedScalarElementwiseFPFMA(vParams.fmaPipeDepth), "vsharedfpfma")
  } else {
    (() => new FPFMAPipe(vParams.fmaPipeDepth), "vfpfma")
  }

  val fpMISCs = if (vParams.useScalarFPMisc) Seq(
    (() => new SharedScalarElementwiseFPMisc, "vsharedfpmisc")
  ) else Seq(
    (() => new FPDivSqrt, "vfdivfu"),
    (() => new FPCompPipe, "vfcmpfu"),
    (() => new FPConvPipe, "vfcvtfu")
  )

  val perm_buffer = Module(new Compactor(dLenB, dLenB, UInt(8.W), true))

  val vxus = if (vParams.separateFpVxs) {
    Seq(Module(new ExecutionUnit(integerFUs ++ fpMISCs)), Module(new ExecutionUnit(Seq(fpFMA) ++ integerMul)))
  } else {
    Seq(Module(new ExecutionUnit(integerFUs ++ fpMISCs ++ Seq(fpFMA) ++ integerMul)))
  }

  require(!(vParams.separateFpVxs & vParams.useScalarFPFMA))
  io.fp_req <> vxus.head.io.shared_fp_req
  vxus.head.io.shared_fp_resp <> io.fp_resp

  val vlissq = Module(new IssueQueue(vParams.vlissqEntries))
  val vsissq = Module(new IssueQueue(vParams.vsissqEntries))
  val vxissq = Module(new IssueQueue(vParams.vxissqEntries))
  val vpissq = Module(new IssueQueue(vParams.vpissqEntries))

  val vls = Module(new LoadSequencer)
  val vss = Module(new StoreSequencer)
  val vxs = vxus.map { xu => Module(new ExecuteSequencer(xu.supported_insns)) }
  val vps = Module(new PermuteSequencer(vxus.head.supported_insns))

  if (vParams.separateFpVxs) {
    vxus.last.io.shared_fp_req.ready := false.B
    vxus.last.io.shared_fp_resp.valid := false.B
    vxus.last.io.shared_fp_resp.bits := DontCare

    assert(vxus.last.io.shared_fp_req.valid === false.B)
    assert(vxs.last.io.perm.req.valid === false.B) 
  }

  case class IssueGroup(
    issq: IssueQueue,
    seqs: Seq[PipeSequencer[_]])

  val issGroups = Seq(
    IssueGroup(vlissq, Seq(vls)),
    IssueGroup(vsissq, Seq(vss)),
    IssueGroup(vxissq, vxs),
    IssueGroup(vpissq, Seq(vps))
  )
  val issqs = issGroups.map(_.issq)
  val allSeqs = issGroups.map(_.seqs).flatten

  vlissq.io.enq.bits.reduction := false.B
  vlissq.io.enq.bits.wide_vd := false.B
  vlissq.io.enq.bits.wide_vs2 := false.B
  vlissq.io.enq.bits.writes_mask := false.B
  vlissq.io.enq.bits.reads_vs1_mask := false.B
  vlissq.io.enq.bits.reads_vs2_mask := false.B
  vlissq.io.enq.bits.nf_log2 := log2_up(vdq.io.deq.bits.nf, 8)
  vlissq.io.enq.bits.renv1 := false.B
  vlissq.io.enq.bits.renv2 := false.B
  vlissq.io.enq.bits.renvd := false.B
  vlissq.io.enq.bits.renvm := !vdq.io.deq.bits.vm
  vlissq.io.enq.bits.wvd   := true.B
  vlissq.io.enq.bits.scalar_to_vd0 := false.B
  vlissq.io.enq.bits.rs1_is_rs2 := false.B

  vsissq.io.enq.bits.reduction := false.B
  vsissq.io.enq.bits.wide_vd := false.B
  vsissq.io.enq.bits.wide_vs2 := false.B
  vsissq.io.enq.bits.writes_mask := false.B
  vsissq.io.enq.bits.reads_vs1_mask := false.B
  vsissq.io.enq.bits.reads_vs2_mask := false.B
  vsissq.io.enq.bits.nf_log2 := log2_up(vdq.io.deq.bits.nf, 8)
  vsissq.io.enq.bits.renv1 := false.B
  vsissq.io.enq.bits.renv2 := false.B
  vsissq.io.enq.bits.renvd := true.B
  vsissq.io.enq.bits.renvm := !vdq.io.deq.bits.vm && vdq.io.deq.bits.mop === mopUnit
  vsissq.io.enq.bits.wvd   := false.B
  vsissq.io.enq.bits.scalar_to_vd0 := false.B
  vsissq.io.enq.bits.rs1_is_rs2 := false.B

  vpissq.io.enq.bits.reduction := false.B
  vpissq.io.enq.bits.wide_vd := false.B
  vpissq.io.enq.bits.wide_vs2 := false.B
  vpissq.io.enq.bits.writes_mask := false.B
  vpissq.io.enq.bits.reads_vs1_mask := false.B
  vpissq.io.enq.bits.reads_vs2_mask := false.B
  vpissq.io.enq.bits.nf_log2 := 0.U
  vpissq.io.enq.bits.renv1 := false.B
  vpissq.io.enq.bits.renv2 := vdq.io.deq.bits.mop(0) || !vdq.io.deq.bits.vmu
  vpissq.io.enq.bits.renvd := true.B
  vpissq.io.enq.bits.renvm := !vdq.io.deq.bits.vm && vdq.io.deq.bits.mop === mopUnit && vdq.io.deq.bits.vmu
  vpissq.io.enq.bits.wvd   := false.B
  vpissq.io.enq.bits.scalar_to_vd0 := false.B
  vpissq.io.enq.bits.rs1_is_rs2 := !vdq.io.deq.bits.vmu && (vdq.io.deq.bits.opif6 === OPIFunct6.rgather || (vdq.io.deq.bits.funct3 === OPIVV && vdq.io.deq.bits.opif6 === OPIFunct6.rgatherei16))

  val xdis_ctrl = new VectorDecoder(vdq.io.deq.bits.funct3, vdq.io.deq.bits.funct6, vdq.io.deq.bits.rs1, vdq.io.deq.bits.rs2, vxus.map(_.supported_insns).flatten,
    Seq(Reduction, Wide2VD, Wide2VS2, WritesAsMask, ReadsVS1AsMask, ReadsVS2AsMask, ReadsVS1, ReadsVS2, ReadsVD, VMBitReadsVM, AlwaysReadsVM, WritesVD, WritesScalar, ScalarToVD0))
  vxissq.io.enq.bits.wide_vd := xdis_ctrl.bool(Wide2VD)
  vxissq.io.enq.bits.wide_vs2 := xdis_ctrl.bool(Wide2VS2)
  vxissq.io.enq.bits.writes_mask := xdis_ctrl.bool(WritesAsMask)
  vxissq.io.enq.bits.reads_vs1_mask := xdis_ctrl.bool(ReadsVS1AsMask)
  vxissq.io.enq.bits.reads_vs2_mask := xdis_ctrl.bool(ReadsVS2AsMask)
  vxissq.io.enq.bits.nf_log2 := 0.U
  vxissq.io.enq.bits.renv1 := xdis_ctrl.bool(ReadsVS1)
  vxissq.io.enq.bits.renv2 := xdis_ctrl.bool(ReadsVS2)
  vxissq.io.enq.bits.renvd := xdis_ctrl.bool(ReadsVD)
  vxissq.io.enq.bits.renvm := (!vdq.io.deq.bits.vm && xdis_ctrl.bool(VMBitReadsVM)) || xdis_ctrl.bool(AlwaysReadsVM)
  vxissq.io.enq.bits.wvd := !xdis_ctrl.bool(WritesScalar)
  vxissq.io.enq.bits.scalar_to_vd0 := xdis_ctrl.bool(ScalarToVD0)
  vxissq.io.enq.bits.reduction := xdis_ctrl.bool(Reduction)
  vxissq.io.enq.bits.rs1_is_rs2 := false.B
  when (vdq.io.deq.bits.funct3 === OPIVV && vdq.io.deq.bits.funct6 === OPIFunct6.mvnrr.litValue.U) {
    vxissq.io.enq.bits.emul := log2_up(vdq.io.deq.bits.nf, 8)
  }

  val issq_stall = Wire(Vec(issGroups.size, Bool()))
  vdq.io.deq.ready := !issq_stall.orR

  for ((group, i) <- issGroups.zipWithIndex) {
    val otherIssGroups = issGroups.zipWithIndex.filter(_._2 != i).map(_._1)
    val otherIssqs = otherIssGroups.map(_.issq)
    val otherIssqSeqs = otherIssGroups.map(_.seqs).flatten

    for ((seq, j) <- group.seqs.zipWithIndex) {
      val otherSameIssqSeqs = group.seqs.zipWithIndex.filter(_._2 != j).map(_._1)
      val otherSeqs = otherIssqSeqs ++ otherSameIssqSeqs

      val vat = seq.io.vat

      seq.io.rvs1 := DontCare
      seq.io.rvs2 := DontCare
      seq.io.rvd := DontCare
      seq.io.rvm := DontCare
      seq.io.perm := DontCare
      seq.io.acc.valid := false.B
      seq.io.acc.bits := DontCare

      val older_issq_wintents = FillInterleaved(egsPerVReg, otherIssqs.map { i =>
        i.io.hazards.map(h => Mux(vatOlder(h.bits.vat, vat) && h.valid, h.bits.wintent, 0.U))
      }.flatten.foldLeft(0.U)(_|_))
      val older_seq_wintents = otherSeqs.map { s =>
        Mux(vatOlder(s.io.seq_hazard.bits.vat, vat) && s.io.seq_hazard.valid, s.io.seq_hazard.bits.wintent, 0.U)
      }.reduce(_|_)
      val older_wintents = older_issq_wintents | older_seq_wintents

      val older_issq_rintents = FillInterleaved(egsPerVReg, otherIssqs.map { i =>
        i.io.hazards.map(h => Mux(vatOlder(h.bits.vat, vat) && h.valid, h.bits.rintent, 0.U))
      }.flatten.foldLeft(0.U)(_|_))
      val older_seq_rintents = otherSeqs.map { s =>
        Mux(vatOlder(s.io.seq_hazard.bits.vat, vat) && s.io.seq_hazard.valid, s.io.seq_hazard.bits.rintent, 0.U)
      }.reduce(_|_)
      val older_rintents = older_issq_rintents | older_seq_rintents

      val older_pipe_writes = vxus.map(_.io.pipe_hazards.toSeq).flatten.map { h =>
        Mux(vatOlder(h.bits.vat, vat) && h.valid, h.bits.eg_oh, 0.U)
      }.reduce(_|_)

      val older_iter_writes = vxus.map(_.io.iter_hazards.toSeq).flatten.map { h =>
        Mux(vatOlder(h.bits.vat, vat) && h.valid, h.bits.eg_oh, 0.U)
      }.reduce(_|_)

      seq.io.older_writes := older_pipe_writes | older_iter_writes | older_wintents
      seq.io.older_reads := older_rintents
    }

    issq_stall(i) := group.seqs.map(_.accepts(vdq.io.deq.bits)).reduce(_ || _) && !group.issq.io.enq.ready
    group.issq.io.enq.valid := vdq.io.deq.valid && !issq_stall.orR && group.seqs.map(_.accepts(vdq.io.deq.bits)).orR
    group.issq.io.enq.bits.viewAsSupertype(new VectorIssueInst) := vdq.io.deq.bits

    group.seqs.zipWithIndex.foreach{ case(s, j) =>
      s.io.dis.valid := group.issq.io.deq.valid
      s.io.dis.bits := group.issq.io.deq.bits
    }

    group.issq.io.deq.ready := group.seqs.map(_.io.dis.ready).reduce(_ || _)
  }

  // Hazard checking for multi-VXS
  // Check if there is a VRF write port hazard against the in-flight insns in other VXUs
  // Check if there is a VRF write port hazard against a simultaneously issuing insn
  //  from another VXS (check that it's actually a valid hazard)
  val inflight_hazards = WireInit(VecInit(Seq.fill(vxs.length)(false.B)))

  for (i <- 0 until vxs.length) {
    val other_vxu_idx = (0 until vxs.length).filter(_ != i)

    val inflight_hazard = other_vxu_idx.map(vxus(_).io.pipe_hazards).flatten.map { hazard =>
      hazard.valid &&
      (hazard.bits.latency === vxus(i).io.issue_pipe_latency) &&
      (hazard.bits.eg(vrfBankBits-1,0) === vxs(i).io.iss.bits.wvd_eg(vrfBankBits-1,0))
    }.reduceOption(_ || _).getOrElse(false.B)

    inflight_hazards(i) := inflight_hazard

    val issue_hazard = other_vxu_idx.map { other_iss =>
      (vxus(other_iss).io.issue_pipe_latency === vxus(i).io.issue_pipe_latency) &&
      (vxs(other_iss).io.iss.bits.wvd_eg(vrfBankBits-1,0) === vxs(i).io.iss.bits.wvd_eg(vrfBankBits-1,0)) &&
      vatOlder(vxs(other_iss).io.iss.bits.vat, vxs(i).io.iss.bits.vat) &&
      !inflight_hazards(other_iss) &&
      vxs(other_iss).io.iss.valid &&
      vxus(other_iss).io.iss.ready
    }.reduceOption(_ || _).getOrElse(false.B)

    vxus(i).io.iss.valid := vxs(i).io.iss.valid && !inflight_hazard && !issue_hazard
    vxs(i).io.iss.ready := vxus(i).io.iss.ready && !inflight_hazard && !issue_hazard
    vxus(i).io.iss.bits := vxs(i).io.iss.bits
    vxs(i).io.acc := vxus(i).io.acc_write
  }

  // Support 1,2, and 4 banks for the VRF
  require(vParams.vrfBanking == 1 || vParams.vrfBanking == 2 || vParams.vrfBanking == 4)
  val vrf = Seq.fill(vParams.vrfBanking) { Module(new RegisterFileBank(4, 1, egsTotal/vParams.vrfBanking)) }

  val load_write = Wire(Decoupled(new VectorWrite(dLen)))
  vmu.io.lresp.ready := vls.io.iss.valid && load_write.ready
  vls.io.iss.ready := vmu.io.lresp.valid && load_write.ready
  load_write.valid := vls.io.iss.valid && vmu.io.lresp.valid
  load_write.bits.eg   := vls.io.iss.bits.wvd_eg
  load_write.bits.data := vmu.io.lresp.bits
  load_write.bits.mask := FillInterleaved(8, vls.io.iss.bits.wmask)

  val resetting = RegInit(true.B)
  val reset_ctr = RegInit(0.U(log2Ceil(egsTotal/2).W))
  when (resetting) {
    reset_ctr := reset_ctr + 1.U
    io.issue.ready := false.B
  }
  when (~reset_ctr === 0.U) { resetting := false.B }

  // Write ports
  // vxu/lresp/reset
  val writes = Seq.tabulate(vParams.vrfBanking) { i =>
    val arb = Module(new Arbiter(new VectorWrite(dLen), 3))
    vrf(i).io.write(0).valid := arb.io.out.valid
    vrf(i).io.write(0).bits  := arb.io.out.bits
    arb.io.out.ready := true.B
    arb.io.in
  }

  for (b <- 0 until vParams.vrfBanking) {
    val bank_match = vxus.map{ xu => (xu.io.write.bits.bankId === b.U) && xu.io.write.valid }
    val bank_write_bits = Mux1H(bank_match, vxus.map(_.io.write.bits.data))
    val bank_write_mask = Mux1H(bank_match, vxus.map(_.io.write.bits.mask))
    val bank_writes_eg = Mux1H(bank_match, vxus.map(_.io.write.bits.eg))
    val bank_match_valid = bank_match.asUInt.orR
    writes(b)(0).valid := bank_match_valid
    writes(b)(0).bits.data := bank_write_bits
    writes(b)(0).bits.mask := bank_write_mask
    writes(b)(0).bits.eg := bank_writes_eg >> vrfBankBits
    when(bank_match_valid) { 
      assert(writes(b)(0).ready) 
      assert(PopCount(bank_match.asUInt) === 1.U)
    }
  }

  load_write.ready := Mux1H(UIntToOH(load_write.bits.bankId), writes.map(_(1).ready))
  for (b <- 0 until vParams.vrfBanking) {
    writes(b)(1).valid := load_write.valid && load_write.bits.bankId === b.U
    writes(b)(1).bits.eg   := load_write.bits.eg >> vrfBankBits
    writes(b)(1).bits.data := load_write.bits.data
    writes(b)(1).bits.mask := load_write.bits.mask
    writes(b)(2).valid := resetting
    writes(b)(2).bits.eg := reset_ctr
    writes(b)(2).bits.data := 0.U
    writes(b)(2).bits.mask := ~(0.U(dLen.W))
  }

  // Read ports are
  // vxs0-vrs1, vxs1-vrs1, vmu-index, frontend-index
  // vxs0-vrs2, vxs1-vrs1
  // vxs0-vrs3, vxs1-vrs1, vss-vrd
  // vxs0-mask, vxs1-mask, vls-mask, vss-mask, vps-mask, frontend-mask
  val reads = Seq(2 + vxs.length, vxs.length, 1 + vxs.length, 4 + vxs.length).zipWithIndex.map { case (rc, i) =>
    val arb = Module(new RegisterReadXbar(rc, vParams.vrfBanking))

    vrf.zipWithIndex.foreach { case(bank, j) =>
      bank.io.read(i) <> arb.io.out(j)
    }

    arb.io.in
  }

  vxs.zipWithIndex.foreach { case(xs, i) =>
    reads(0)(i) <> vxs(i).io.rvs1
    reads(1)(i) <> vxs(i).io.rvs2
    reads(2)(i) <> vxs(i).io.rvd
    reads(3)(i) <> vxs(i).io.rvm
  }

  reads(0)(vxs.length) <> vps.io.rvs2
  vps.io.rvs1.req.ready := true.B

  val index_access_eg = getEgId(io.index_access.vrs, io.index_access.eidx, io.index_access.eew, false.B)
  val index_access_eg_oh = UIntToOH(index_access_eg)
  val index_access_hazard = (allSeqs.map(_.io.seq_hazard).map { h =>
    h.valid && ((h.bits.wintent & index_access_eg_oh) =/= 0.U)
  } ++ issqs.map(_.io.hazards).flatten.map { h =>
    h.valid && h.bits.wintent(io.index_access.vrs)
  } ++ vxus.map(_.io.pipe_hazards).flatten.map { h =>
    h.valid && h.bits.eg === index_access_eg
  } ++ vxus.map(_.io.iter_hazards).flatten.map { h =>
    h.valid && h.bits.eg === index_access_eg
  }).orR

  reads(0)(vxs.length+1).req.valid := io.index_access.valid && !index_access_hazard
  io.index_access.ready := reads(0)(vxs.length+1).req.ready && !index_access_hazard
  reads(0)(vxs.length+1).req.bits  := index_access_eg
  io.index_access.idx   := reads(0)(vxs.length+1).resp >> ((io.index_access.eidx << io.index_access.eew)(dLenOffBits-1,0) << 3) & eewBitMask(io.index_access.eew)

  reads(2)(vxs.length) <> vss.io.rvd
  vmu.io.sdata.valid   := vss.io.iss.valid
  vmu.io.sdata.bits.data := vss.io.iss.bits.stdata
  vmu.io.sdata.bits.mask := vss.io.iss.bits.stmask
  vss.io.iss.ready     := vmu.io.sdata.ready

  reads(3)(vxs.length) <> vls.io.rvm
  reads(3)(vxs.length+1) <> vss.io.rvm
  reads(3)(vxs.length+2) <> vps.io.rvm
  val vm_busy = Wire(Bool())
  reads(3)(vxs.length+3).req.valid := io.mask_access.valid && !vm_busy
  reads(3)(vxs.length+3).req.bits  := getEgId(0.U, io.mask_access.eidx, 0.U, true.B)
  io.mask_access.ready  := reads(3)(vxs.length+3).req.ready && !vm_busy
  io.mask_access.mask   := reads(3)(vxs.length+3).resp >> io.mask_access.eidx(log2Ceil(dLen)-1,0)


  val maskindex_q = Module(new DCEQueue(new MaskIndex, 2))
  val perm_q = Module(new DCEQueue(new PermuteMicroOp, 2))

  vmu.io.maskindex <> maskindex_q.io.deq

  maskindex_q.io.enq.valid := vps.io.iss.valid && vps.io.iss.bits.vmu
  val index_shifted = (vps.io.iss.bits.rvs2_data >> ((vps.io.iss.bits.eidx << vps.io.iss.bits.rvs2_eew)(dLenOffBits-1,0) << 3))
  maskindex_q.io.enq.bits.index := index_shifted & eewBitMask(vps.io.iss.bits.rvs2_eew)
  maskindex_q.io.enq.bits.mask  := vps.io.iss.bits.rvm_data >> vps.io.iss.bits.eidx(log2Ceil(dLen)-1,0)
  vps.io.iss.ready             := Mux(vps.io.iss.bits.vmu, maskindex_q.io.enq.ready, perm_q.io.enq.ready)

  perm_q.io.enq.valid := vps.io.iss.valid && !vps.io.iss.bits.vmu
  perm_q.io.enq.bits := vps.io.iss.bits

  perm_q.io.deq.ready := perm_buffer.io.push.ready
  perm_buffer.io.push.valid := perm_q.io.deq.valid
  perm_buffer.io.push.bits.head := perm_q.io.deq.bits.eidx << perm_q.io.deq.bits.rvs2_eew
  perm_buffer.io.push.bits.tail := Mux(perm_q.io.deq.bits.tail,
    perm_q.io.deq.bits.vl << perm_q.io.deq.bits.rvs2_eew,
    0.U)
  perm_buffer.io.push_data := perm_q.io.deq.bits.rvs2_data.asTypeOf(Vec(dLenB, UInt(8.W)))

  perm_buffer.io.pop <> vxs.head.io.perm.req
  vxs.head.io.perm.data := perm_buffer.io.pop_data.asUInt

  // Clear the age tags
  def clearVat(fire: Bool, tag: UInt) = when (fire) {
    assert(vat_valids(tag))
    vat_valids(tag) := false.B
  }

  clearVat(vls.io.iss.fire && vls.io.iss.bits.tail, vls.io.iss.bits.vat)
  clearVat(vmu.io.vat_release.valid               , vmu.io.vat_release.bits)
  vxus.map(_.io.vat_release).foreach{ rel => clearVat(rel.valid, rel.bits) }

  // Signalling to frontend
  val seq_inflight_wv0 = (allSeqs.map(_.io.seq_hazard).map { h =>
    h.valid && ((h.bits.wintent & ~(0.U(egsPerVReg.W))) =/= 0.U)
  } ++ issqs.map(_.io.hazards).flatten.map { h =>
    h.valid && h.bits.wintent(0)
  } ++ vxus.map(_.io.pipe_hazards).flatten.map { h =>
    h.valid && (h.bits.eg < egsPerVReg.U)
  } ++ vxus.map(_.io.iter_hazards).flatten.map { h =>
    h.valid && (h.bits.eg < egsPerVReg.U)
  }).orR
  val vdq_inflight_wv0 = vdq.io.peek.map { h =>
    h.valid && h.bits.may_write_v0
  }.orR

  io.mem_busy := vmu.io.busy
  vm_busy := seq_inflight_wv0 || vdq_inflight_wv0
  io.backend_busy := vdq.io.deq.valid || allSeqs.map(_.io.busy).orR || vxus.map(_.io.busy).asUInt.orR || resetting
  io.set_vxsat := vxus.map(_.io.set_vxsat).asUInt.orR
  io.set_fflags.valid := vxus.map(_.io.set_fflags.valid).asUInt.orR
  io.set_fflags.bits  := vxus.map( xu => Mux(xu.io.set_fflags.valid, xu.io.set_fflags.bits, 0.U)).reduce(_|_)
  scalar_resp_arb.io.in(0) <> vxus(0).io.scalar_write
  if (vxus.length > 1) {
    for (i <- 1 until vxus.length) {
      vxus(i).io.scalar_write.ready := false.B
    }
  }
}
