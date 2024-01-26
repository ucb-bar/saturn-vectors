package vector.backend

import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import vector.mem.{VectorMemIO, MaskIndex, VectorMemUnit}
import vector.exu._
import vector.common._
import vector.insns._

class VectorBackend(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val issue = Flipped(Decoupled(new VectorIssueInst))

    val mem = new VectorMemIO

    val backend_busy = Output(Bool())
    val mem_busy = Output(Bool())
    val vm_busy = Output(Bool())

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
  vmu.io.dmem <> io.mem

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
  vmu.io.enq.bits.phys := issue_inst.phys
  vmu.io.enq.bits.base_addr := issue_inst.rs1_data
  vmu.io.enq.bits.stride := issue_inst.rs2_data
  vmu.io.enq.bits.vstart := issue_inst.vstart
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
    Module(new IntegerPipe).suggestName("vintfu"),
    Module(new BitwisePipe).suggestName("vbitfu"),
    (if (vParams.useSegmentedIMul)
      Module(new SegmentedMultiplyPipe(4))
    else
      Module(new ElementwiseMultiplyPipe(4))
    ).suggestName("vmulfu"),
    Module(new IterativeIntegerDivider).suggestName("vdivfu"),
    Module(new MaskUnit).suggestName("vmaskfu"),
    Module(new PermuteUnit).suggestName("vpermfu"),
  )

  val fpFUs = if (vParams.useScalarFPUFMAPipe) {
    val shared_fma = Module(new ElementwiseFPUFMA(5)).suggestName("fpfma") 
    //val shared_fpu = Module(new ElementwiseFPU).suggestName("fpfu")
  
    io.fp_req <> shared_fma.io_fp_req
    shared_fma.io_fp_resp <> io.fp_resp
    
    //val fpArb = Module(new Arbiter(new FPInput(), 2))
    //val fpArb = Module(new InOrderArbiter(new FPInput(), new FPResult(), 2))

    //io.fp_req <> fpArb.io.out
    //fpArb.io.in(0) <> shared_fma.io_fp_req
    //shared_fma.io_fp_resp <> io.fp_resp
    //fpArb.io.in(1) <> shared_fpu.io_fp_req
    //shared_fpu.io_fp_resp <> io.fp_resp

    //io.fp_req <> fpArb.io.out_req
    //fpArb.io.out_resp <> io.fp_resp

    //fpArb.io.in_req(0) <> shared_fma.io_fp_req
    //shared_fma.io_fp_resp <> fpArb.io.in_resp(0)

    //fpArb.io.in_req(1) <> shared_fpu.io_fp_req
    //shared_fpu.io_fp_resp <> fpArb.io.in_resp(1)

    //Seq(shared_fma, shared_fpu)
    Seq(shared_fma)
  } else {
    io.fp_req.valid := false.B
    io.fp_req.bits := DontCare
    io.fp_resp.ready := DontCare
    Seq(
      Module(new FPFMAPipe(vParams.fmaPipeDepth)).suggestName("vfpfmafu"),
      Module(new FPDivSqrt).suggestName("vfdivfu"),
      Module(new FPCompPipe).suggestName("vfcmpfu"),
      Module(new FPConvPipe).suggestName("vfcvtfu")
    )
  }

  val perm_buffer = Module(new Compactor(dLenB, dLenB, UInt(8.W), true))

  val vxu = new ExecutionUnit(integerFUs ++ fpFUs)

  val vlissq = Module(new IssueQueue(vParams.vlissqEntries))
  val vsissq = Module(new IssueQueue(vParams.vsissqEntries))
  val vxissq = Module(new IssueQueue(vParams.vxissqEntries))
  val vpissq = Module(new IssueQueue(vParams.vpissqEntries))

  val vls = Module(new LoadSequencer)
  val vss = Module(new StoreSequencer)
  val vxs = Module(new ExecuteSequencer(vxu.supported_insns))
  val vps = Module(new PermuteSequencer(vxu.supported_insns))

  val issGroups = Seq(
    (vlissq, vls),
    (vsissq, vss),
    (vxissq, vxs),
    (vpissq, vps)
  )
  val issqs = issGroups.map(_._1)
  val seqs = issGroups.map(_._2)

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

  val xdis_ctrl = new VectorDecoder(vdq.io.deq.bits.funct3, vdq.io.deq.bits.funct6, vdq.io.deq.bits.rs1, vdq.io.deq.bits.rs2, vxu.supported_insns,
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
  for (((issq, seq), i) <- issGroups.zipWithIndex) {
    val otherIssqs = issqs.zipWithIndex.filter(_._2 != i).map(_._1)
    val otherSeqs = seqs.zipWithIndex.filter(_._2 != i).map(_._1)

    issq_stall(i) := seq.accepts(vdq.io.deq.bits) && !issq.io.enq.ready
    issq.io.enq.valid := vdq.io.deq.valid && !issq_stall.orR && seq.accepts(vdq.io.deq.bits)
    issq.io.enq.bits.viewAsSupertype(new VectorIssueInst) := vdq.io.deq.bits


    val vat = seq.io.vat

    seq.io.dis.valid := issq.io.deq.valid
    seq.io.dis.bits := issq.io.deq.bits
    issq.io.deq.ready := seq.io.dis.ready

    seq.io.rvs1 := DontCare
    seq.io.rvs2 := DontCare
    seq.io.rvd := DontCare
    seq.io.rvm := DontCare
    seq.io.perm := DontCare
    seq.io.sub_dlen := 0.U
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

    val older_pipe_writes = vxu.hazards.map { h =>
      Mux(vatOlder(h.bits.vat, vat) && h.valid, h.bits.eg_oh, 0.U)
    }.reduce(_|_)

    seq.io.older_writes := older_pipe_writes | older_wintents
    seq.io.older_reads := older_rintents
  }

  val vrf = Seq.fill(2) { Module(new RegisterFileBank(4, 1, egsTotal/2)) }

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
  val writes = Seq.tabulate(2) { i =>
    val arb = Module(new Arbiter(new VectorWrite(dLen), 3))
    vrf(i).io.write(0).valid := arb.io.out.valid
    vrf(i).io.write(0).bits  := arb.io.out.bits
    arb.io.out.ready := true.B
    arb.io.in
  }

  for (b <- 0 until 2) {
    writes(b)(0).valid := vxu.write.valid && vxu.write.bits.eg(0) === b.U
    writes(b)(0).bits.data  := vxu.write.bits.data
    writes(b)(0).bits.mask  := vxu.write.bits.mask
    writes(b)(0).bits.eg    := vxu.write.bits.eg >> 1
    when (vxu.write.valid) { assert(writes(b)(0).ready) }
  }

  load_write.ready := Mux1H(UIntToOH(load_write.bits.eg(0)), writes.map(_(1).ready))
  for (b <- 0 until 2) {
    writes(b)(1).valid := load_write.valid && load_write.bits.eg(0) === b.U
    writes(b)(1).bits.eg   := load_write.bits.eg >> 1
    writes(b)(1).bits.data := load_write.bits.data
    writes(b)(1).bits.mask := load_write.bits.mask
    writes(b)(2).valid := resetting
    writes(b)(2).bits.eg := reset_ctr
    writes(b)(2).bits.data := 0.U
    writes(b)(2).bits.mask := ~(0.U(dLen.W))
  }

  // Read ports are
  // vxs-vrs1, vmu-index, frontend-index
  // vxs-vrs2
  // vxs-vrs3, vss-vrd
  // vls-mask, vss-mask, vxs-mask, vps-mask, frontend-mask
  val reads = Seq(3, 1, 2, 5).zipWithIndex.map { case (rc, i) =>
    val arb = Module(new RegisterReadXbar(rc))
    vrf(0).io.read(i) <> arb.io.out(0)
    vrf(1).io.read(i) <> arb.io.out(1)
    arb.io.in
  }

  reads(0)(1) <> vps.io.rvs2
  vps.io.rvs1.req.ready := true.B

  reads(0)(2).req.valid := io.index_access.valid
  io.index_access.ready := reads(0)(2).req.ready
  reads(0)(2).req.bits  := getEgId(io.index_access.vrs, io.index_access.eidx, io.index_access.eew, false.B)
  io.index_access.idx   := reads(0)(2).resp >> ((io.index_access.eidx << io.index_access.eew)(dLenOffBits-1,0) << 3) & eewBitMask(io.index_access.eew)

  reads(0)(0) <> vxs.io.rvs1
  reads(1)(0) <> vxs.io.rvs2
  reads(2)(0) <> vxs.io.rvd

  reads(2)(1) <> vss.io.rvd
  vmu.io.sdata.valid   := vss.io.iss.valid
  vmu.io.sdata.bits.data := vss.io.iss.bits.stdata
  vmu.io.sdata.bits.mask := vss.io.iss.bits.stmask
  vss.io.iss.ready     := vmu.io.sdata.ready

  reads(3)(0) <> vls.io.rvm
  reads(3)(1) <> vss.io.rvm
  reads(3)(2) <> vxs.io.rvm
  reads(3)(3) <> vps.io.rvm
  reads(3)(4).req.valid := io.mask_access.valid
  reads(3)(4).req.bits  := getEgId(0.U, io.mask_access.eidx, 0.U, true.B)
  io.mask_access.ready  := reads(3)(4).req.ready
  io.mask_access.mask   := reads(3)(4).resp >> io.mask_access.eidx(log2Ceil(dLen)-1,0)


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

  perm_buffer.io.pop <> vxs.io.perm.req
  vxs.io.perm.data := perm_buffer.io.pop_data.asUInt

  // Clear the age tags
  def clearVat(fire: Bool, tag: UInt) = when (fire) {
    assert(vat_valids(tag))
    vat_valids(tag) := false.B
  }

  clearVat(vls.io.iss.fire && vls.io.iss.bits.tail, vls.io.iss.bits.vat)
  clearVat(vmu.io.vat_release.valid               , vmu.io.vat_release.bits)
  clearVat(vxu.vat_release.valid               , vxu.vat_release.bits)

  vxu.iss <> vxs.io.iss
  vxs.io.sub_dlen := vxu.iss_sub_dlen
  vxs.io.acc := vxu.acc_write


  // Signalling to frontend
  val seq_inflight_wv0 = (seqs.map(_.io.seq_hazard).map { h =>
    h.valid && ((h.bits.wintent & ~(0.U(egsPerVReg.W))) =/= 0.U)
  } ++ issqs.map(_.io.hazards).flatten.map { h =>
    h.valid && h.bits.wintent(0)
  } ++ vxu.hazards.map { h =>
    h.valid && (h.bits.eg < egsPerVReg.U)
  }).orR
  val vdq_inflight_wv0 = vdq.io.peek.map { h =>
    h.valid && h.bits.may_write_v0
  }.orR

  io.mem_busy := vmu.io.busy
  io.vm_busy := seq_inflight_wv0 || vdq_inflight_wv0
  io.backend_busy := vdq.io.deq.valid || seqs.map(_.io.busy).orR || vxu.busy || resetting
  io.set_vxsat := vxu.set_vxsat
  io.set_fflags := vxu.set_fflags
  scalar_resp_arb.io.in(0) <> vxu.scalar_write
}
