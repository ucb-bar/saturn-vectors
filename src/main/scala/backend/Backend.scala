package vector.backend

import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tile.{CoreModule}
import freechips.rocketchip.util._
import vector.mem.{VectorMemIO, MaskIndex, VectorMemUnit}
import vector.exu._
import vector.common._

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


  when (issue_inst.vconfig.vl <= issue_inst.vstart) {
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

  val vlissq  = Module(new IssueQueue(vParams.vlissqEntries))
  val vsissq  = Module(new IssueQueue(vParams.vsissqEntries))
  val vxissq  = Module(new IssueQueue(vParams.vxissqEntries))
  val vimissq = Module(new IssueQueue(vParams.vimissqEntries))

  val vls  = Module(new LoadSequencer)
  val vss  = Module(new StoreSequencer)
  val vxs  = Module(new ExecuteSequencer)
  val vims = Module(new IndexMaskSequencer)

  val issGroups = Seq(
    (vlissq, vls),
    (vsissq, vss),
    (vxissq, vxs),
    (vimissq, vims)
  )
  val issqs = issGroups.map(_._1)
  val seqs = issGroups.map(_._2)

  val vxu = Module(new ExecutionUnit(Seq(
    () => new IntegerPipe,
    () => new BitwisePipe,
    () => if (vParams.useSegmentedIMul) (new SegmentedMultiplyPipe(3)) else (new ElementwiseMultiplyPipe(3)),
    () => new IterativeIntegerDivider,
    () => new MaskUnit,
    () => new FPFMAPipe(vParams.fmaPipeDepth),
    () => new FPDivSqrt,
    () => new FPCompPipe,
    () => new FPConvPipe,
  )))
  val pipe_hazards = vxu.io.hazards

  vlissq.io.enq.bits.reduction := false.B
  vlissq.io.enq.bits.wide_vd := false.B
  vlissq.io.enq.bits.wide_vs2 := false.B
  vlissq.io.enq.bits.writes_mask := false.B
  vlissq.io.enq.bits.reads_mask := false.B
  vlissq.io.enq.bits.nf_log2 := VecInit.tabulate(8)({nf => log2Ceil(nf+1).U})(vdq.io.deq.bits.nf)
  vlissq.io.enq.bits.renv1 := false.B
  vlissq.io.enq.bits.renv2 := false.B
  vlissq.io.enq.bits.renvd := false.B
  vlissq.io.enq.bits.renvm := !vdq.io.deq.bits.vm
  vlissq.io.enq.bits.wvd   := true.B

  vsissq.io.enq.bits.reduction := false.B
  vsissq.io.enq.bits.wide_vd := false.B
  vsissq.io.enq.bits.wide_vs2 := false.B
  vsissq.io.enq.bits.writes_mask := false.B
  vsissq.io.enq.bits.reads_mask := false.B
  vsissq.io.enq.bits.nf_log2 := VecInit.tabulate(8)({nf => log2Ceil(nf+1).U})(vdq.io.deq.bits.nf)
  vsissq.io.enq.bits.renv1 := false.B
  vsissq.io.enq.bits.renv2 := false.B
  vsissq.io.enq.bits.renvd := true.B
  vsissq.io.enq.bits.renvm := !vdq.io.deq.bits.vm && vdq.io.deq.bits.mop === mopUnit
  vsissq.io.enq.bits.wvd   := false.B

  vimissq.io.enq.bits.reduction := false.B
  vimissq.io.enq.bits.wide_vd := false.B
  vimissq.io.enq.bits.wide_vs2 := false.B
  vimissq.io.enq.bits.writes_mask := false.B
  vimissq.io.enq.bits.reads_mask := false.B
  vimissq.io.enq.bits.nf_log2 := 0.U
  vimissq.io.enq.bits.renv1 := false.B
  vimissq.io.enq.bits.renv2 := vdq.io.deq.bits.mop(0)
  vimissq.io.enq.bits.renvd := true.B
  vimissq.io.enq.bits.renvm := !vdq.io.deq.bits.vm && vdq.io.deq.bits.mop === mopUnit
  vimissq.io.enq.bits.wvd   := false.B

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
  val dis_wide_vd :: dis_wide_vs2 :: dis_writes_mask :: dis_reads_mask :: Nil = VecDecode.applyBools(
    vdq.io.deq.bits.funct3, vdq.io.deq.bits.funct6, Seq.fill(4)(false.B), decode_table)

  vxissq.io.enq.bits.reduction   := (
    (vdq.io.deq.bits.isOpm && vdq.io.deq.bits.funct6 <= OPMFunct6.redmax.asUInt) ||
      vdq.io.deq.bits.opif6.isOneOf(OPIFunct6.wredsum, OPIFunct6.wredsumu) ||
      vdq.io.deq.bits.opff6.isOneOf(OPFFunct6.fredusum, OPFFunct6.fredosum, OPFFunct6.fwredusum, OPFFunct6.fwredosum, OPFFunct6.fredmax, OPFFunct6.fredmin)
  )
  vxissq.io.enq.bits.wide_vd     := dis_wide_vd
  when (vdq.io.deq.bits.funct3.isOneOf(OPFVV) && vdq.io.deq.bits.opff6 === OPFFunct6.funary0 && vdq.io.deq.bits.rs1(3)) {
    vxissq.io.enq.bits.wide_vd    := true.B
  }
  vxissq.io.enq.bits.wide_vs2    := dis_wide_vs2
  when (vdq.io.deq.bits.funct3.isOneOf(OPFVV) && vdq.io.deq.bits.opff6 === OPFFunct6.funary0 && vdq.io.deq.bits.rs1(4)) {
    vxissq.io.enq.bits.wide_vs2  := true.B
  }
  vxissq.io.enq.bits.writes_mask := dis_writes_mask
  when (vdq.io.deq.bits.funct3 === OPMVV && vdq.io.deq.bits.opmf6.isOneOf(OPMFunct6.munary0) && !vdq.io.deq.bits.rs1(4)) {
    vxissq.io.enq.bits.writes_mask := true.B
  }
  vxissq.io.enq.bits.renv1       := vdq.io.deq.bits.funct3.isOneOf(OPIVV, OPFVV, OPMVV)
  when ((vdq.io.deq.bits.funct3 === OPFVV && (vdq.io.deq.bits.opff6 === OPFFunct6.funary1)) ||
    (vdq.io.deq.bits.funct3 === OPMVV && (vdq.io.deq.bits.opmf6.isOneOf(OPMFunct6.wrxunary0, OPMFunct6.munary0)))
  ) {
    vxissq.io.enq.bits.renv1       := false.B
  }
  vxissq.io.enq.bits.renv2 := true.B
  when (((vdq.io.deq.bits.opif6 === OPIFunct6.merge || vdq.io.deq.bits.opff6 === OPFFunct6.fmerge) && vdq.io.deq.bits.vm) ||
    (vdq.io.deq.bits.opmf6 === OPMFunct6.wrxunary0 && vdq.io.deq.bits.funct3 === OPMVX) ||
    (vdq.io.deq.bits.opff6 === OPFFunct6.wrfunary0 && vdq.io.deq.bits.funct3 === OPFVF)) {
    vxissq.io.enq.bits.renv2 := false.B
  }
  vxissq.io.enq.bits.reads_mask  := dis_reads_mask
  when (vdq.io.deq.bits.funct3.isOneOf(OPMVV) && vdq.io.deq.bits.opmf6 === OPMFunct6.wrxunary0 && vdq.io.deq.bits.rs1(4)) {
    vxissq.io.enq.bits.reads_mask := true.B
  }
  vxissq.io.enq.bits.nf_log2 := 0.U
  vxissq.io.enq.bits.renvd       := vdq.io.deq.bits.opmf6.isOneOf(
    OPMFunct6.macc, OPMFunct6.nmsac, OPMFunct6.madd, OPMFunct6.nmsub,
    OPMFunct6.wmaccu, OPMFunct6.wmacc, OPMFunct6.wmaccsu, OPMFunct6.wmaccus) || vdq.io.deq.bits.opff6.isOneOf(
    OPFFunct6.fmacc, OPFFunct6.fnmacc, OPFFunct6.fmsac, OPFFunct6.fnmsac,
      OPFFunct6.fmadd, OPFFunct6.fnmadd, OPFFunct6.fmsub, OPFFunct6.fnmsub,
      OPFFunct6.fwmacc, OPFFunct6.fwnmacc, OPFFunct6.fwmsac, OPFFunct6.fwnmsac)
  vxissq.io.enq.bits.renvm       := !vdq.io.deq.bits.vm || vdq.io.deq.bits.opif6 === OPIFunct6.merge || vdq.io.deq.bits.opff6 === OPFFunct6.fmerge
  vxissq.io.enq.bits.wvd := !(vdq.io.deq.bits.opmf6 === OPMFunct6.wrxunary0 && vdq.io.deq.bits.funct3 === OPMVV)

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

    val older_pipe_writes = pipe_hazards.map { h =>
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
    writes(b)(0).valid := vxu.io.write.valid && vxu.io.write.bits.eg(0) === b.U
    writes(b)(0).bits.data  := vxu.io.write.bits.data
    writes(b)(0).bits.mask  := vxu.io.write.bits.mask
    writes(b)(0).bits.eg    := vxu.io.write.bits.eg >> 1
    when (vxu.io.write.valid) { assert(writes(b)(0).ready) }
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
  // vls-mask, vss-mask, vxs-mask, vims-mask, frontend-mask
  val reads = Seq(3, 1, 2, 5).zipWithIndex.map { case (rc, i) =>
    val arb = Module(new RegisterReadXbar(rc))
    vrf(0).io.read(i) <> arb.io.out(0)
    vrf(1).io.read(i) <> arb.io.out(1)
    arb.io.in
  }

  reads(0)(1) <> vims.io.rvs2
  vims.io.rvs1.req.ready := true.B

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
  reads(3)(3) <> vims.io.rvm
  reads(3)(4).req.valid := io.mask_access.valid
  reads(3)(4).req.bits  := getEgId(0.U, io.mask_access.eidx, 0.U, true.B)
  io.mask_access.ready  := reads(3)(4).req.ready
  io.mask_access.mask   := reads(3)(4).resp >> io.mask_access.eidx(log2Ceil(dLen)-1,0)


  val maskindex_q = Module(new Queue(new MaskIndex, 2))
  vmu.io.maskindex <> maskindex_q.io.deq

  maskindex_q.io.enq.valid := vims.io.iss.valid
  val index_shifted = (vims.io.iss.bits.rvs2_data >> ((vims.io.iss.bits.eidx << vims.io.iss.bits.rvs2_eew)(dLenOffBits-1,0) << 3))
  maskindex_q.io.enq.bits.index := index_shifted & eewBitMask(vims.io.iss.bits.rvs2_eew)
  maskindex_q.io.enq.bits.mask  := vims.io.iss.bits.rvm_data >> vims.io.iss.bits.eidx(log2Ceil(dLen)-1,0)
  vims.io.iss.ready             := maskindex_q.io.enq.ready

  // Clear the age tags
  def clearVat(fire: Bool, tag: UInt) = when (fire) {
    assert(vat_valids(tag))
    vat_valids(tag) := false.B
  }

  clearVat(vls.io.iss.fire && vls.io.iss.bits.tail, vls.io.iss.bits.vat)
  clearVat(vmu.io.vat_release.valid               , vmu.io.vat_release.bits)
  clearVat(vxu.io.vat_release.valid               , vxu.io.vat_release.bits)

  vxu.io.iss <> vxs.io.iss
  vxs.io.sub_dlen := vxu.io.iss_sub_dlen
  vxs.io.acc := vxu.io.acc_write


  // Signalling to frontend
  val seq_inflight_wv0 = (seqs.map(_.io.seq_hazard).map { h =>
    h.valid && ((h.bits.wintent & ~(0.U(egsPerVReg.W))) =/= 0.U)
  } ++ issqs.map(_.io.hazards).flatten.map { h =>
    h.valid && h.bits.wintent(0)
  } ++ pipe_hazards.map { h =>
    h.valid && (h.bits.eg < egsPerVReg.U)
  }).orR
  val vdq_inflight_wv0 = vdq.io.peek.map { h =>
    h.valid && h.bits.may_write_v0
  }.orR

  io.mem_busy := vmu.io.busy
  io.vm_busy := seq_inflight_wv0 || vdq_inflight_wv0
  io.backend_busy := vdq.io.deq.valid || seqs.map(_.io.busy).orR || vxu.io.busy || resetting
  io.set_vxsat := vxu.io.set_vxsat
  io.set_fflags := vxu.io.set_fflags
  scalar_resp_arb.io.in(0) <> vxu.io.scalar_write
}
