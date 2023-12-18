package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import vector.mem.{VectorMemIO, MaskIndex, VectorMemUnit}
import vector.exu.{ExecutionUnit, IntegerPipe, ElementwiseMultiplyPipe, IterativeIntegerDivider, FPFMAPipe, FPCompPipe, FPConvPipe}


class VectorBackend(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val issue = Flipped(Decoupled(new VectorIssueInst))

    val mem = new VectorMemIO

    val backend_busy = Output(Bool())
    val mem_busy = Output(Bool())
    val vm_busy = Output(Bool())

    val index_access = new VectorIndexAccessIO
    val mask_access = new VectorMaskAccessIO

    val set_vxsat = Output(Bool())
    val exc = Valid(UInt(5.W))
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

  when (io.issue.bits.vconfig.vl <= io.issue.bits.vstart) {
    io.issue.ready := true.B
    vdq.io.enq.valid := false.B
    vmu.io.enq.valid := false.B
  }

  vdq.io.enq.bits := issue_inst

  vmu.io.enq.bits.vat := issue_inst.vat
  vmu.io.enq.bits.phys := issue_inst.phys
  vmu.io.enq.bits.base_addr := issue_inst.rs1_data
  vmu.io.enq.bits.stride := issue_inst.rs2_data
  vmu.io.enq.bits.vstart := issue_inst.vstart
  vmu.io.enq.bits.vl := issue_inst.vconfig.vl
  vmu.io.enq.bits.mop := issue_inst.bits(27,26)
  vmu.io.enq.bits.vm := issue_inst.bits(25)
  vmu.io.enq.bits.nf := issue_inst.bits(31,29)
  vmu.io.enq.bits.idx_size := issue_inst.bits(13,12)
  vmu.io.enq.bits.elem_size := Mux(issue_inst.bits(26), issue_inst.vconfig.vtype.vsew, issue_inst.bits(13,12))
  vmu.io.enq.bits.whole_reg := issue_inst.bits(24,20) === lumopWhole && issue_inst.bits(27,26) === mopUnit
  vmu.io.enq.bits.store := issue_inst.bits(5)

  vmu.io.vat_tail := vat_tail

  val vls = Module(new LoadSequencer)
  val vss = Module(new StoreSequencer)
  val vxs = Module(new ExecuteSequencer)
  val vims = Module(new IndexMaskSequencer)
  val seqs = Seq(vls, vss, vxs, vims)

  val vxu = Module(new ExecutionUnit(Seq(
    () => new IntegerPipe,
    () => new ElementwiseMultiplyPipe(3),
    () => new IterativeIntegerDivider,
    () => new FPFMAPipe,
    () => new FPCompPipe,
    () => new FPConvPipe
  )))

  vdq.io.deq.ready := seqs.map(_.io.dis.ready).andR
  seqs.foreach { s =>
    s.io.dis.fire := vdq.io.deq.fire
    s.io.dis.inst := vdq.io.deq.bits
    s.io.rvs1 := DontCare
    s.io.rvs2 := DontCare
    s.io.rvd := DontCare
    s.io.rvm := DontCare
    s.io.sub_dlen := 0.U
  }

  val hazards = vxu.io.hazards

  for ((seq, i) <- seqs.zipWithIndex) {
    val otherSeqs = seqs.zipWithIndex.filter(_._2 != i).map(_._1)
    val older_wintents = otherSeqs.map { s =>
      Mux(vatOlder(s.io.seq_hazards.vat, seq.io.seq_hazards.vat) && s.io.seq_hazards.valid,
        s.io.seq_hazards.wintent, 0.U)
    }.reduce(_|_)
    val older_rintents = (otherSeqs.map { s =>
      Mux(vatOlder(s.io.seq_hazards.vat, seq.io.seq_hazards.vat) && s.io.seq_hazards.valid,
        s.io.seq_hazards.rintent, 0.U)
    }).reduce(_|_)
    val older_writes = hazards.map(h =>
      Mux(vatOlder(h.bits.vat, seq.io.seq_hazards.vat) && h.valid,
        h.bits.eg_oh, 0.U)).reduce(_|_)

    seq.io.seq_hazards.writes := older_writes | older_wintents
    seq.io.seq_hazards.reads := older_rintents
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

  writes(0)(0).valid := vxu.io.writes(0).valid
  writes(0)(0).bits  := vxu.io.writes(0).bits
  when (vxu.io.writes(0).valid) { assert(writes(0)(0).ready) }

  writes(1)(0).valid := vxu.io.writes(1).valid
  writes(1)(0).bits  := vxu.io.writes(1).bits
  when (vxu.io.writes(1).valid) { assert(writes(1)(0).ready) }

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
  reads(0)(2).req.bits  := getEgId(io.index_access.vrs, io.index_access.eidx, io.index_access.eew)
  io.index_access.idx   := reads(0)(2).resp >> ((io.index_access.eidx << io.index_access.eew)(dLenOffBits-1,0) << 3) & eewBitMask(io.index_access.eew)

  reads(0)(0) <> vxs.io.rvs1
  reads(1)(0) <> vxs.io.rvs2
  reads(2)(0) <> vxs.io.rvd

  reads(2)(1) <> vss.io.rvd
  vmu.io.sdata.valid   := vss.io.iss.valid
  vmu.io.sdata.bits.data := vss.io.iss.bits.rvd_data
  vmu.io.sdata.bits.mask := vss.io.iss.bits.rmask
  vss.io.iss.ready     := vmu.io.sdata.ready

  reads(3)(0) <> vls.io.rvm
  reads(3)(1) <> vss.io.rvm
  reads(3)(2) <> vxs.io.rvm
  reads(3)(3) <> vims.io.rvm
  reads(3)(4).req.valid := io.mask_access.valid
  reads(3)(4).req.bits  := getEgId(0.U, io.mask_access.eidx >> 3, 0.U)
  io.mask_access.ready  := reads(3)(4).req.ready
  io.mask_access.mask   := reads(3)(4).resp >> io.mask_access.eidx(log2Ceil(dLen)-1,0)


  val maskindex_q = Module(new Queue(new MaskIndex, 2))
  vmu.io.maskindex <> maskindex_q.io.deq

  maskindex_q.io.enq.valid := vims.io.iss.valid
  val index_shifted = (vims.io.iss.bits.rvs2_data >> ((vims.io.iss.bits.eidx << vims.io.iss.bits.rvs2_eew)(dLenOffBits-1,0) << 3))
  maskindex_q.io.enq.bits.index := index_shifted & eewBitMask(vims.io.iss.bits.rvs2_eew)
  maskindex_q.io.enq.bits.mask  := reads(3)(3).resp >> vims.io.iss.bits.eidx(log2Ceil(dLen)-1,0)
  vims.io.iss.ready      := maskindex_q.io.enq.ready

  // Clear the age tags
  def clearVat(fire: Bool, tag: UInt) = when (fire) {
    assert(vat_valids(tag))
    vat_valids(tag) := false.B
  }

  clearVat(vls.io.iss.fire && vls.io.iss.bits.last, vls.io.iss.bits.vat)
  clearVat(vmu.io.vat_release.valid               , vmu.io.vat_release.bits)
  for (v <- vxu.io.vat_release)
    clearVat(v.valid                              , v.bits)

  vxu.io.iss <> vxs.io.iss
  vxs.io.sub_dlen := vxu.io.iss_sub_dlen


  // Signalling to frontend
  val seq_inflight_wv0 = (seqs.map(_.io.seq_hazards).map { h =>
    h.valid && ((h.wintent & ~(0.U(egsPerVReg.W))) =/= 0.U)
  } ++ hazards.map { h =>
    h.valid && (h.bits.eg < egsPerVReg.U)
  }).orR
  val vdq_inflight_wv0 = vdq.io.peek.map { h =>
    h.valid && h.bits.may_write_v0
  }.orR

  io.mem_busy := vmu.io.busy
  io.vm_busy := seq_inflight_wv0 || vdq_inflight_wv0
  io.backend_busy := vdq.io.deq.valid || seqs.map(_.io.busy).orR || vxu.io.busy || resetting
  io.set_vxsat := vxu.io.set_vxsat
  io.exc <> vxu.io.exc
}
