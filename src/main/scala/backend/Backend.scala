package saturn.backend

import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import saturn.mem._
import saturn.exu._
import saturn.common._
import saturn.insns._

class VectorBackend(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val dis = Flipped(Decoupled(new VectorIssueInst))

    val vmu = Flipped(new VectorMemDatapathIO)

    val busy = Output(Bool())

    val index_access = new VectorIndexAccessIO
    val mask_access = new VectorMaskAccessIO

    val scalar_resp = Decoupled(new ScalarWrite)

    val set_vxsat = Output(Bool())
    val set_fflags = Output(Valid(UInt(5.W)))

    val fp_req = Decoupled(new FPInput())
    val fp_resp = Flipped(Valid(new FPResult()))

    val vat_tail = Input(UInt(vParams.vatSz.W))
    val vat_head = Input(UInt(vParams.vatSz.W))

    val vat_release = Output(Vec(nRelease, Valid(UInt(vParams.vatSz.W))))
  })

  require(vLen >= 64)
  require(xLen == 64)
  require(vLen >= dLen)
  require(vLen % dLen == 0)

  def vatOlder(i0: UInt, i1: UInt) = cqOlder(i0, i1, io.vat_tail)

  // ====================================================================
  // Set up the dispatch queue, issue queues, sequencers, execution units

  val vdq = Module(new DCEQueue(new VectorIssueInst, vParams.vdqEntries))
  vdq.io.enq <> io.dis

  val perm_buffer = Module(new Compactor(dLenB, dLenB, UInt(8.W), false))

  val xissParams = vParams.issStructure.generate(vParams)
  val all_supported_insns = xissParams.map(_.insns).flatten

  val vlissq = Module(new IssueQueue(vParams.vlissqEntries, 1))
  val vsissq = Module(new IssueQueue(vParams.vsissqEntries, 1))
  val vpissq = Module(new IssueQueue(vParams.vpissqEntries, 1))
  val vrissq = Module(new IssueQueue(vParams.vrissqEntries, 1))
  val vxissqs = xissParams.map(q => Module(new IssueQueue(q.depth, q.seqs.size)).suggestName(s"vxissq_${q.name}"))

  val vls = Module(new LoadSequencer)
  val vss = Module(new StoreSequencer)
  val vps = Module(new PermuteSequencer(all_supported_insns))
  val vrs = Module(new ReductionSequencer(all_supported_insns))
  val vxs = xissParams.map(q => q.seqs.map(s =>
    Module(new ExecuteSequencer(s.insns)).suggestName(s"vxs${s.name}")
  ))

  val allSeqs = Seq(vls, vss, vps, vrs) ++ vxs.flatten
  val allIssQs = Seq(vlissq, vsissq, vpissq, vrissq) ++ vxissqs

  val vxus = xissParams.map(_.seqs.map(s => Module(new ExecutionUnit(s.fus)).suggestName(s"vxu${s.name}")))


  io.fp_req.valid := false.B
  io.fp_req.bits := DontCare
  vxus.foreach(_.foreach(_.io.shared_fp_req := DontCare))
  vxus.foreach(_.foreach(_.io.shared_fp_resp := DontCare))

  val shared_fp_vxu = vxus.flatten.filter(_.hasSharedFPUnits)
  require(shared_fp_vxu.size <= 1)
  shared_fp_vxu.headOption.foreach { vxu =>
    io.fp_req <> vxu.io.shared_fp_req
    vxu.io.shared_fp_resp <> io.fp_resp
  }

  case class IssueGroup(
    issq: IssueQueue,
    seqs: Seq[Sequencer[_]])


  val issGroups = Seq(
    IssueGroup(vlissq, Seq(vls)),
    IssueGroup(vsissq, Seq(vss)),
    IssueGroup(vpissq, Seq(vps)),
    IssueGroup(vrissq, Seq(vrs))
  ) ++ (vxissqs.zip(vxs).map { case (q, seqs) =>
    IssueGroup(q, seqs)
  })

  // ======================================
  // Set inputs to each issq/sequencer pair

  // Set common defaults
  for (issq <- allIssQs) {
    issq.io.enq.bits.reduction := false.B
    issq.io.enq.bits.wide_vd := false.B
    issq.io.enq.bits.wide_vs2 := false.B
    issq.io.enq.bits.writes_mask := false.B
    issq.io.enq.bits.reads_vs1_mask := false.B
    issq.io.enq.bits.reads_vs2_mask := false.B
    issq.io.enq.bits.nf_log2 := 0.U
    issq.io.enq.bits.renv1 := false.B
    issq.io.enq.bits.renv2 := false.B
    issq.io.enq.bits.renvd := false.B
    issq.io.enq.bits.renvm := false.B
    issq.io.enq.bits.wvd   := false.B
    issq.io.enq.bits.scalar_to_vd0 := false.B
    issq.io.enq.bits.rs1_is_rs2 := false.B
  }

  val dis_ctrl = new VectorDecoder(vdq.io.deq.bits.funct3, vdq.io.deq.bits.funct6,
    vdq.io.deq.bits.rs1, vdq.io.deq.bits.rs2, all_supported_insns, Seq(
      Reduction, Wide2VD, Wide2VS2, WritesAsMask,
      ReadsVS1AsMask, ReadsVS2AsMask, ReadsVS1, ReadsVS2, ReadsVD,
      VMBitReadsVM, AlwaysReadsVM, WritesVD, WritesScalar, ScalarToVD0
    )
  )


  // Load sequencer
  vlissq.io.enq.bits.nf_log2 := log2_up(vdq.io.deq.bits.nf, 8)
  vlissq.io.enq.bits.renvm := !vdq.io.deq.bits.vm
  vlissq.io.enq.bits.wvd   := true.B

  // Store sequencer
  vsissq.io.enq.bits.nf_log2 := log2_up(vdq.io.deq.bits.nf, 8)
  vsissq.io.enq.bits.renvd := true.B
  vsissq.io.enq.bits.renvm := !vdq.io.deq.bits.vm && vdq.io.deq.bits.mop === mopUnit

  // Permute source sequencer
  vpissq.io.enq.bits.renv2 := vdq.io.deq.bits.mop(0) || !vdq.io.deq.bits.vmu
  vpissq.io.enq.bits.renvd := true.B
  vpissq.io.enq.bits.renvm := !vdq.io.deq.bits.vm && vdq.io.deq.bits.mop =/= mopUnit && vdq.io.deq.bits.vmu
  vpissq.io.enq.bits.rs1_is_rs2 := !vdq.io.deq.bits.vmu && (vdq.io.deq.bits.opif6 === OPIFunct6.rgather || (vdq.io.deq.bits.funct3 === OPIVV && vdq.io.deq.bits.opif6 === OPIFunct6.rgatherei16))

  // Reduction sequencer
  vrissq.io.enq.bits.renv1 := true.B
  vrissq.io.enq.bits.wide_vd := dis_ctrl.bool(Wide2VD)

  // Execute sequencers
  vxissqs.foreach { vxissq =>
    vxissq.io.enq.bits.wide_vd := dis_ctrl.bool(Wide2VD)
    vxissq.io.enq.bits.wide_vs2 := dis_ctrl.bool(Wide2VS2)
    vxissq.io.enq.bits.writes_mask := dis_ctrl.bool(WritesAsMask)
    vxissq.io.enq.bits.reads_vs1_mask := dis_ctrl.bool(ReadsVS1AsMask)
    vxissq.io.enq.bits.reads_vs2_mask := dis_ctrl.bool(ReadsVS2AsMask)
    vxissq.io.enq.bits.renv1 := dis_ctrl.bool(ReadsVS1) && !dis_ctrl.bool(Reduction)
    vxissq.io.enq.bits.renv2 := dis_ctrl.bool(ReadsVS2)
    vxissq.io.enq.bits.renvd := dis_ctrl.bool(ReadsVD)
    vxissq.io.enq.bits.renvm := (!vdq.io.deq.bits.vm && dis_ctrl.bool(VMBitReadsVM)) || dis_ctrl.bool(AlwaysReadsVM)
    vxissq.io.enq.bits.wvd := !dis_ctrl.bool(WritesScalar)
    vxissq.io.enq.bits.scalar_to_vd0 := dis_ctrl.bool(ScalarToVD0)
    vxissq.io.enq.bits.reduction := dis_ctrl.bool(Reduction)
  }

  // ======================================
  // Connect VDQ to issue queues
  // Connect issue queues to sequencers

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

      seq.io.vat_head := io.vat_head

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

      val older_pipe_writes = vxus.flatten.map(_.io.pipe_hazards.toSeq).flatten.map { h =>
        Mux(h.valid, h.bits.eg_oh, 0.U)
      }.reduce(_|_)

      val older_iter_writes = vxus.flatten.map(_.io.iter_hazards.toSeq).flatten.map { h =>
        Mux(h.valid, h.bits.eg_oh, 0.U)
      }.reduce(_|_)

      seq.io.older_writes := older_pipe_writes | older_iter_writes | older_wintents
      seq.io.older_reads := older_rintents

      if (!vParams.enableOOO) {
        // stall dispatch if any other sequencers are at the head and stalled
        seq.io.dis_stall := otherSeqs.map { s =>
          s.io.busy && s.io.head && !(s.io.iss.valid && s.io.iss.ready)
        }.orR
      } else {
        seq.io.dis_stall := false.B // never stall dispatch
      }
    }

    val accepts = group.seqs.map(_.accepts(vdq.io.deq.bits))
    issq_stall(i) := !group.issq.io.enq.ready && accepts.orR

    group.issq.io.enq.valid := vdq.io.deq.valid && !issq_stall.orR && accepts.orR
    group.issq.io.enq.bits.viewAsSupertype(new VectorIssueInst) := vdq.io.deq.bits
    group.issq.io.enq.bits.seq := VecInit(accepts).asUInt

    // In case of multiple available sequencers, select the first ready one
    val valid_seqs = group.issq.io.deq.bits.seq
    val ready_seqs = VecInit(group.seqs.map(_.io.dis.ready)).asUInt
    val chosen_seq = PriorityEncoder(valid_seqs & ready_seqs)

    group.seqs.zipWithIndex.foreach{ case(s, j) =>
      s.io.dis.valid := group.issq.io.deq.valid && chosen_seq === j.U
      s.io.dis.bits := group.issq.io.deq.bits.viewAsSupertype(new BackendIssueInst)
    }
    group.issq.io.deq.ready := (valid_seqs & ready_seqs) =/= 0.U
  }

  val flat_vxs = vxs.flatten
  val flat_vxus = vxus.flatten
  require(flat_vxs.size == flat_vxus.size)

  // Hazard checking for multi-VXS
  // Check if there is a VRF write port hazard against the in-flight insns in other VXUs
  // Check if there is a VRF write port hazard against a simultaneously issuing insn
  //  from another VXS (check that it's actually a valid hazard)
  val inflight_hazards = WireInit(VecInit(Seq.fill(flat_vxs.length)(false.B)))
  for (i <- 0 until flat_vxs.length) {
    val vxs = flat_vxs(i)
    val vxu = flat_vxus(i)
    val other_vxu_idx = (0 until flat_vxs.length).filter(_ != i)

    val inflight_hazard = other_vxu_idx.map(flat_vxus(_).io.pipe_hazards).flatten.map { hazard =>
      hazard.valid &&
      (hazard.bits.latency === vxu.io.issue_pipe_latency) &&
      (hazard.bits.eg(vrfBankBits-1,0) === vxs.io.iss.bits.wvd_eg(vrfBankBits-1,0))
    }.reduceOption(_ || _).getOrElse(false.B)

    inflight_hazards(i) := inflight_hazard

    val issue_hazard = other_vxu_idx.map { other_iss =>
      (flat_vxus(other_iss).io.issue_pipe_latency === vxu.io.issue_pipe_latency) &&
      (flat_vxs(other_iss).io.iss.bits.wvd_eg(vrfBankBits-1,0) === vxs.io.iss.bits.wvd_eg(vrfBankBits-1,0)) &&
      vatOlder(flat_vxs(other_iss).io.iss.bits.vat, vxs.io.iss.bits.vat) &&
      !inflight_hazards(other_iss) &&
      flat_vxs(other_iss).io.iss.valid &&
      flat_vxus(other_iss).io.iss.ready
    }.reduceOption(_ || _).getOrElse(false.B)

    vxu.io.iss.valid := vxs.io.iss.valid && !inflight_hazard && !issue_hazard
    vxs.io.iss.ready := vxu.io.iss.ready && !inflight_hazard && !issue_hazard
    vxu.io.iss.bits.viewAsSupertype(new ExecuteMicroOp) := vxs.io.iss.bits
  }

  // ======================================
  // Connect reads to VRF

  val vrf = Module(new RegisterAccess(flat_vxs.size))
  vrf.io.vls.rvm.req <> vls.io.rvm
  vrf.io.vss.rvd.req <> vss.io.rvd
  vrf.io.vss.rvm.req <> vss.io.rvm
  vrf.io.vps.rvs2.req <> vps.io.rvs2
  vrf.io.vps.rvm.req <> vps.io.rvm
  vrs.io.rvs <> vrf.io.vrs.rvs
  vrs.io.acc_init_resp := vrf.io.vrs.rvs.resp

  for (i <- 0 until flat_vxs.size) {
    val vxs = flat_vxs(i)
    val vxu = flat_vxus(i)

    vrf.io.vxs(i).rvs1.req <> vxs.io.rvs1
    vrf.io.vxs(i).rvs2.req <> vxs.io.rvs2
    vrf.io.vxs(i).rvd.req <> vxs.io.rvd
    vrf.io.vxs(i).rvm.req <> vxs.io.rvm

    val rvs1_data = Mux1H(Seq(
      vxs.io.iss.bits.use_scalar_rvs1 -> dLenSplat(vxs.io.iss.bits.scalar, vxs.io.iss.bits.rvs1_eew),
      vxs.io.iss.bits.use_normal_rvs1 -> vrf.io.vxs(i).rvs1.resp))
    val rvs2_data = Mux1H(Seq(
      vxs.io.iss.bits.use_zero_rvs2 -> 0.U,
      vxs.io.iss.bits.use_slide_rvs2 -> vxs.io.iss.bits.slide_data,
      vxs.io.iss.bits.use_normal_rvs2 -> vrf.io.vxs(i).rvs2.resp))
    val rvd_data = vrf.io.vxs(i).rvd.resp

    vxu.io.iss.bits.rvs1_data := rvs1_data
    vxu.io.iss.bits.rvs2_data := rvs2_data
    vxu.io.iss.bits.rvd_data := rvd_data

    val rvs1_elem = extractElem(rvs1_data, vxs.io.iss.bits.rvs1_eew, vxs.io.iss.bits.eidx)
    val rvs2_elem = extractElem(rvs2_data, vxs.io.iss.bits.rvs2_eew, vxs.io.iss.bits.eidx)
    val rvd_elem = extractElem(rvd_data, vxs.io.iss.bits.rvd_eew, vxs.io.iss.bits.eidx)

    vxu.io.iss.bits.rvs1_elem := rvs1_elem
    vxu.io.iss.bits.rvs2_elem := rvs2_elem
    vxu.io.iss.bits.rvd_elem := rvd_elem

    val vm_off    = ((1 << dLenOffBits) - 1).U(log2Ceil(dLen).W)
    val vm_eidx   = (vxs.io.iss.bits.eidx & ~(vm_off >> vxs.io.iss.bits.vd_eew))(log2Ceil(dLen)-1,0)
    val vm_resp   = (vrf.io.vxs(i).rvm.resp >> vm_eidx)(dLenB-1,0)
    val vm_mask   = Mux(vxs.io.iss.bits.use_wmask,
      VecInit.tabulate(4)({ sew => FillInterleaved(1 << sew, vm_resp)(dLenB-1,0) })(vxs.io.iss.bits.vd_eew),
      ~(0.U(dLenB.W))
    )
    vxu.io.iss.bits.wmask := vxs.io.iss.bits.eidx_mask & vm_mask
    vxu.io.iss.bits.rmask := Mux(vxs.io.iss.bits.vm, ~(0.U(dLenB.W)), vm_resp)
    vxu.io.iss.bits.rvm_data := Mux(vxs.io.iss.bits.vm, ~(0.U(dLen.W)), vrf.io.vxs(i).rvm.resp)


    when (vxs.io.iss.bits.acc) {
      val acc_data = vrs.io.acc_data.bits
      when (vxs.io.iss.bits.acc_fold) {
        val acc_fold_id = vxs.io.iss.bits.acc_fold_id
        val folded = VecInit.tabulate(log2Ceil(dLenB))(i => acc_data((dLen >> i) - 1, dLen >> (i + 1)))(acc_fold_id)

        vxu.io.iss.bits.wmask := Mux(vxs.io.iss.bits.tail, eewByteMask(vxs.io.iss.bits.vd_eew), ~(0.U(dLenB.W)))
        vxu.io.iss.bits.rvs1_elem := folded
        vxu.io.iss.bits.rvs1_data := folded
        vxu.io.iss.bits.rvs1_eew := vxs.io.iss.bits.vd_eew
        vxu.io.iss.bits.rvs2_elem := acc_data
        vxu.io.iss.bits.rvs2_data := acc_data
        vxu.io.iss.bits.rvs2_eew  := vxs.io.iss.bits.vd_eew
      } .otherwise {
        when (vxs.io.iss.bits.acc_ew) {
          val mask_bit = Mux(vxs.io.iss.bits.use_wmask, (vrf.io.vxs(i).rvm.resp >> vxs.io.iss.bits.eidx(log2Ceil(dLen)-1,0))(0), true.B)
          vxu.io.iss.bits.wmask := Mux(mask_bit, eewByteMask(vxs.io.iss.bits.vd_eew), 0.U)
          vxu.io.iss.bits.rvs1_elem := acc_data
          vxu.io.iss.bits.rvs1_data := acc_data
          vxu.io.iss.bits.rvs1_eew  := vxs.io.iss.bits.vd_eew
          vxu.io.iss.bits.rvs2_data := rvs2_elem
        } .otherwise {
          vxu.io.iss.bits.rvs1_data := acc_data
          vxu.io.iss.bits.rvs1_eew := vxs.io.iss.bits.vd_eew
        }
      }
    }
  }

  val frontend_rindex = Wire(new VectorReadIO)
  val frontend_rmask  = Wire(new VectorReadIO)
  vrf.io.frontend.rindex <> frontend_rindex
  vrf.io.frontend.rmask <> frontend_rmask

  // ============================
  // Connect writes to VRF
  for (i <- 0 until flat_vxs.size) {
    vrf.io.pipe_writes(i) <> flat_vxus(i).io.pipe_write
    vrf.io.iter_writes(i) <> flat_vxus(i).io.iter_write
  }

  val load_write = Wire(Decoupled(new VectorWrite(dLen)))
  vrf.io.load_write <> load_write


  io.vmu.lresp.ready := vls.io.iss.valid && load_write.ready
  vls.io.iss.ready := io.vmu.lresp.valid && load_write.ready
  load_write.valid := vls.io.iss.valid && io.vmu.lresp.valid
  load_write.bits.eg   := vls.io.iss.bits.wvd_eg
  load_write.bits.data := io.vmu.lresp.bits.data
  val load_wmask = Mux(vls.io.iss.bits.use_rmask,
    get_vm_mask(vrf.io.vls.rvm.resp, vls.io.iss.bits.eidx, vls.io.iss.bits.elem_size),
    ~(0.U(dLenB.W)))
  load_write.bits.mask := FillInterleaved(8, vls.io.iss.bits.eidx_wmask & load_wmask)
  when (io.vmu.lresp.fire) {
    assert(io.vmu.lresp.bits.debug_id === vls.io.iss.bits.debug_id)
  }


  // ========================================
  // Connect frontend index/mask access ports

  val index_access_eg = getEgId(io.index_access.vrs, io.index_access.eidx, io.index_access.eew, false.B)
  val index_access_eg_oh = UIntToOH(index_access_eg)
  val index_access_hazard = (allSeqs.map(_.io.seq_hazard).map { h =>
    h.valid && ((h.bits.wintent & index_access_eg_oh) =/= 0.U)
  } ++ allIssQs.map(_.io.hazards).flatten.map { h =>
    h.valid && h.bits.wintent(io.index_access.vrs)
  } ++ vxus.flatten.map(_.io.pipe_hazards).flatten.map { h =>
    h.valid && h.bits.eg === index_access_eg
  } ++ vxus.flatten.map(_.io.iter_hazards).flatten.map { h =>
    h.valid && h.bits.eg === index_access_eg
  }).orR || vdq.io.peek.map(i => i.valid && !(i.bits.vmu && i.bits.store)).orR
  // TODO: this conservatively assumes a index data hazard against anything in the vdq

  frontend_rindex.req.valid := io.index_access.valid && !index_access_hazard
  io.index_access.ready := frontend_rindex.req.ready && !index_access_hazard
  frontend_rindex.req.bits.eg  := index_access_eg
  frontend_rindex.req.bits.oldest  := false.B
  io.index_access.idx   := frontend_rindex.resp >> ((io.index_access.eidx << io.index_access.eew)(dLenOffBits-1,0) << 3) & eewBitMask(io.index_access.eew)

  val vm_busy = Wire(Bool())
  frontend_rmask.req.valid    := io.mask_access.valid && !vm_busy
  frontend_rmask.req.bits.eg  := getEgId(0.U, io.mask_access.eidx, 0.U, true.B)
  frontend_rmask.req.bits.oldest := false.B
  io.mask_access.ready  := frontend_rmask.req.ready && !vm_busy
  io.mask_access.mask   := frontend_rmask.resp >> io.mask_access.eidx(log2Ceil(dLen)-1,0)

  // =====================================
  // Connect VMU index/mask access ports

  val vmu_index_q = Module(new Compactor(dLenB, dLenB, UInt(8.W), false))
  val vmu_mask_q = Module(new Compactor(dLenB, dLenB, Bool(), false))
  val perm_q = Module(new DCEQueue(new PermuteMicroOpWithData, 2))

  vmu_index_q.io.push_data      := vrf.io.vps.rvs2.resp.asTypeOf(Vec(dLenB, UInt(8.W)))
  vmu_index_q.io.push.bits.head := vps.io.iss.bits.eidx << vps.io.iss.bits.rvs2_eew
  vmu_index_q.io.push.bits.tail := Mux(vps.io.iss.bits.tail,
    vps.io.iss.bits.vl << vps.io.iss.bits.rvs2_eew,
    0.U)

  vmu_mask_q.io.push_data       := Mux(vps.io.iss.bits.renvm,
    (vrf.io.vps.rvm.resp >> vps.io.iss.bits.eidx(log2Ceil(dLen)-1,0))(dLenB-1,0),
    ~(0.U(dLenB.W))
  ).asBools
  vmu_mask_q.io.push.bits.head  := 0.U
  vmu_mask_q.io.push.bits.tail  := Mux(vps.io.iss.bits.tail, vps.io.iss.bits.vl, 0.U) - vps.io.iss.bits.eidx


  vps.io.iss.ready := Mux(vps.io.iss.bits.vmu,
    vmu_index_q.io.push.ready && vmu_mask_q.io.push.ready,
    perm_q.io.enq.ready)

  vmu_index_q.io.push.valid := vps.io.iss.valid && vps.io.iss.bits.vmu && vps.io.iss.bits.renv2 && vps.io.iss.ready
  vmu_mask_q.io.push.valid  := vps.io.iss.valid && vps.io.iss.bits.vmu && vps.io.iss.bits.renvm && vps.io.iss.ready

  io.vmu.sdata.valid   := vss.io.iss.valid
  vss.io.iss.ready     := io.vmu.sdata.ready
  io.vmu.sdata.bits.stdata    := vrf.io.vss.rvd.resp
  io.vmu.sdata.bits.stmask    := Mux(vss.io.iss.bits.use_stmask,
    get_vm_mask(vrf.io.vss.rvm.resp, vss.io.iss.bits.eidx, vss.io.iss.bits.elem_size),
    ~(0.U(dLenB.W))
  )
  io.vmu.sdata.bits.debug_id := vss.io.iss.bits.debug_id


  io.vmu.mask_pop   <> vmu_mask_q.io.pop
  io.vmu.mask_data  := vmu_mask_q.io.pop_data
  io.vmu.index_pop  <> vmu_index_q.io.pop
  io.vmu.index_data := vmu_index_q.io.pop_data

  // ==================================
  // Connect Permute-to-Execute path

  perm_q.io.enq.valid := vps.io.iss.valid && !vps.io.iss.bits.vmu
  perm_q.io.enq.bits.viewAsSupertype(new PermuteMicroOp) := vps.io.iss.bits
  perm_q.io.enq.bits.rvs2_data := vrf.io.vps.rvs2.resp

  perm_q.io.deq.ready := perm_buffer.io.push.ready
  perm_buffer.io.push.valid := perm_q.io.deq.valid
  perm_buffer.io.push.bits.head := perm_q.io.deq.bits.eidx << perm_q.io.deq.bits.rvs2_eew
  perm_buffer.io.push.bits.tail := Mux(perm_q.io.deq.bits.tail,
    perm_q.io.deq.bits.vl << perm_q.io.deq.bits.rvs2_eew,
    0.U)
  perm_buffer.io.push_data := perm_q.io.deq.bits.rvs2_data.asTypeOf(Vec(dLenB, UInt(8.W)))

  // Only the first VSU can handle permutations TODO clean this up
  flat_vxs.foreach(_.io.perm.req.ready := false.B)
  flat_vxs.foreach(_.io.perm.data := DontCare)

  val perm_vxs = flat_vxs.filter(_.uses_perm)
  require(perm_vxs.size == 1)
  perm_buffer.io.pop <> perm_vxs.head.io.perm.req
  perm_vxs.head.io.perm.data := perm_buffer.io.pop_data.asUInt

  // =================================
  // Connect ReductionSeq to sequencer

  vrs.io.iss.ready := true.B
  vrs.io.acc_data.ready := false.B
  vrs.io.done := false.B
  for (vxs <- flat_vxs) {
    vxs.io.acc_valid := false.B
    if (vxs.uses_acc) {
      when (vxs.io.vat === vrs.io.vat && vxs.io.busy) {
        vrs.io.acc_data.ready := vxs.io.acc_ready
        vxs.io.acc_valid := vrs.io.acc_data.valid
        when (vxs.io.iss.fire && vxs.io.iss.bits.tail) {
          vrs.io.done := true.B
        }
      }
    }
  }

  // ==============================
  // Connect ReductionSeq to writebacks

  val acc_wbs = flat_vxus.map(_.io.acc_write)
  assert(PopCount(acc_wbs.map(_.valid)) <= 1.U)

  vrs.io.acc_fu_resp.valid := acc_wbs.map(_.valid).orR
  vrs.io.acc_fu_resp.bits := Mux1H(acc_wbs.map(_.valid), acc_wbs.map(_.bits))

  // Clear the age tags
  var r_idx = 0
  def clearVat(fire: Bool, tag: UInt) = {
    assert(r_idx < nRelease)
    io.vat_release(r_idx).valid := fire
    io.vat_release(r_idx).bits := tag
    r_idx += 1
  }

  clearVat(vls.io.iss.fire && vls.io.iss.bits.tail, vls.io.iss.bits.vat)
  clearVat(vss.io.iss.fire && vss.io.iss.bits.tail, vss.io.iss.bits.vat)
  vxs.flatten.foreach(xs => clearVat(xs.io.iss.fire && xs.io.iss.bits.tail, xs.io.iss.bits.vat))

  // Signalling to frontend
  val seq_inflight_wv0 = (allSeqs.map(_.io.seq_hazard).map { h =>
    h.valid && ((h.bits.wintent & ~(0.U(egsPerVReg.W))) =/= 0.U)
  } ++ allIssQs.map(_.io.hazards).flatten.map { h =>
    h.valid && h.bits.wintent(0)
  } ++ vxus.flatten.map(_.io.pipe_hazards).flatten.map { h =>
    h.valid && (h.bits.eg < egsPerVReg.U)
  } ++ vxus.flatten.map(_.io.iter_hazards).flatten.map { h =>
    h.valid && (h.bits.eg < egsPerVReg.U)
  }).orR
  val vdq_inflight_wv0 = vdq.io.peek.map { h =>
    h.valid && h.bits.may_write_v0
  }.orR

  vm_busy := seq_inflight_wv0 || vdq_inflight_wv0
  io.busy := vdq.io.deq.valid || allSeqs.map(_.io.busy).orR || vxus.flatten.map(_.io.busy).asUInt.orR
  io.set_vxsat := vxus.flatten.map(_.io.set_vxsat).asUInt.orR
  io.set_fflags.valid := vxus.flatten.map(_.io.set_fflags.valid).asUInt.orR
  io.set_fflags.bits  := vxus.flatten.map( xu => Mux(xu.io.set_fflags.valid, xu.io.set_fflags.bits, 0.U)).reduce(_|_)

  // Only one of these should actually be connected
  val scalar_write_arb = Module(new Arbiter(new ScalarWrite, flat_vxus.size))
  vxus.flatten.map(_.io.scalar_write).zip(scalar_write_arb.io.in).foreach { case (i,o) => o <> i }
  io.scalar_resp <> scalar_write_arb.io.out
}
