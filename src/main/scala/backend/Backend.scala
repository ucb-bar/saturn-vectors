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


  val xissParams = vParams.issStructure.generate(vParams)
  val vlissq = Module(new IssueQueue(vParams.vlissqEntries, 1))
  val vsissq = Module(new IssueQueue(vParams.vsissqEntries, 1))
  val vpissq = Module(new IssueQueue(vParams.vpissqEntries, 2)) // permute/reduction
  val vxissqs = xissParams.map(q => Module(new IssueQueue(q.depth, q.seqs.size + 1)).suggestName(s"vxissq_${q.name}")) // +1 hack for opu

  val vxus = xissParams.map(_.seqs.map(s => Module(new ExecutionUnit(s.fus, s.name)).suggestName(s"vxu${s.name}")))
  val flat_vxus = vxus.flatten
  val vopu = Option.when(useOpu) { Module(new OuterProductUnit) }
  val maxPipeDepth = (flat_vxus.map(_.maxPipeDepth) ++ vopu.map(_.yDim + 2)).max


  val vls = Module(new LoadSequencer)
  val vss = Module(new StoreSequencer)
  val vxs = xissParams.map(q => q.seqs.map(s =>
    Module(new ExecuteSequencer(s.insns, maxPipeDepth, s.fus.size)).suggestName(s"vxs${s.name}")
  ))

  val vos = Option.when(useOpu) { Module(new OuterProductSequencer) }
  val all_supported_insns = xissParams.map(_.insns).flatten ++ vos.map(_.opu_insns).getOrElse(Nil)
  val vps = Module(new SpecialSequencer(all_supported_insns))

  val allSeqs = Seq(vls, vss, vps) ++ vxs.flatten ++ vos
  val allIssQs = Seq(vlissq, vsissq, vpissq) ++ vxissqs

  val flat_vxs = vxs.flatten
  require(flat_vxs.size == flat_vxus.size)

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


  // Add OPU Issue group
  val issGroups = Seq(
    IssueGroup(vlissq, Seq(vls)),
    IssueGroup(vsissq, Seq(vss)),
    IssueGroup(vpissq, Seq(vps)),
  ) ++ (vxissqs.zip(vxs).zipWithIndex.map { case ((q, seqs), i) =>
    val s = if (i == 0 && useOpu) (seqs ++ vos) else seqs
    IssueGroup(q, s)
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

  val dis_ctrl = Wire(new VectorDecodedControl(all_supported_insns, Seq(
    Reduction, Wide2VD, Wide2VS2, WritesAsMask,
    ReadsVS1AsMask, ReadsVS2AsMask, ReadsVS1, ReadsVS2, ReadsVD,
    VMBitReadsVM, AlwaysReadsVM, WritesVD, WritesScalar, ScalarToVD0
  ))).decode(vdq.io.deq.bits)

  // Load sequencer
  vlissq.io.enq.bits.nf_log2 := log2_up(vdq.io.deq.bits.nf, 8)
  vlissq.io.enq.bits.renvm := !vdq.io.deq.bits.vm
  vlissq.io.enq.bits.wvd   := true.B

  // Store sequencer
  vsissq.io.enq.bits.nf_log2 := log2_up(vdq.io.deq.bits.nf, 8)
  vsissq.io.enq.bits.renvd := true.B
  vsissq.io.enq.bits.renvm := !vdq.io.deq.bits.vm && vdq.io.deq.bits.mop === mopUnit

  // Permute source sequencer
  vpissq.io.enq.bits.renv1 := !vdq.io.deq.bits.vmu && dis_ctrl.bool(Reduction)
  vpissq.io.enq.bits.renv2 := (vdq.io.deq.bits.mop(0) || (!vdq.io.deq.bits.vmu && !dis_ctrl.bool(Reduction)))
  vpissq.io.enq.bits.renvd := !dis_ctrl.bool(Reduction)
  vpissq.io.enq.bits.renvm := !vdq.io.deq.bits.vm && vdq.io.deq.bits.mop =/= mopUnit && vdq.io.deq.bits.vmu
  vpissq.io.enq.bits.wide_vd := dis_ctrl.bool(Wide2VD) && !vdq.io.deq.bits.vmu
  vpissq.io.enq.bits.rs1_is_rs2 := !vdq.io.deq.bits.vmu && (vdq.io.deq.bits.opif6 === OPIFunct6.rgather || (vdq.io.deq.bits.funct3 === OPIVV && vdq.io.deq.bits.opif6 === OPIFunct6.rgatherei16))

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

  var flat_vxu_id: Int = 0

  val vos_wsboard = vos.map(_.io.wsboard).getOrElse(0.U)
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
      val older_wintents = older_issq_wintents | older_seq_wintents | vos_wsboard

      val older_issq_rintents = FillInterleaved(egsPerVReg, otherIssqs.map { i =>
        i.io.hazards.map(h => Mux(vatOlder(h.bits.vat, vat) && h.valid, h.bits.rintent, 0.U))
      }.flatten.foldLeft(0.U)(_|_))
      val older_seq_rintents = otherSeqs.map { s =>
        Mux(vatOlder(s.io.seq_hazard.bits.vat, vat) && s.io.seq_hazard.valid, s.io.seq_hazard.bits.rintent, 0.U)
      }.reduce(_|_)
      val older_rintents = older_issq_rintents | older_seq_rintents

      val (other_vxus, same_vxu): (Seq[ExecutionUnit], Option[ExecutionUnit]) = seq match {
        case s: ExecuteSequencer => {
          val other_vxus = flat_vxus.zipWithIndex.filter(_._2 != flat_vxu_id).map(_._1)
          val same_vxu = flat_vxus(flat_vxu_id)
          flat_vxu_id += 1
          (other_vxus, Some(same_vxu))
        }
        case _ => (flat_vxus, None)
      }

      // Older writes from adjacent VXUs will always induce WAW/RAW, but older
      // writes from the same VXU may be from the same instruction, and no WAR
      // or RAW is possible
      val older_other_pipe_writes = other_vxus.map(_.io.pipe_hazards.toSeq).flatten.map { h =>
        Mux(h.valid, h.bits.eg_oh, 0.U)
      }.foldLeft(0.U)(_|_)
      val older_same_pipe_writes = same_vxu.map(_.io.pipe_hazards.toSeq.map { h =>
        Mux(h.valid && h.bits.vat =/= vat, h.bits.eg_oh, 0.U)
      }.reduce(_|_)).getOrElse(0.U)
      val older_pipe_writes = older_other_pipe_writes | older_same_pipe_writes

      val older_iter_writes = flat_vxus.map(_.io.iter_hazards.toSeq).flatten.map { h =>
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
    val chosen_seq = PriorityEncoderOH(valid_seqs & ready_seqs)

    group.seqs.zipWithIndex.foreach { case(s, j) =>
      s.io.dis.valid := group.issq.io.deq.valid && chosen_seq(j)
      s.io.dis.bits := group.issq.io.deq.bits.viewAsSupertype(new BackendIssueInst)
    }
    group.issq.io.deq.ready := (valid_seqs & ready_seqs) =/= 0.U
  }

  // ======================================
  // Connect reads to VRF

  val vrf = Module(new RegisterAccess((flat_vxs ++ vos).size, maxPipeDepth))
  vrf.io.vls.rvm.req <> vls.io.rvm
  vrf.io.vss.rvd.req <> vss.io.rvd
  vrf.io.vss.rvm.req <> vss.io.rvm
  vrf.io.vps.rvs2.req <> vps.io.rvs2
  vrf.io.vps.rvm.req <> vps.io.rvm
  vps.io.acc_init_resp := vrf.io.vps.rvs2.resp


  // Connection to VXS -> VRF and VXS <-> VXUS
  for (i <- 0 until flat_vxs.size) {
    val vxs = flat_vxs(i)
    val vxu = flat_vxus(i)

    vrf.io.vxs(i).rvs1.req <> vxs.io.rvs1
    vrf.io.vxs(i).rvs2.req <> vxs.io.rvs2
    vrf.io.vxs(i).rvd.req <> vxs.io.rvd
    vrf.io.vxs(i).rvm.req <> vxs.io.rvm

    val vxs_iss = vxs.io.iss.bits
    val vxu_iss = vxu.io.iss.bits

    val rvs1_data = Mux1H(Seq(
      vxs_iss.use_scalar_rvs1 -> dLenSplat(vxs_iss.scalar, vxs_iss.rvs1_eew),
      vxs_iss.use_normal_rvs1 -> vrf.io.vxs(i).rvs1.resp))
    val rvs2_data = Mux1H(Seq(
      vxs_iss.use_zero_rvs2 -> 0.U,
      vxs_iss.use_slide_rvs2 -> vxs_iss.slide_data,
      vxs_iss.use_normal_rvs2 -> vrf.io.vxs(i).rvs2.resp))
    val rvd_data = vrf.io.vxs(i).rvd.resp

    vxu_iss.rvs1_data := rvs1_data
    vxu_iss.rvs2_data := rvs2_data
    vxu_iss.rvd_data := rvd_data

    val rvs1_elem = extractElem(rvs1_data, vxs_iss.rvs1_eew, vxs_iss.eidx)
    val rvs2_elem = extractElem(rvs2_data, vxs_iss.rvs2_eew, vxs_iss.eidx)
    val rvd_elem = extractElem(rvd_data, vxs_iss.rvd_eew, vxs_iss.eidx)

    vxu_iss.rvs1_elem := rvs1_elem
    vxu_iss.rvs2_elem := rvs2_elem
    vxu_iss.rvd_elem := rvd_elem

    val vm_off    = ((1 << dLenOffBits) - 1).U(log2Ceil(dLen).W)
    val vm_eidx   = (vxs_iss.eidx & ~(vm_off >> vxs_iss.vd_eew))(log2Ceil(dLen)-1,0)
    val vm_resp   = (vrf.io.vxs(i).rvm.resp >> vm_eidx)(dLenB-1,0)
    val vm_mask   = Mux(vxs_iss.use_wmask,
      VecInit.tabulate(4)({ sew => FillInterleaved(1 << sew, vm_resp)(dLenB-1,0) })(vxs_iss.vd_eew),
      ~(0.U(dLenB.W))
    )
    vxu_iss.wmask := vxs_iss.eidx_mask & vm_mask
    vxu_iss.rmask := Mux(vxs_iss.vm, ~(0.U(dLenB.W)), vm_resp)
    vxu_iss.rvm_data := Mux(vxs_iss.vm, ~(0.U(dLen.W)), vrf.io.vxs(i).rvm.resp)

    vxu.io.iss.valid := vxs.io.iss.valid
    vxs.io.iss.ready := vxu.io.iss.ready
    vxu.io.iss.bits.viewAsSupertype(new ExecuteMicroOp(vxu.nFUs)) := vxs.io.iss.bits

    when (vxs_iss.acc) {
      val acc_data = vps.io.acc_data.bits
      when (vxs_iss.acc_fold) {
        val acc_fold_id = vxs_iss.acc_fold_id
        val folded = VecInit.tabulate(log2Ceil(dLenB))(i => acc_data((dLen >> i) - 1, dLen >> (i + 1)))(acc_fold_id)

        vxu_iss.wmask := Mux(vxs_iss.tail, eewByteMask(vxs_iss.vd_eew), ~(0.U(dLenB.W)))
        vxu_iss.rvs1_elem := Mux(vxs_iss.acc_copy, vps.io.acc_init, folded)
        vxu_iss.rvs1_data := Mux(vxs_iss.acc_copy, vps.io.acc_init, folded)
        vxu_iss.rvs1_eew := vxs_iss.vd_eew
        vxu_iss.rvs2_elem := acc_data
        vxu_iss.rvs2_data := acc_data
        vxu_iss.rvs2_eew  := vxs_iss.vd_eew
      } .otherwise {
        when (vxs_iss.acc_ew) {
          val mask_bit = Mux(vxs_iss.use_wmask, (vrf.io.vxs(i).rvm.resp >> vxs_iss.eidx(log2Ceil(dLen)-1,0))(0), true.B)
          vxu_iss.wmask := Mux(mask_bit, eewByteMask(vxs_iss.vd_eew), 0.U)
          vxu_iss.rvs1_elem := acc_data
          vxu_iss.rvs1_data := acc_data
          vxu_iss.rvs1_eew  := vxs_iss.vd_eew
          vxu_iss.rvs2_data := rvs2_elem
        } .otherwise {
          vxu_iss.rvs1_data := acc_data
          vxu_iss.rvs1_eew := vxs_iss.vd_eew
        }
      }
    }
  }

  // outer product vrf reads
  vopu.foreach { vopu =>

    vrf.io.vxs(flat_vxs.size).rvs1.req <> vos.get.io.rvs1
    vrf.io.vxs(flat_vxs.size).rvs2.req <> vos.get.io.rvs2
    vrf.io.vxs(flat_vxs.size).rvm.req.valid := false.B
    vrf.io.vxs(flat_vxs.size).rvm.req.bits := DontCare
    vrf.io.vxs(flat_vxs.size).rvd.req.valid := false.B
    vrf.io.vxs(flat_vxs.size).rvd.req.bits := DontCare
    vos.get.io.iss.ready := true.B

    val vopu_ctrl_reg = Reg(new OuterProductControl)
    vopu_ctrl_reg := vos.get.io.iss.bits
    when (vos.get.io.iss.valid) {
      when (vos.get.io.iss.bits.mvin.orR || vos.get.io.iss.bits.mvin_bcast.head) {
        vopu_ctrl_reg.in_t := vrf.io.vxs(flat_vxs.size).rvs2.resp.asTypeOf(
          Vec(vopu.xDim, Vec(vopu.clusterXdim, UInt(opuParams.bWidth.W)))
        )
      }

      when (vos.get.io.iss.bits.macc.head) {
        vopu_ctrl_reg.in_l := vrf.io.vxs(flat_vxs.size).rvs1.resp.asTypeOf(
          Vec(vopu.yDim, Vec(vopu.clusterYdim, UInt(opuParams.aWidth.W)))
        )

        val elems = vrf.io.vxs(flat_vxs.size).rvs2.resp.asTypeOf(
          Vec(vopu.xDim * vopu.clusterXdim, UInt(opuParams.bWidth.W))
        )
        for (i <- 0 until vopu.xDim) {
          for (j <- 0 until vopu.clusterXdim) {
            vopu_ctrl_reg.in_t(i)(j) := elems(i + j * vopu.xDim)
          }
        }
      }
    }

    vopu.io.op := vopu_ctrl_reg
  }


  val frontend_rindex = Wire(new VectorReadIO)
  val frontend_rmask  = Wire(new VectorReadIO)
  vrf.io.frontend.rindex <> frontend_rindex
  vrf.io.frontend.rmask <> frontend_rmask

  // ============================
  // Connect writes to VRF
  for (i <- 0 until flat_vxs.size) {
    vrf.io.vxs(i).pipe_write_req <> flat_vxs(i).io.pipe_write_req
    vrf.io.pipe_writes(i) <> flat_vxus(i).io.pipe_write
    vrf.io.iter_writes(i) <> flat_vxus(i).io.iter_write
  }


  // Sepecial OPU connection
  vos.foreach { vos =>
    vrf.io.vxs(flat_vxs.size).pipe_write_req <> vos.io.pipe_write_req
    vrf.io.pipe_writes(flat_vxs.size).valid := vos.io.write.valid
    vrf.io.pipe_writes(flat_vxs.size).bits.eg := vos.io.write.bits
    vrf.io.pipe_writes(flat_vxs.size).bits.data := RegEnable(vopu.get.io.out.asUInt, vos.io.write_reg_enable)
    vrf.io.pipe_writes(flat_vxs.size).bits.mask := ~(0.U(dLen.W))

    vrf.io.iter_writes(flat_vxs.size).valid := false.B
    vrf.io.iter_writes(flat_vxs.size).bits := DontCare
  }

  val load_write = Wire(Decoupled(new VectorWrite(dLen)))
  vrf.io.load_write <> load_write


  io.vmu.lresp.ready := vls.io.iss.valid && load_write.ready
  vls.io.iss.ready := io.vmu.lresp.valid && load_write.ready
  load_write.valid := vls.io.iss.valid && io.vmu.lresp.valid
  load_write.bits.eg   := vls.io.iss.bits.wvd_eg
  load_write.bits.data := Fill(dLen / mLen, io.vmu.lresp.bits.data)
  val load_wmask = Mux(vls.io.iss.bits.use_rmask,
    get_vm_mask(vrf.io.vls.rvm.resp, vls.io.iss.bits.eidx, vls.io.iss.bits.elem_size, dLen),
    ~(0.U(dLenB.W)))
  load_write.bits.mask := FillInterleaved(8, vls.io.iss.bits.eidx_wmask & load_wmask)
  when (io.vmu.lresp.fire) {
    assert(io.vmu.lresp.bits.debug_id === vls.io.iss.bits.debug_id)
  }


  // ========================================
  // Connect frontend index/mask access ports

  val index_access_eg = getEgId(io.index_access.vrs, io.index_access.eidx, io.index_access.eew, false.B)
  val index_access_eg_oh = UIntToOH(index_access_eg)
  val index_access_hazard = ((allSeqs.map(_.io.seq_hazard).map { h =>
    h.valid && ((h.bits.wintent & index_access_eg_oh) =/= 0.U)
  } ++ allIssQs.map(_.io.hazards).flatten.map { h =>
    h.valid && h.bits.wintent(io.index_access.vrs)
  } ++ vxus.flatten.map(_.io.pipe_hazards).flatten.map { h =>
    h.valid && h.bits.eg === index_access_eg
  } ++ vxus.flatten.map(_.io.iter_hazards).flatten.map { h =>
    h.valid && h.bits.eg === index_access_eg
  }).orR ||
    vdq.io.peek.map(i => i.valid && !(i.bits.vmu && i.bits.store)).orR
  )
  // TODO: this conservatively assumes a index data hazard against anything in the vdq

  frontend_rindex.req.valid := io.index_access.valid
  io.index_access.ready := frontend_rindex.req.ready && !index_access_hazard
  frontend_rindex.req.bits.eg  := index_access_eg
  frontend_rindex.req.bits.oldest  := false.B
  io.index_access.idx   := frontend_rindex.resp >> ((io.index_access.eidx << io.index_access.eew)(dLenOffBits-1,0) << 3) & eewBitMask(io.index_access.eew)

  val vm_busy = Wire(Bool())
  frontend_rmask.req.valid    := io.mask_access.valid
  frontend_rmask.req.bits.eg  := getEgId(0.U, io.mask_access.eidx, 0.U, true.B)
  frontend_rmask.req.bits.oldest := false.B
  io.mask_access.ready  := frontend_rmask.req.ready && !vm_busy
  io.mask_access.mask   := frontend_rmask.resp >> io.mask_access.eidx(log2Ceil(dLen)-1,0)

  // =====================================
  // Connect VMU index/mask access ports

  val vmu_index_q = Module(new Compactor(dLenB, mLenB, UInt(8.W), false))
  val vmu_mask_q = Module(new Compactor(dLenB, mLenB, Bool(), false))
  val vgu = Module(new GatherUnit)

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
  vmu_mask_q.io.push.bits.tail  := (Mux(vmu_index_q.io.push.bits.tail === 0.U, dLenB.U, vmu_index_q.io.push.bits.tail) - vmu_index_q.io.push.bits.head) >> vps.io.iss.bits.rvs2_eew

  vps.io.iss.ready := Mux(vps.io.iss.bits.vmu,
    vmu_index_q.io.push.ready && vmu_mask_q.io.push.ready,
    vgu.io.vps.ready)

  vmu_index_q.io.push.valid := vps.io.iss.valid && vps.io.iss.bits.vmu && vps.io.iss.bits.renv2 && vps.io.iss.ready
  vmu_mask_q.io.push.valid  := vps.io.iss.valid && vps.io.iss.bits.vmu && vps.io.iss.bits.renvm && vps.io.iss.ready

  io.vmu.sdata.valid   := vss.io.iss.valid
  vss.io.iss.ready     := io.vmu.sdata.ready
  val iss_stdata = if (mLen < dLen) {
    val select = (vss.io.iss.bits.eidx << vss.io.iss.bits.elem_size)(dLenOffBits-1,mLenOffBits)
    vrf.io.vss.rvd.resp.asTypeOf(Vec(dLen/mLen, UInt(mLen.W)))(select)
  } else {
    vrf.io.vss.rvd.resp
  }
  io.vmu.sdata.bits.stdata    := iss_stdata
  io.vmu.sdata.bits.stmask    := vss.io.iss.bits.eidx_mask & Mux(vss.io.iss.bits.use_stmask,
    get_vm_mask(vrf.io.vss.rvm.resp, vss.io.iss.bits.eidx, vss.io.iss.bits.elem_size, mLen),
    ~(0.U(mLenB.W))
  )
  io.vmu.sdata.bits.debug_id := vss.io.iss.bits.debug_id


  io.vmu.mask_pop   <> vmu_mask_q.io.pop
  io.vmu.mask_data  := vmu_mask_q.io.pop_data
  io.vmu.index_pop  <> vmu_index_q.io.pop
  io.vmu.index_data := vmu_index_q.io.pop_data

  // ==================================
  // Connect Permute-to-Execute path

  vgu.io.vps.valid := vps.io.iss.valid && !vps.io.iss.bits.vmu
  vgu.io.vps.bits.viewAsSupertype(new SpecialMicroOp) := vps.io.iss.bits
  vgu.io.vps.bits.rvs2_data := vrf.io.vps.rvs2.resp

  // Only the first VSU can handle permutations TODO clean this up
  flat_vxs.foreach(_.io.vgu.slide_req.ready := false.B)
  flat_vxs.foreach(_.io.vgu.slide_data := DontCare)
  flat_vxs.foreach(_.io.vgu.gather_eidx.valid := false.B)
  flat_vxs.foreach(_.io.vgu.gather_eidx.bits := DontCare)

  val perm_vxs = flat_vxs.filter(_.usesPerm)
  require(perm_vxs.size == 1)
  vgu.io.vxs <> perm_vxs.head.io.vgu

  // =================================
  // Connect ReductionSeq to sequencer

  vps.io.acc_data.ready := false.B
  vps.io.acc_done := false.B
  for (vxs <- flat_vxs) {
    vxs.io.acc_valid := false.B
    if (vxs.usesAcc) {
      when (vxs.io.vat === vps.io.vat && vxs.io.busy) {
        vps.io.acc_data.ready := vxs.io.acc_ready
        vxs.io.acc_valid := vps.io.acc_data.valid
        when (vxs.io.iss.fire && vxs.io.iss.bits.tail) {
          vps.io.acc_done := true.B
        }
      }
    }
  }

  // ==============================
  // Connect ReductionSeq to writebacks

  val acc_wbs = flat_vxus.map(_.io.acc_write)
  assert(PopCount(acc_wbs.map(_.valid)) <= 1.U)

  vps.io.acc_fu_resp.valid := acc_wbs.map(_.valid).orR
  vps.io.acc_fu_resp.bits := Mux1H(acc_wbs.map(_.valid), acc_wbs.map(_.bits))

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
  vos.foreach { vos =>
    clearVat(vos.io.iss.fire && vos.io.tail, vos.io.vat)
  }

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
