package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

class VectorIssueInst(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val bits = UInt(32.W)
  val vconfig = new VConfig
  val vstart = UInt(log2Ceil(maxVLMax).W)
  val rs1_data = UInt(xLen.W)
  val rs2_data = UInt(xLen.W)
  val vat = UInt(vParams.vatSz.W)

  def opcode = bits(6,0)
  def mem_size = bits(13,12)
  def mop = bits(27,26)
  def vm = bits(25)
  def umop = bits(24,20)
  def nf = bits(31,29)
  def pos_lmul = Mux(vconfig.vtype.vlmul_sign, 0.U, vconfig.vtype.vlmul_mag)
  def vmu = opcode.isOneOf(opcLoad, opcStore)
  def rs1 = bits(19,15)
  def rs2 = bits(24,20)
  def rd  = bits(11,7)
  def may_write_v0 = rd === 0.U && opcode =/= opcStore
}

class VectorIndexAccessIO(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val ready = Output(Bool())
  val valid = Input(Bool())
  val vrs = Input(UInt(5.W))
  val eidx = Input(UInt(maxVLMax.W))
  val eew = Input(UInt(2.W))
  val idx = Output(UInt(64.W))
}

class VectorBackend(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val issue = Flipped(Decoupled(new VectorIssueInst))

    val vm = Output(UInt(maxVLMax.W))

    val mem = new VectorMemInterface

    val backend_busy = Output(Bool())
    val mem_busy = Output(Bool())
    val vm_busy = Output(Bool())

    val index_access = new VectorIndexAccessIO
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
  vmu.io.enq.valid := vat_available && io.issue.valid   && issue_inst.vmu

  when (io.issue.bits.vconfig.vl <= io.issue.bits.vstart) {
    io.issue.ready := true.B
    vdq.io.enq.valid := false.B
    vmu.io.enq.valid := false.B
  }

  vdq.io.enq.bits := issue_inst
  vmu.io.enq.bits := issue_inst

  val vls = Module(new PipeSequencer(0, (i: VectorIssueInst) => i.vmu && !i.opcode(5),
    true, false, false, false))
  val vss = Module(new PipeSequencer(0, (i: VectorIssueInst) => i.vmu &&  i.opcode(5),
    false, false, false, true))
  val vxs = Module(new PipeSequencer(3, (i: VectorIssueInst) => !i.vmu,
    true, false, false, false))
  val seqs = Seq(vls, vss, vxs)


  vdq.io.deq.ready := seqs.map(_.io.dis.ready).andR
  seqs.foreach { s =>
    s.io.dis.valid := vdq.io.deq.valid
    s.io.dis.inst := vdq.io.deq.bits
    s.io.dis.wvd := false.B
    s.io.dis.renv1 := false.B
    s.io.dis.renv2 := false.B
    s.io.dis.renvd := false.B
    s.io.dis.renvm := false.B
    s.io.dis.execmode := execRegular
    when (s.io.vat_release.valid) {
      assert(vat_valids(s.io.vat_release.bits))
      vat_valids(s.io.vat_release.bits) := false.B
    }
    s.io.dis.vs1_eew  := vdq.io.deq.bits.vconfig.vtype.vsew
    s.io.dis.vs2_eew  := vdq.io.deq.bits.vconfig.vtype.vsew
    s.io.dis.vs3_eew  := vdq.io.deq.bits.vconfig.vtype.vsew
    s.io.dis.vd_eew   := vdq.io.deq.bits.vconfig.vtype.vsew
    s.io.dis.incr_eew := vdq.io.deq.bits.vconfig.vtype.vsew
  }

  vls.io.dis.wvd := true.B
  vls.io.dis.renvm := !vdq.io.deq.bits.vm
  when (!(vdq.io.deq.bits.mop === mopUnit && vdq.io.deq.bits.vstart === 0.U && vdq.io.deq.bits.vm)) {
    vls.io.dis.execmode := execElementOrder
  }
  when (!vdq.io.deq.bits.mop(0)) {
    vls.io.dis.vd_eew := vdq.io.deq.bits.mem_size
    vls.io.dis.incr_eew := vdq.io.deq.bits.mem_size
  }

  vss.io.dis.renvd := true.B
  vss.io.dis.renvm := vdq.io.deq.bits.vm
  when (!(vdq.io.deq.bits.mop === mopUnit && vdq.io.deq.bits.vstart === 0.U && vdq.io.deq.bits.vm)) {
    vss.io.dis.execmode := execElementOrder
  }
  when (!(vdq.io.deq.bits.mop(0))) {
    vss.io.dis.vs3_eew := vdq.io.deq.bits.mem_size
    vss.io.dis.incr_eew := vdq.io.deq.bits.mem_size
  }

  for ((seq, i) <- seqs.zipWithIndex) {
    val otherSeqs = seqs.zipWithIndex.filter(_._2 != i).map(_._1)
    val older_wintents = otherSeqs.map { s =>
      Mux(vatOlder(s.io.seq_hazards.vat, seq.io.seq_hazards.vat) && s.io.seq_hazards.valid,
        s.io.seq_hazards.wintent, 0.U)
    }.reduce(_|_)
    val older_rintents = (otherSeqs.map { s =>
      Mux(vatOlder(s.io.seq_hazards.vat, seq.io.seq_hazards.vat) && s.io.seq_hazards.valid,
        s.io.seq_hazards.rintent, 0.U)
    } :+ Mux(vatOlder(vmu.io.vm_hazard.vat, seq.io.seq_hazards.vat) && vmu.io.vm_hazard.valid,
      ~(0.U(egsPerVReg.W)), 0.U)
    ).reduce(_|_)
    val older_pipe_writes = (otherSeqs.map(_.io.pipe_hazards).flatten.map(h =>
      Mux(vatOlder(h.bits.vat, seq.io.seq_hazards.vat) && h.valid, UIntToOH(h.bits.eg), 0.U)) :+ 0.U).reduce(_|_)

    seq.io.seq_hazards.writes := older_pipe_writes | older_wintents
    seq.io.seq_hazards.reads := older_rintents
  }
  vmu.io.vm_hazard.hazard := seqs.map { s =>
    val same = s.io.seq_hazards.vat === vmu.io.vm_hazard.vat
    val older = vatOlder(s.io.seq_hazards.vat, vmu.io.vm_hazard.vat)
    !same && older && s.io.seq_hazards.valid && s.io.seq_hazards.wintent(egsPerVReg-1,0) =/= 0.U
  }.orR || seqs.map(_.io.pipe_hazards).flatten.map { h =>
    val same = h.bits.vat === vmu.io.vm_hazard.vat
    val older = vatOlder(h.bits.vat, vmu.io.vm_hazard.vat)
    !same && older && h.valid && h.bits.eg < egsPerVReg.U
  }.orR

  val vrf = Mem(egsTotal, Vec(dLenB, UInt(8.W)))
  val vmf = Reg(Vec(egsPerVReg, Vec(dLenB, UInt(8.W))))

  def vrfWrite(eg: UInt, data: UInt, mask: UInt) {
    val wdata = data.asTypeOf(Vec(dLenB, UInt(8.W)))
    val wmask = mask.asBools
    vrf.write(eg, wdata, wmask)
    when (eg < egsPerVReg.U) {
      for (i <- 0 until dLenB) when (mask(i)) { vmf(eg)(i) := wdata(i) }
    }
  }

  vmu.io.load.ready := vls.io.iss.valid
  vls.io.iss.ready := vmu.io.load.valid
  vxs.io.iss.ready := false.B

  val resetting = RegInit(true.B)
  val reset_ctr = RegInit(0.U(log2Ceil(egsTotal).W))
  when (resetting) {
    reset_ctr := reset_ctr + 1.U
    io.issue.ready := false.B
  }
  when (~reset_ctr === 0.U) { resetting := false.B }

  when (vls.io.iss.fire || resetting) {
    val eg = Wire(UInt(log2Ceil(egsTotal).W))
    val data = Wire(UInt(dLen.W))
    val mask = Wire(UInt(dLenB.W))
    when (resetting) {
      eg := reset_ctr
      data := 0.U
      mask := ~(0.U(dLenB.W))
    } .otherwise {
      eg := vls.io.iss.bits.wvd_eg
      data := vmu.io.load.bits
      mask := vls.io.iss.bits.wmask

      when (!vls.io.iss.bits.inst.vm &&
        (vmf.asUInt & UIntToOH(vls.io.iss.bits.eidx)) === 0.U) {
        mask := 0.U(dLenB.W)
      }
    }
    vrfWrite(eg, data, mask)
  }

  vss.io.iss.ready := vmu.io.vstdata.ready


  val vstdata_egid = WireInit(vss.io.iss.bits.rvd_eg)
  io.index_access.ready := !io.backend_busy
  if (vParams.frontendIndexAccess) {
    when (io.index_access.valid && io.index_access.ready) {
      vstdata_egid := getEgId(io.index_access.vrs, io.index_access.eidx, io.index_access.eew)
    }
  }
  val vstdata_rdata = vrf.read(vstdata_egid).asUInt
  io.index_access.idx := vstdata_rdata >> ((io.index_access.eidx << io.index_access.eew)(log2Ceil(dLenB)-1,0) << 3)
  vmu.io.vstdata.valid := vss.io.iss.valid
  vmu.io.vstdata.bits.data := vstdata_rdata
  vmu.io.vstdata.bits.mask := vss.io.iss.bits.wmask
  vmu.io.vm := vmf.asUInt


  val seq_inflight_wv0 = (seqs.map(_.io.seq_hazards).map { h =>
    h.valid && ((h.wintent & ~(0.U(egsPerVReg.W))) =/= 0.U)
  } ++ seqs.map(_.io.pipe_hazards).flatten.map { h =>
    h.valid && (h.bits.eg < egsPerVReg.U)
  }).orR
  val vdq_inflight_wv0 = vdq.io.peek.map { h =>
    h.valid && h.bits.may_write_v0
  }.orR


  io.mem_busy := vmu.io.busy
  io.vm := vmf.asUInt
  io.vm_busy := seq_inflight_wv0 || vdq_inflight_wv0
  io.backend_busy := vdq.io.deq.valid || resetting
}
