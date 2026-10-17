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
import scala.math._
import saturn.exu._

class OuterProductSequencerIO(implicit p: Parameters) extends SequencerIO(new OuterProductControl) with HasOPUParams {
  val rvs1 = Decoupled(new VectorReadReq)
  val rvs2 = Decoupled(new VectorReadReq)
  // every register-file write, to invalidate cached scales
  val write_snoop = Input(Vec(vParams.vrfBanking, Valid(UInt(log2Ceil(egsTotal).W))))

  val pipe_write_req = new VectorPipeWriteReqIO(yDim+2)

  val tail = Output(Bool())
  val write = Output(Valid(UInt(log2Ceil(egsTotal).W)))
  val write_reg_enable = Output(Bool())
  val wsboard = Output(UInt(egsTotal.W))
}

class OuterProductSequencer(implicit p: Parameters) extends Sequencer[OuterProductControl]()(p) with HasOPUParams {

  val opu_insns = vParams.opuInsns

  def accepts(inst: VectorIssueInst) = !inst.vmu && new VectorDecoder(inst, opu_insns, Nil).matched

  // wsboard (write scoreboard) keeps track of inflight mvouts
  val wsboard = RegInit(0.U(egsTotal.W))
  val wsboard_write = WireInit(0.U(egsTotal.W))
  val wsboard_clear = WireInit(0.U(egsTotal.W))
  wsboard := (wsboard | wsboard_write) & ~wsboard_clear

  val io = IO(new OuterProductSequencerIO)

  // registers for currently handled instruction
  val valid = RegInit(false.B)
  val inst = Reg(new BackendIssueInst)
  val head = Reg(Bool())

  val wvd_mask = Reg(UInt(egsTotal.W))
  val rvs1_mask = Reg(UInt(egsTotal.W))
  val rvs2_mask = Reg(UInt(egsTotal.W))
  val emul = Reg(UInt(2.W))
  val eew = Reg(UInt(2.W))

  val mvin = Reg(Bool())
  val mvin_bcast = Reg(Bool())
  val mvin_col = Reg(Bool())
  val mvout = Reg(Bool())
  val macc = Reg(Bool())
  val macc_fp8 = Reg(Bool())
  val scaled = Reg(Bool()) // OPFMACC.S

  // Scale cache. OPFMACC.S reads its scales from vs1+1 and vs2+1, but the OPU
  // has only two register-file read ports, both busy with data. Scales change
  // once per block, so the last scale registers read (one per entry; a 2x2
  // kernel uses four) are kept in OuterProductUnit and re-read only after a
  // write to them. This sequencer keeps the tags and valid pieces, because it
  // decides the fills: fill cycles read missing pieces through the two data
  // ports before the data reads start, and send them to the OPU's entries.
  val nScaleEntries = opuParams.nScaleEntries
  // an instruction may need two entries; c_ptr wraps by overflowing
  require(nScaleEntries >= 2 && isPow2(nScaleEntries), "nScaleEntries must be a power of two, at least 2")
  val c_alloc = RegInit(VecInit(Seq.fill(nScaleEntries)(false.B)))
  val c_tag   = Reg(Vec(nScaleEntries, UInt(5.W)))
  val c_pv    = RegInit(VecInit(Seq.fill(nScaleEntries)(0.U(egsPerVReg.W)))) // pieces valid
  val c_ptr   = RegInit(0.U(log2Ceil(nScaleEntries).W))
  val eA = Reg(UInt(log2Ceil(nScaleEntries).W)) // entry of vs1+1
  val eB = Reg(UInt(log2Ceil(nScaleEntries).W)) // entry of vs2+1

  // val scalar_row_idx = Cat(
  //   inst.rs1_data(log2Ceil(yDim * vLen/dLen)-1, 0), 
  //   inst.rs1_data(log2Ceil(yDim * vLen/dLen * clusterYdim)-1, log2Ceil(yDim * vLen/dLen)))
  val scalar_row_idx = Cat(
    inst.rs1_data(log2Ceil(yDim)-1, 0), 
    inst.rs1_data(log2Ceil(yDim * clusterYdim * vLen / dLen)-1, log2Ceil(yDim)))
  val scalar_cluster_row_idx = (scalar_row_idx >> log2Ceil(clusterYdim * vLen / dLen))(log2Ceil(yDim)-1,0)
  // row0 takes the longest
  val scalar_row_latency = ((yDim+1).U - scalar_cluster_row_idx)

  // maccs use both col_idx and row_idx, mvins/mvouts use col_idx only
  val col_idx = Reg(UInt(log2Ceil(clusterXdim * maxLMUL * vLen / dLen).W))
  val row_idx = Reg(UInt(log2Ceil(vLen / dLen).W))

  val renv1 = macc
  val renv2 = macc || mvin || mvin_bcast

  val next_col_idx = col_idx +& 1.U
  val next_row_idx = row_idx +& 1.U

  val col_idx_tail = next_col_idx === (Mux(macc, (vLen / dLen).U, (clusterXdim * vLen / dLen).U) << (emul - eew))
  val row_idx_tail = next_row_idx === (vLen / dLen).U

  val macc_tail = col_idx_tail && row_idx_tail

  val tail = Mux(macc, macc_tail, col_idx_tail)

  io.dis.ready := !valid || (tail && io.iss.fire) && !io.dis_stall

  // Take a new instruction
  when (io.dis.fire) {
    val dis_inst = io.dis.bits

    val dis_vd_arch_mask  = get_arch_mask(dis_inst.rd , 0.U)
    val dis_vs1_arch_mask = get_arch_mask(dis_inst.rs1, dis_inst.emul)
    val dis_vs2_arch_mask = get_arch_mask(dis_inst.rs2, dis_inst.emul)

    valid := true.B
    inst := io.dis.bits
    wvd_mask      := Mux(dis_inst.wvd               , FillInterleaved(egsPerVReg, dis_vd_arch_mask), 0.U)
    rvs1_mask     := Mux(dis_inst.renv1             , FillInterleaved(egsPerVReg, dis_vs1_arch_mask), 0.U)
    rvs2_mask     := Mux(dis_inst.renv2             , FillInterleaved(egsPerVReg, dis_vs2_arch_mask), 0.U)
    eew        := dis_inst.vconfig.vtype.vsew
    val funct6 = OPMFunct6(dis_inst.funct6)
    val fp_funct6 = OPFFunct6(dis_inst.funct6)
    val dis_scaled = fp_funct6 === OPFFunct6.opfmaccs
    // the frontend gives OPFMACC.S EMUL 2 so hazards cover the scale
    // registers; the data are still one register each
    emul        := Mux(dis_scaled, dis_inst.emul - 1.U, dis_inst.emul)
    scaled := dis_scaled
    macc_fp8 :=  fp_funct6 === OPFFunct6.opfmacc || dis_scaled
    macc :=  (funct6 === OPMFunct6.opmacc) || (fp_funct6 === OPFFunct6.opfmacc) || dis_scaled

    // look up both scale registers. A miss takes a free entry first, else a
    // round-robin one, never the entry the other operand uses. A register
    // keeps its entry after a write (only its pieces are dropped), so a
    // kernel using at most four scale registers never evicts.
    // rule A: the scale register of vN is vN+1. The frontend assumes the same
    // (PipelinedFaultCheck gives OPFMACC.S EMUL 2); a change of rule must
    // change both.
    val tA = dis_inst.rs1 | 1.U // scale register of A: vs1+1
    val tB = dis_inst.rs2 | 1.U // scale register of B: vs2+1
    // one bit per entry: the entry holds that register
    val hitA = VecInit((0 until nScaleEntries).map(e => c_alloc(e) && c_tag(e) === tA)).asUInt
    val hitB = VecInit((0 until nScaleEntries).map(e => c_alloc(e) && c_tag(e) === tB)).asUInt
    val bIdx = PriorityEncoder(hitB) // B's entry, if it hits
    val free = ~c_alloc.asUInt // entries never used since reset
    val f0 = PriorityEncoder(free) // lowest free entry
    val f1 = PriorityEncoder(free & ~UIntToOH(f0, nScaleEntries)) // second lowest
    // the entries a miss takes, first and second choice: free ones, else
    // the round-robin pointer and the entry after it
    val cand0 = Mux(free.orR, f0, c_ptr)
    val cand1 = Mux(PopCount(free) >= 2.U, f1, Mux(c_ptr === cand0, c_ptr + 1.U, c_ptr))
    // the entries this instruction uses; a miss skips the other operand's entry
    val newA = Mux(hitA.orR, PriorityEncoder(hitA), Mux(hitB.orR && bIdx === cand0, cand1, cand0))
    val newB = Mux(hitB.orR, bIdx, Mux(tB === tA, newA, Mux(newA === cand0, cand1, cand0)))
    when (dis_scaled) {
      eA := newA
      eB := newB
      when (!hitA.orR) {
        c_alloc(newA) := true.B
        c_tag(newA) := tA
        c_pv(newA) := 0.U
      }
      when (!hitB.orR && tB =/= tA) {
        c_alloc(newB) := true.B
        c_tag(newB) := tB
        c_pv(newB) := 0.U
      }
      when ((!hitA.orR || !hitB.orR) && !free.orR) { c_ptr := c_ptr + 1.U }
    }
    mvin := funct6 === OPMFunct6.opmvin
    mvout :=  funct6 === OPMFunct6.opmvout
    mvin_bcast :=  funct6 === OPMFunct6.opmvinbcast
    mvin_col := dis_inst.rd(4) // MSB indicates column write
    col_idx := 0.U
    row_idx := 0.U
    head := true.B
  } .elsewhen (io.iss.fire) {
    valid := !tail
    head := false.B
  }

  // element group we are writing
  val wvd_eg = ((inst.rd << log2Ceil(egsPerVReg)) +& col_idx)(log2Ceil(egsTotal)-1,0)

  // scale-cache fill: the missing pieces of vs1+1 go through rvs1, those of
  // vs2+1 through rvs2 (when both are the same register, its first two
  // missing pieces)
  val tA = inst.rs1 | 1.U // scale registers, rule A as at dispatch above
  val tB = inst.rs2 | 1.U
  val sameAB = eA === eB // vs1 = vs2: one register, one entry
  // one bit per piece (element group) of the register: still to be read
  val needA = ~c_pv(eA)
  val needB = Mux(sameAB, needA & ~UIntToOH(PriorityEncoder(needA), egsPerVReg), ~c_pv(eB))
  val pA = PriorityEncoder(needA) // the piece read this cycle
  val pB = PriorityEncoder(needB)
  val fillA = valid && scaled && needA.orR // A's scales still incomplete
  val fillB = valid && scaled && needB.orR
  val filling = fillA || fillB // a fill cycle: no data reads, no issue
  // register-file addresses (element groups) of those pieces
  val fill_egA = ((tA << log2Ceil(egsPerVReg)) +& pA)(log2Ceil(egsTotal)-1,0)
  val fill_egB = ((tB << log2Ceil(egsPerVReg)) +& pB)(log2Ceil(egsTotal)-1,0)
  // a mask of the register pieces holding this instruction's scale factors,
  // among the whole register file (one bit per element group, as older_writes)
  val scale_oh = Mux(scaled, FillInterleaved(egsPerVReg, UIntToOH(tA, 32) | UIntToOH(tB, 32)), 0.U)
  // an older instruction will still write those scales: wait before filling
  val scale_hazard = (scale_oh & io.older_writes) =/= 0.U

  // element group we are reading
  io.rvs1.bits.eg := Mux(filling, fill_egA, ((inst.rs1 << log2Ceil(egsPerVReg)) +& row_idx)(log2Ceil(egsTotal)-1,0))
  io.rvs2.bits.eg := Mux(filling, fill_egB, ((inst.rs2 << log2Ceil(egsPerVReg)) +& col_idx)(log2Ceil(egsTotal)-1,0))

  io.rvs1.valid := Mux(filling, fillA && !scale_hazard, valid && renv1)
  io.rvs2.valid := Mux(filling, fillB && !scale_hazard, valid && renv2)

  // cache updates: a write to a cached register drops it. A write and a fill
  // of the same register in one cycle cannot happen (fills wait for older
  // writes, younger writes wait for this instruction); the write wins anyway,
  // so a broken ordering costs a refill rather than stale scales.
  for (e <- 0 until nScaleEntries) {
    // a register-file bank writes this entry's register this cycle
    val snooped = io.write_snoop.map(w => w.valid && (w.bits >> log2Ceil(egsPerVReg)) === c_tag(e)).reduce(_||_)
    // a piece of this entry's register is read for it this cycle
    val setA = filling && io.rvs1.fire && eA === e.U
    val setB = filling && io.rvs2.fire && eB === e.U
    when (snooped) {
      c_pv(e) := 0.U
    } .elsewhen (setA || setB) {
      c_pv(e) := c_pv(e) | Mux(setA, UIntToOH(pA, egsPerVReg), 0.U) | Mux(setB, UIntToOH(pB, egsPerVReg), 0.U)
    }
  }

  val oldest = inst.vat === io.vat_head
  io.rvs1.bits.oldest := oldest
  io.rvs2.bits.oldest := oldest

  // report hazards
  io.vat := inst.vat
  io.seq_hazard.valid := valid
  io.seq_hazard.bits.rintent := hazardMultiply(rvs1_mask | rvs2_mask)
  io.seq_hazard.bits.wintent := hazardMultiply(wvd_mask)
  io.seq_hazard.bits.vat := inst.vat
  io.wsboard := wsboard

  val vs1_read_oh = Mux(renv1   , UIntToOH(io.rvs1.bits.eg), 0.U)
  val vs2_read_oh = Mux(renv2   , UIntToOH(io.rvs2.bits.eg), 0.U)
  val vd_write_oh = Mux(mvout   , UIntToOH(wvd_eg), 0.U)

  val raw_hazard = ((vs1_read_oh | vs2_read_oh) & io.older_writes) =/= 0.U || scale_hazard
  val waw_hazard = (vd_write_oh & io.older_writes) =/= 0.U
  val war_hazard = (vd_write_oh & io.older_reads) =/= 0.U
  val data_hazard = raw_hazard || waw_hazard || war_hazard

  // this avoids write-structural-conflicts from the OPU
  val exu_scheduler = Module(new PipeScheduler(1, yDim+2))
  exu_scheduler.io.reqs(0).request := valid && mvout
  exu_scheduler.io.reqs(0).fire := io.iss.fire
  exu_scheduler.io.reqs(0).depth := scalar_row_latency

  // this avoids write-structural-hazards on bank ports with other FUs (maybe)
  io.pipe_write_req.request := valid && mvout && exu_scheduler.io.reqs(0).available
  io.pipe_write_req.bank_sel := (if (vrfBankBits == 0) 1.U else UIntToOH(wvd_eg(vrfBankBits,1)))
  io.pipe_write_req.pipe_depth := scalar_row_latency
  io.pipe_write_req.oldest := oldest
  io.pipe_write_req.fire := io.iss.fire

  val iss_valid = (valid &&
    !filling &&
    !data_hazard &&
    !(renv1 && !io.rvs1.ready) &&
    !(renv2 && !io.rvs2.ready) &&
    !(mvout && !io.pipe_write_req.available) &&
    !(mvout && !exu_scheduler.io.reqs(0).available)
  )

  io.iss.valid := iss_valid
  io.iss.bits.in_l := DontCare // set in Backend
  io.iss.bits.in_t := DontCare
  // scales: on a fill cycle, which entries and pieces the read data go to;
  // on an issue cycle, which ones the OPU applies (2^0 unless scaled)
  io.iss.bits.scaled := io.iss.fire && scaled
  io.iss.bits.sa_entry := eA
  io.iss.bits.sb_entry := eB
  io.iss.bits.sa_piece := Mux(filling, pA, row_idx)
  io.iss.bits.sb_piece := Mux(filling, pB, col_idx)
  io.iss.bits.sa_fill  := filling && io.rvs1.fire
  io.iss.bits.sb_fill  := filling && io.rvs2.fire

  // set the control signals
  val mrf_eg = Mux(mvout, inst.rs2 , inst.rd) +& Mux(macc,  
    (col_idx >> log2Ceil(vLen / dLen)),
    (col_idx >> log2Ceil(clusterXdim * vLen / dLen))  
  )
  val mrf_row_idx = Mux(macc,
    row_idx,
    scalar_row_idx >> log2Ceil(clusterYdim),
  )(log2Ceil(vLen / dLen)-1,0)
  val mrf_col_idx = Mux(macc,
    col_idx,
    col_idx >> log2Ceil(clusterXdim)
  )(log2Ceil(vLen / dLen)-1,0)
  // mvout_pipe tracks the inflight write destinations
  val mvout_pipe = Reg(Vec(yDim+2, UInt(log2Ceil(egsTotal).W)))
  val mvout_valids = RegInit(0.U((yDim+2).W))

  // high bit is the tile-sel, then the quadrant sel (mrf_row_idx, mrf_col_idx)
  io.iss.bits.mrf_idx.foreach(_ := Mux(io.iss.fire, Cat(
    mrf_eg,
    Mux(mvin_col, mrf_col_idx, mrf_row_idx),
    Mux(mvin_col, mrf_row_idx, mrf_col_idx)
  ), 0.U))
  io.iss.bits.row_idx.foreach(_ := Mux(io.iss.fire, scalar_row_idx, 0.U))
  io.iss.bits.col_idx.foreach(_ := Mux(io.iss.fire, col_idx, 0.U))
  io.iss.bits.macc.foreach(_ := io.iss.fire && macc)
  io.iss.bits.mvin_bcast.foreach(_ := io.iss.fire && mvin_bcast)
  if (vParams.useMxOPU) {
    for (i <- 0 until yDim) {
      io.iss.bits.fp8.get(i) := io.iss.fire && macc_fp8
      io.iss.bits.altfmt.get(i) := inst.vconfig.vtype.altfmt
    }
  }
  io.iss.bits.mvin_bcast.foreach(_ := io.iss.fire && mvin_bcast && !mvin_col)
  io.iss.bits.mvin_bcast_col.foreach(_ := io.iss.fire && mvin_bcast && mvin_col)
  io.iss.bits.clock_enable := valid || mvout_valids =/= 0.U

  // for a non-bcast mvin, only the specific row of clusters gets mvin set
  for (i <- 0 until yDim) {
    io.iss.bits.mvin(i) := io.iss.fire && mvin && !mvin_col && scalar_cluster_row_idx === i.U
  }
  for (j <- 0 until xDim) {
    io.iss.bits.mvin_col(j) := io.iss.fire && mvin && mvin_col && scalar_cluster_row_idx === j.U
  }
  mvout_valids := (mvout_valids << 1) | ((io.iss.fire && mvout) << scalar_cluster_row_idx)

  // if the row above us has a valid thing being mv'd out, we have to shift that in
  io.iss.bits.shift.foreach(_ := false.B)
  for (i <- 1 until yDim) {
    when (mvout_valids(i-1)) { mvout_pipe(i) := mvout_pipe(i-1) }
    io.iss.bits.shift(i) := mvout_valids(i-1)
  }

  for (i <- 0 until yDim) {
    when (io.iss.fire && mvout && i.U === scalar_cluster_row_idx) {
      mvout_pipe(i) := wvd_eg
    }
  }

  when (mvout_valids(yDim-1)) { mvout_pipe(yDim) := mvout_pipe(yDim-1) }
  when (mvout_valids(yDim)) { mvout_pipe(yDim+1) := mvout_pipe(yDim) }
  // When it leave the mvout pipe, then we do the write
  io.write.valid := mvout_valids(yDim+1)
  io.write.bits := mvout_pipe(yDim+1)
  io.write_reg_enable := mvout_valids(yDim)

  // clear the wsboard when we do a write
  wsboard_clear := (mvout_valids(yDim+1) << mvout_pipe(yDim+1))

  // update counters
  when (io.iss.fire && !tail) {
    when (!macc || row_idx_tail) {
      rvs2_mask := rvs2_mask & ~UIntToOH(io.rvs2.bits.eg)
    }
    rvs1_mask := rvs1_mask & ~UIntToOH(io.rvs1.bits.eg)

    col_idx := next_col_idx
    when (col_idx_tail) {
      col_idx := 0.U
      row_idx := next_row_idx
    }
    when (mvout) {
      wsboard_write := UIntToOH(wvd_eg)
    }
  }

  io.busy := valid
  io.head := head
  io.tail := tail
}


