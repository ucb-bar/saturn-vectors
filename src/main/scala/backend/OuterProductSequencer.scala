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

  val pipe_write_req = new VectorPipeWriteReqIO(yDim+2)

  val tail = Output(Bool())
  val write = Output(Valid(UInt(log2Ceil(egsTotal).W)))
  val write_mask = Output(UInt(dLen.W))
  val write_reg_enable = Output(Bool())
  val write_col = Output(Bool()) // the readout captured at write_reg_enable came from the column pipe
  val wsboard = Output(UInt(egsTotal.W))
}

/** Sequences the Xsfmm v0.6.6 subset (RISC-V VME stand-in) onto the outer-product array.
  *
  *  sf.mm.{u,s}.{u,s} / sf.mm.{e4m3,e5m2}.{e4m3,e5m2} mtd, vs2, vs1   (vtype e8, w4)
  *     C[tm,tn] += A[tk,tm]^T * B[tk,tn]; A row k in vs2+2k, B row k in vs1+2k.
  *     Loop order: k, then row sub-tile, then column sub-tile. Sub-tiles outside
  *     tm x tn are skipped; elements outside it are write-disabled.
  *  sf.vtzero.t mtd                                                    (any matrix vtype)
  *  sf.vtmv.t.v rs1=TSS, vs2  /  sf.vtmv.v.t vd, rs1=TSS               (vtype e32, w1)
  *     TSS[30:29] = tile, TSS[24] = pattern (0 row, 1 column), TSS[23:0] = index.
  *  sf.vtdiscard                                                       (no-op)
  */
class OuterProductSequencer(implicit p: Parameters) extends Sequencer[OuterProductControl]()(p) with HasOPUParams {

  val opu_insns = vParams.opuInsns

  def accepts(inst: VectorIssueInst) = !inst.vmu && new VectorDecoder(inst, opu_insns, Nil).matched

  def idxW(n: Int) = log2Ceil(n) max 1
  // low log2(R) bits of a sub-tile index (empty when VLEN == DLEN)
  def subBits(x: UInt): UInt = if (vLen > dLen) x(log2Ceil(vLen / dLen)-1, 0) else 0.U(0.W)
  val R = vLen / dLen                    // element groups per vector register
  val S = subTileEdge                    // sub-tile edge in elements (DLEN/8)
  val TE = opuTE                         // tile edge (VLEN/8)
  val moveEgs = TE * opuParams.cWidth / dLen // element groups per 32-bit tile row/column
  val tssIdxBits = log2Ceil(TE)
  require(R >= 1 && isPow2(R))
  require(TE == S * R)

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

  val mm      = Reg(Bool())
  val mm_fp8  = Reg(Bool())
  val zero    = Reg(Bool())
  val mvin    = Reg(Bool())
  val mvout   = Reg(Bool())
  val nop     = Reg(Bool())   // sf.vtdiscard, or zero-sized operation
  val col     = Reg(Bool())   // move uses the column pattern
  val tile    = Reg(UInt(log2Ceil(opuParams.nMrfRegs).W))
  val tss_idx = Reg(UInt(tssIdxBits.W))
  val signed_l = Reg(Bool())
  val signed_t = Reg(Bool())
  val e5m2_l   = Reg(Bool())
  val e5m2_t   = Reg(Bool())

  val k_idx    = Reg(UInt(2.W))
  val k_last   = Reg(UInt(2.W))
  val row_idx  = Reg(UInt(idxW(R).W))
  val row_last = Reg(UInt(idxW(R).W))
  val col_idx  = Reg(UInt(idxW(moveEgs).W))
  val col_last = Reg(UInt(idxW(moveEgs).W))

  val col_tail = col_idx === col_last
  val row_tail = row_idx === row_last
  val k_tail   = k_idx === k_last
  val tail = nop || Mux(mm, col_tail && row_tail && k_tail, Mux(zero, col_tail && row_tail, col_tail))

  io.dis.ready := !valid || (tail && io.iss.fire) && !io.dis_stall

  // Registers holding operand rows of an sf.mm (tk rows, 8/KMAX = 2 registers apart)
  def mmRowsMask(base: UInt, rows: UInt): UInt = {
    val arch = (0 until 4).map { k => Mux(k.U < rows, UIntToOH((base +& (2*k).U)(4,0), 32), 0.U(32.W)) }.reduce(_|_)
    FillInterleaved(egsPerVReg, arch)
  }

  // Take a new instruction
  when (io.dis.fire) {
    val dis_inst = io.dis.bits
    val dis_ctrl = new VectorDecoder(dis_inst, opu_insns, Seq(OPUKind))
    val kind = dis_ctrl.uint(OPUKind)
    val tss = dis_inst.rs1_data
    val tm = dis_inst.vconfig.vtype.tm
    val tn = dis_inst.vconfig.vl
    val tk = dis_inst.vconfig.vtype.tk

    val d_mm    = kind === OPUKinds.MM_INT.U || kind === OPUKinds.MM_FP8.U
    val d_zero  = kind === OPUKinds.ZERO.U
    val d_mvin  = kind === OPUKinds.MV_T_V.U
    val d_mvout = kind === OPUKinds.MV_V_T.U
    val d_move  = d_mvin || d_mvout
    val d_empty = Mux(d_mm, tm === 0.U || tn === 0.U || tk === 0.U,
                  Mux(d_zero, tm === 0.U || tn === 0.U,
                  Mux(d_move, tn === 0.U, true.B)))

    valid := true.B
    inst := dis_inst
    mm := d_mm
    mm_fp8 := kind === OPUKinds.MM_FP8.U
    zero := d_zero
    mvin := d_mvin
    mvout := d_mvout
    nop := d_empty
    col := d_move && tss(24)
    // sf.mm / sf.vtzero name the tile in rd[4:3]; moves take it from TSS[30:29]
    tile := Mux(d_move, tss(30, 29), dis_inst.rd(4, 3))
    tss_idx := tss(tssIdxBits-1, 0)
    // sf.mm.<a>.<b>: a = funct6[0] describes vs2 (A), b = inst[7] = rd[0] describes vs1 (B)
    signed_l := dis_inst.funct6(0)
    signed_t := dis_inst.rd(0)
    e5m2_l := !dis_inst.funct6(0)
    e5m2_t := !dis_inst.rd(0)

    k_idx := 0.U
    row_idx := 0.U
    col_idx := 0.U
    k_last := (tk - 1.U)(1, 0)
    row_last := ((tm - 1.U) >> log2Ceil(S))
    col_last := Mux(d_move, (tn - 1.U) >> log2Ceil(xDim), (tn - 1.U) >> log2Ceil(S))

    val vd_arch_mask  = get_arch_mask(dis_inst.rd , dis_inst.emul)
    val vs2_arch_mask = get_arch_mask(dis_inst.rs2, dis_inst.emul)
    wvd_mask  := Mux(d_mvout && !d_empty, FillInterleaved(egsPerVReg, vd_arch_mask), 0.U)
    rvs1_mask := Mux(d_mm && !d_empty, mmRowsMask(dis_inst.rs1, tk), 0.U)
    rvs2_mask := Mux(d_empty, 0.U, Mux(d_mm, mmRowsMask(dis_inst.rs2, tk),
                 Mux(d_mvin, FillInterleaved(egsPerVReg, vs2_arch_mask), 0.U)))
    head := true.B
  } .elsewhen (io.iss.fire) {
    valid := !tail
    head := false.B
  }

  val renv1 = mm && !nop
  val renv2 = (mm || mvin) && !nop

  // element groups read/written this cycle
  val mm_vs2 = (inst.rs2 +& (k_idx << 1))(4,0)
  val mm_vs1 = (inst.rs1 +& (k_idx << 1))(4,0)
  val wvd_eg = ((inst.rd << log2Ceil(egsPerVReg)) +& col_idx)(log2Ceil(egsTotal)-1,0)
  io.rvs1.bits.eg := ((mm_vs1 << log2Ceil(egsPerVReg)) +& col_idx)(log2Ceil(egsTotal)-1,0)
  io.rvs2.bits.eg := Mux(mm,
    ((mm_vs2 << log2Ceil(egsPerVReg)) +& row_idx),
    ((inst.rs2 << log2Ceil(egsPerVReg)) +& col_idx))(log2Ceil(egsTotal)-1,0)

  io.rvs1.valid := valid && renv1
  io.rvs2.valid := valid && renv2

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

  val do_mvout = mvout && !nop
  val vs1_read_oh = Mux(renv1   , UIntToOH(io.rvs1.bits.eg), 0.U)
  val vs2_read_oh = Mux(renv2   , UIntToOH(io.rvs2.bits.eg), 0.U)
  val vd_write_oh = Mux(do_mvout, UIntToOH(wvd_eg), 0.U)

  val raw_hazard = ((vs1_read_oh | vs2_read_oh) & io.older_writes) =/= 0.U
  val waw_hazard = (vd_write_oh & io.older_writes) =/= 0.U
  val war_hazard = (vd_write_oh & io.older_reads) =/= 0.U
  val data_hazard = raw_hazard || waw_hazard || war_hazard

  // ----------------------------------------------------------------------
  // Tile-state hazards from the pipelined FP8 FMA. A result issued at cycle t
  // is visible to accesses issued at t+3 or later.
  val mm_entry = Cat(tile, subBits(row_idx), subBits(col_idx))
  val fp8_hist_valid = RegInit(0.U(fp8MaccLatency.W))
  val fp8_hist_entry = Reg(Vec(fp8MaccLatency, UInt(mm_entry.getWidth.W)))
  val fp8_conflict = Mux(mm && mm_fp8 && !nop,
    (0 until fp8MaccLatency).map(i => fp8_hist_valid(i) && fp8_hist_entry(i) === mm_entry).reduce(_||_),
    fp8_hist_valid =/= 0.U)

  // ----------------------------------------------------------------------
  // Move readout. Items travel down the vertical pipe (rows) or across the
  // horizontal pipe (columns); both take (yDim + 1 - position) cycles.
  val row_move_cluster = tss_idx(log2Ceil(yDim)-1, 0)            // tile row -> cluster row
  val col_move_cluster = tss_idx(log2Ceil(xDim)-1, 0)            // tile col -> cluster column
  val move_cluster = Mux(col, col_move_cluster, row_move_cluster)
  val move_latency = ((yDim+1).U - move_cluster)

  // this avoids write-structural-conflicts from the OPU
  val exu_scheduler = Module(new PipeScheduler(1, yDim+2))
  exu_scheduler.io.reqs(0).request := valid && do_mvout
  exu_scheduler.io.reqs(0).fire := io.iss.fire
  exu_scheduler.io.reqs(0).depth := move_latency

  // this avoids write-structural-hazards on bank ports with other FUs (maybe)
  io.pipe_write_req.request := valid && do_mvout && exu_scheduler.io.reqs(0).available
  io.pipe_write_req.bank_sel := (if (vrfBankBits == 0) 1.U else UIntToOH(wvd_eg(vrfBankBits,1)))
  io.pipe_write_req.pipe_depth := move_latency
  io.pipe_write_req.oldest := oldest
  io.pipe_write_req.fire := io.iss.fire

  val iss_valid = (valid &&
    !data_hazard &&
    !fp8_conflict &&
    !(renv1 && !io.rvs1.ready) &&
    !(renv2 && !io.rvs2.ready) &&
    !(do_mvout && !io.pipe_write_req.available) &&
    !(do_mvout && !exu_scheduler.io.reqs(0).available)
  )

  io.iss.valid := iss_valid
  io.iss.bits.in_l := DontCare // set in Backend
  io.iss.bits.in_t := DontCare

  // ----------------------------------------------------------------------
  // Control signals
  val fire = io.iss.fire
  val do_mm = fire && mm && !nop
  val do_zero = fire && zero && !nop
  val do_mvin = fire && mvin && !nop

  // Row moves: tile row r = sub_r*S + i*yDim + I; iteration col_idx picks
  // cell column (col_idx % clusterXdim) and column sub-tile (col_idx / clusterXdim).
  // Column moves are the transpose.
  val scalar_cell = (tss_idx >> log2Ceil(yDim))(log2Ceil(clusterYdim)-1, 0)
  val scalar_sub  = (tss_idx >> log2Ceil(S))
  val iter_cell   = col_idx(log2Ceil(clusterXdim)-1, 0)
  val iter_sub    = (col_idx >> log2Ceil(clusterXdim))

  val move_row_sub = Mux(col, subBits(iter_sub), subBits(scalar_sub))
  val move_col_sub = Mux(col, subBits(scalar_sub), subBits(iter_sub))
  val mrf_idx = Mux(mm || zero,
    Cat(tile, subBits(row_idx), subBits(col_idx)),
    Cat(tile, move_row_sub, move_col_sub))

  io.iss.bits.mrf_idx.foreach(_ := Mux(fire, mrf_idx, 0.U))
  // Row move: (cell row, cell col) = (scalar, iteration). Column move: row_idx
  // carries the scalar cell column and col_idx the iteration cell row.
  io.iss.bits.row_idx.foreach(_ := Mux(fire, scalar_cell, 0.U))
  io.iss.bits.col_idx.foreach(_ := Mux(fire, iter_cell, 0.U))
  io.iss.bits.macc.foreach(_ := do_mm)
  io.iss.bits.zero.foreach(_ := do_zero)

  // tm/tn write enables for sf.mm and sf.vtzero
  val tm = inst.vconfig.vtype.tm
  val tn = inst.vconfig.vl
  for (i <- 0 until yDim) {
    for (j <- 0 until clusterYdim) {
      io.iss.bits.row_en(i)(j) := ((row_idx << log2Ceil(S)) +& (i + j * yDim).U) < tm
    }
  }
  for (i <- 0 until xDim) {
    for (j <- 0 until clusterXdim) {
      io.iss.bits.col_en(i)(j) := ((col_idx << log2Ceil(S)) +& (i + j * xDim).U) < tn
    }
  }
  // vl enables for moves, per 32-bit element position of the element group
  for (k <- 0 until xDim) {
    io.iss.bits.mv_en(k) := ((col_idx << log2Ceil(xDim)) +& k.U) < tn
  }

  io.iss.bits.signed_l := signed_l
  io.iss.bits.signed_t := signed_t
  if (vParams.useMxOPU) {
    io.iss.bits.fp8.get := mm_fp8
    io.iss.bits.e5m2_l.get := e5m2_l
    io.iss.bits.e5m2_t.get := e5m2_t
    io.iss.bits.rm.get := inst.rm
  }

  // a row move-in only enables the addressed cluster row; a column move-in the addressed cluster column
  for (i <- 0 until yDim) {
    io.iss.bits.mvin(i) := do_mvin && !col && row_move_cluster === i.U
  }
  for (j <- 0 until xDim) {
    io.iss.bits.mvin_col(j) := do_mvin && col && col_move_cluster === j.U
  }

  // ----------------------------------------------------------------------
  // Readout tracking: mvout_pipe(p) holds the destination of the item at pipe position p
  val mvout_pipe = Reg(Vec(yDim+2, UInt(log2Ceil(egsTotal).W)))
  val mvout_col_pipe = Reg(Vec(yDim+2, Bool()))
  val mvout_mask_pipe = Reg(Vec(yDim+2, UInt(xDim.W)))
  val mvout_valids = RegInit(0.U((yDim+2).W))
  mvout_valids := (mvout_valids << 1) | ((fire && do_mvout) << move_cluster)

  io.iss.bits.shift.foreach(_ := false.B)
  io.iss.bits.shift_h.foreach(_ := false.B)
  for (i <- 1 until yDim) {
    when (mvout_valids(i-1)) {
      mvout_pipe(i) := mvout_pipe(i-1)
      mvout_col_pipe(i) := mvout_col_pipe(i-1)
      mvout_mask_pipe(i) := mvout_mask_pipe(i-1)
    }
    io.iss.bits.shift(i) := mvout_valids(i-1) && !mvout_col_pipe(i-1)
    io.iss.bits.shift_h(i) := mvout_valids(i-1) && mvout_col_pipe(i-1)
  }

  for (i <- 0 until yDim) {
    when (fire && do_mvout && i.U === move_cluster) {
      mvout_pipe(i) := wvd_eg
      mvout_col_pipe(i) := col
      mvout_mask_pipe(i) := io.iss.bits.mv_en.asUInt
    }
  }

  for (i <- yDim until yDim+2) {
    when (mvout_valids(i-1)) {
      mvout_pipe(i) := mvout_pipe(i-1)
      mvout_col_pipe(i) := mvout_col_pipe(i-1)
      mvout_mask_pipe(i) := mvout_mask_pipe(i-1)
    }
  }
  // When it leave the mvout pipe, then we do the write
  io.write.valid := mvout_valids(yDim+1)
  io.write.bits := mvout_pipe(yDim+1)
  io.write_mask := FillInterleaved(opuParams.cWidth, mvout_mask_pipe(yDim+1))
  io.write_reg_enable := mvout_valids(yDim)
  io.write_col := mvout_col_pipe(yDim)

  // clear the wsboard when we do a write
  wsboard_clear := (mvout_valids(yDim+1) << mvout_pipe(yDim+1))

  // FP8 in-flight history
  fp8_hist_valid := (fp8_hist_valid << 1) | (do_mm && mm_fp8)
  fp8_hist_entry(0) := mm_entry
  for (i <- 1 until fp8MaccLatency) { fp8_hist_entry(i) := fp8_hist_entry(i-1) }

  // keep the array clocked while readouts or FP8 accumulations are in flight
  io.iss.bits.clock_enable := valid || mvout_valids =/= 0.U || fp8_hist_valid =/= 0.U

  // update counters and release operands (not on the tail, where a new
  // instruction may be dispatched in the same cycle)
  when (fire && !tail) {
    col_idx := col_idx + 1.U
    when (mm || zero) {
      when (col_tail) {
        col_idx := 0.U
        row_idx := row_idx + 1.U
        when (row_tail) {
          row_idx := 0.U
          k_idx := k_idx + 1.U
        }
      }
    }
    // release operand rows once their k-step is done
    when (mm && col_tail && row_tail) {
      val done = FillInterleaved(egsPerVReg, UIntToOH(mm_vs1, 32)) | FillInterleaved(egsPerVReg, UIntToOH(mm_vs2, 32))
      rvs1_mask := rvs1_mask & ~done
      rvs2_mask := rvs2_mask & ~done
    }
    when (mvin) {
      rvs2_mask := rvs2_mask & ~UIntToOH(io.rvs2.bits.eg)
    }
    when (do_mvout) {
      wvd_mask := wvd_mask & ~UIntToOH(wvd_eg)
    }
  }
  // every in-flight readout, including the last one, is scoreboarded until written
  when (fire && do_mvout) {
    wsboard_write := UIntToOH(wvd_eg)
  }

  io.busy := valid
  io.head := head
  io.tail := tail
}
