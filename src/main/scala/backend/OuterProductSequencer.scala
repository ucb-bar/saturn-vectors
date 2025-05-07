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

  val mvin = Reg(Bool())
  val mvin_bcast = Reg(Bool())
  val mvout = Reg(Bool())
  val macc = Reg(Bool())

  val scalar_row_idx = inst.rs1_data
  val scalar_cluster_row_idx = (scalar_row_idx >> log2Ceil(clusterYdim))(log2Ceil(yDim)-1,0)
  // row0 takes the longest
  val scalar_row_latency = ((yDim+1).U - scalar_cluster_row_idx)

  // maccs use both col_idx and row_idx, mvins/mvouts use col_idx only
  val col_idx = Reg(UInt(log2Ceil(wideningFactor * (vLen / dLen)).W))
  val row_idx = Reg(UInt(log2Ceil(vLen / dLen).W))

  val renv1 = macc
  val renv2 = macc || mvin || mvin_bcast

  val next_col_idx = col_idx +& 1.U
  val next_row_idx = row_idx +& 1.U

  val col_idx_tail = next_col_idx === Mux(macc, (vLen / dLen).U, (wideningFactor * vLen / dLen).U)
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
    val funct6 = OPMFunct6(dis_inst.funct6)
    mvin := funct6 === OPMFunct6.opmvin
    mvout :=  funct6 === OPMFunct6.opmvout
    macc :=  funct6 === OPMFunct6.opmacc
    mvin_bcast :=  funct6 === OPMFunct6.opmvinbcast
    col_idx := 0.U
    row_idx := 0.U
    head := true.B
  } .elsewhen (io.iss.fire) {
    valid := !tail
    head := false.B
  }

  val wvd_eg = ((inst.rd << log2Ceil(egsPerVReg)) +& col_idx)(log2Ceil(egsTotal)-1,0)

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

  val raw_hazard = ((vs1_read_oh | vs2_read_oh) & io.older_writes) =/= 0.U
  val waw_hazard = (vd_write_oh & io.older_writes) =/= 0.U
  val war_hazard = (vd_write_oh & io.older_reads) =/= 0.U
  val data_hazard = raw_hazard || waw_hazard || war_hazard

  // element group we are reading
  io.rvs1.bits.eg := ((inst.rs1 << log2Ceil(egsPerVReg)) +& row_idx)(log2Ceil(egsTotal)-1,0)
  io.rvs2.bits.eg := ((inst.rs2 << log2Ceil(egsPerVReg)) +& col_idx)(log2Ceil(egsTotal)-1,0)

  io.rvs1.valid := valid && renv1
  io.rvs2.valid := valid && renv2

  val oldest = inst.vat === io.vat_head
  io.rvs1.bits.oldest := oldest
  io.rvs2.bits.oldest := oldest

  // this avoids write-structural-conflicts from the OPU
  val exu_scheduler = Module(new PipeScheduler(1, yDim+2))
  exu_scheduler.io.reqs(0).request := valid && mvout
  exu_scheduler.io.reqs(0).fire := io.iss.fire
  exu_scheduler.io.reqs(0).depth := scalar_row_latency

  // this avoids write-structural-hazards on bank ports with other FUs (maybe)
  io.pipe_write_req.request := valid && mvout && exu_scheduler.io.reqs(0).available
  io.pipe_write_req.bank_sel := (if (vrfBankBits == 0) 1.U else UIntToOH(wvd_eg(vrfBankBits-1,0)))
  io.pipe_write_req.pipe_depth := scalar_row_latency
  io.pipe_write_req.oldest := oldest
  io.pipe_write_req.fire := io.iss.fire

  val iss_valid = (valid &&
    !data_hazard &&
    !(renv1 && !io.rvs1.ready) &&
    !(renv2 && !io.rvs2.ready) &&
    !(mvout && !io.pipe_write_req.available) &&
    !(mvout && !exu_scheduler.io.reqs(0).available)
  )

  io.iss.valid := iss_valid
  io.iss.bits.in_l := DontCare // set in Backend
  io.iss.bits.in_t := DontCare


  // set the control signals
  val mrf_row_idx = Mux(macc,
    row_idx,
    scalar_row_idx >> log2Ceil(yDim * clusterYdim),
  )(log2Ceil(vLen / dLen)-1,0)
  val mrf_col_idx = Mux(macc,
    col_idx,
    col_idx >> log2Ceil(opuParams.cWidth / opuParams.bWidth)
  )(log2Ceil(vLen / dLen)-1,0)

  // mvout_pipe tracks the inflight write destinations
  val mvout_pipe = Reg(Vec(yDim+2, UInt(log2Ceil(egsTotal).W)))
  val mvout_valids = RegInit(0.U((yDim+2).W))

  // high bit is the tile-sel, then the quadrant sel (mrf_row_idx, mrf_col_idx)
  io.iss.bits.mrf_idx.foreach(_ := Mux(io.iss.fire, Cat(
    Mux(mvout, inst.rs2, inst.rd),
    mrf_row_idx,
    mrf_col_idx
  ), 0.U))
  io.iss.bits.row_idx.foreach(_ := Mux(io.iss.fire, scalar_row_idx, 0.U))
  io.iss.bits.col_idx.foreach(_ := Mux(io.iss.fire, col_idx, 0.U))
  io.iss.bits.macc.foreach(_ := io.iss.fire && macc)
  io.iss.bits.mvin_bcast.foreach(_ := io.iss.fire && mvin_bcast)
  io.iss.bits.clock_enable := valid || mvout_valids =/= 0.U

  // for a non-bcast mvin, only the specific row of clusters gets mvin set
  for (i <- 0 until yDim) {
    io.iss.bits.mvin(i) := io.iss.fire && mvin && scalar_cluster_row_idx === i.U
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


