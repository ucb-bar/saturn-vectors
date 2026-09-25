package saturn.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import chisel3.util.experimental.decode._
import saturn.common._
import saturn.backend._
import hardfloat._
import scala.math._

// Declare supported types (dictates multipler instantiated)
object OPUTypes extends Enumeration { val INT32, INT16, INT8, E4M3, E5M2 = Value }

// Parameters for configured OPU
case class OPUParameters (
  val aWidth : Int = 8,
  val bWidth : Int = 8,
  val cWidth : Int = 32, // Accumulator size

  val nMrfRegs : Int = 4 // TEW=32 tiles (Xsfmm mt0, mt4, mt8, mt12)
)

// Geometry (VLEN = vLen, DLEN = dLen, R = VLEN/DLEN):
//  - The array has (DLEN/8) x (DLEN/8) cells, grouped into 4x4 clusters.
//  - A tile is TE x TE 32-bit accumulators with TE = VLEN/8. Each cell holds
//    R x R entries ("sub-tiles") of each of the 4 tiles.
//  - Tile row r = sub*S + i*yDim + I, where S = DLEN/8 is the sub-tile edge,
//    I the cluster row and i the cell row inside the cluster (columns alike).
trait HasOPUParams extends HasVectorParams { this: HasCoreParameters =>
  def regsPerTileReg = (vLen/dLen) * (vLen/dLen)
  def regsPerCell = regsPerTileReg * opuParams.nMrfRegs
  def cellRegIdxBits = log2Ceil(regsPerCell)
  def prodWidth = opuParams.aWidth + opuParams.bWidth

  def clusterXdim = opuParams.cWidth / opuParams.bWidth
  def clusterYdim = opuParams.cWidth / opuParams.aWidth

  def yDim = (dLen / opuParams.aWidth) / clusterYdim
  def xDim = (dLen / opuParams.bWidth) / clusterXdim

  def subTileEdge = dLen / opuParams.aWidth
  def fp8MaccLatency = 2
}

class OuterProductCell(implicit p: Parameters) extends CoreModule()(p) with HasOPUParams {

  val io = IO(new Bundle{
    // Data signals
    val in_l = Input(UInt(opuParams.aWidth.W)) // left input  (row operand, A = vs2)
    val in_t = Input(UInt(opuParams.bWidth.W)) // top input   (column operand, B = vs1)

    // Contol Signals
    val signed_l = Input(Bool()) // int8: A operand is signed
    val signed_t = Input(Bool()) // int8: B operand is signed
    val fp8 = if (useMxOPU) Some(Input(Bool())) else None     // FP8 outer product
    val e5m2_l = if (useMxOPU) Some(Input(Bool())) else None  // A operand is E5M2 (else E4M3)
    val e5m2_t = if (useMxOPU) Some(Input(Bool())) else None  // B operand is E5M2 (else E4M3)
    val rm = if (useMxOPU) Some(Input(UInt(3.W))) else None   // frm for the FP32 accumulate
    val mrf_idx = Input(UInt(cellRegIdxBits.W)) // Index for µarch register to access
    val macc = Input(Bool())     // qualified by tm/tn enables
    val zero = Input(Bool())     // qualified by tm/tn enables
    val mvin = Input(Bool())     // qualified by cell position and vl
    val mvin_data = Input(UInt(opuParams.cWidth.W))
    val out = Output(UInt(opuParams.cWidth.W))
  })
  // Matrix Register + Logic
  val regs = Reg(Vec(regsPerCell, UInt(opuParams.cWidth.W)))

  val is_fp8 = io.fp8.getOrElse(false.B)
  val int_macc = io.macc && !is_fp8
  val a_ext = Cat(io.signed_l && io.in_l(opuParams.aWidth-1), io.in_l).asSInt
  val b_ext = Cat(io.signed_t && io.in_t(opuParams.bWidth-1), io.in_t).asSInt
  val prod = a_ext * b_ext
  val sum_int = (regs(io.mrf_idx).asSInt + prod).asUInt

  val mrf_idx_pipe = Wire(UInt(cellRegIdxBits.W))
  val fp8_macc_complete = Wire(Bool())
  val sum_fp8 = Wire(UInt(opuParams.cWidth.W))

  if (useMxOPU) {
    def widen(in: UInt, inT: FType, outT: FType, active: Bool): UInt = {
      val widen = Module(new hardfloat.RecFNToRecFN(inT.exp, inT.sig, outT.exp, outT.sig))
      widen.io.in := Mux(active, in, 0.U)
      widen.io.roundingMode := hardfloat.consts.round_near_even
      widen.io.detectTininess := hardfloat.consts.tininess_afterRounding
      widen.io.out
    }
    val f8macc = io.macc && is_fp8
    // FP8 x FP8 products are exact in FP32, so widening before the FMA loses nothing
    val f8a = MXFType.E5M3.recode(fp8ToE5M3(io.in_l, io.e5m2_l.get))
    val f8b = MXFType.E5M3.recode(fp8ToE5M3(io.in_t, io.e5m2_t.get))
    val f8aw = widen(f8a, MXFType.E5M3, FType.S, f8macc)
    val f8bw = widen(f8b, MXFType.E5M3, FType.S, f8macc)
    val fma = Module(new MulAddRecFNPipe(fp8MaccLatency, FType.S.exp, FType.S.sig))
    fma.io.validin := f8macc
    fma.io.op := 0.U // FMA
    fma.io.roundingMode := io.rm.get
    fma.io.detectTininess := hardfloat.consts.tininess_afterRounding
    fma.io.a := f8aw
    fma.io.b := f8bw
    fma.io.c := FType.S.recode(regs(io.mrf_idx))
    // Pipeline the destination index to match FMA latency
    mrf_idx_pipe := Pipe(f8macc, io.mrf_idx, fp8MaccLatency).bits

    sum_fp8 := FType.S.ieee(fma.io.out)
    fp8_macc_complete := fma.io.validout
  } else {
    mrf_idx_pipe := 0.U
    fp8_macc_complete := false.B
    sum_fp8 := 0.U
  }

  // The sequencer never overlaps an in-flight FP8 write with another access to
  // the same entry, nor with any non-FP8 operation.
  for (i <- 0 until regsPerCell) {
    val hit = io.mrf_idx === i.U
    val fp8_hit = fp8_macc_complete && mrf_idx_pipe === i.U
    when (fp8_hit) {
      regs(i) := sum_fp8
    } .elsewhen (hit && int_macc) {
      regs(i) := sum_int
    } .elsewhen (hit && io.zero) {
      regs(i) := 0.U
    } .elsewhen (hit && io.mvin) {
      regs(i) := io.mvin_data
    }
  }
  io.out := regs(io.mrf_idx)
}

class OuterProductCluster(implicit p : Parameters) extends CoreModule()(p) with HasOPUParams {
  val io = IO(new Bundle{
    val in_l      = Input(Vec(clusterYdim, UInt(opuParams.aWidth.W)))
    val in_t      = Input(Vec(clusterXdim, UInt(opuParams.bWidth.W)))

    // Vertical readout pipe (row moves) and horizontal readout pipe (column moves)
    val in_pipe     = Input(UInt(opuParams.cWidth.W))
    val out_pipe    = Output(UInt(opuParams.cWidth.W))
    val in_pipe_h   = Input(UInt(opuParams.cWidth.W))
    val out_pipe_h  = Output(UInt(opuParams.cWidth.W))

    val mrf_idx = Input(UInt(cellRegIdxBits.W))
    val row_idx = Input(UInt(log2Ceil(clusterYdim).W))
    val col_idx = Input(UInt(log2Ceil(clusterXdim).W))

    val macc    = Input(Bool())
    val zero    = Input(Bool())
    val shift   = Input(Bool())
    val shift_h = Input(Bool())
    val mvin     = Input(Bool()) // row move into this cluster row
    val mvin_col = Input(Bool()) // column move into this cluster column
    val row_en = Input(Vec(clusterYdim, Bool())) // tile row < tm
    val col_en = Input(Vec(clusterXdim, Bool())) // tile col < tn
    val mv_en_row = Input(Bool()) // row move: element for this cluster column is < vl
    val mv_en_col = Input(Bool()) // column move: element for this cluster row is < vl

    val signed_l = Input(Bool())
    val signed_t = Input(Bool())
    val fp8    = if (useMxOPU) Some(Input(Bool())) else None
    val e5m2_l = if (useMxOPU) Some(Input(Bool())) else None
    val e5m2_t = if (useMxOPU) Some(Input(Bool())) else None
    val rm     = if (useMxOPU) Some(Input(UInt(3.W))) else None
  })

  // cells(i)(j): i = cell row, j = cell column
  val cells = Seq.fill(clusterYdim, clusterXdim)(Module(new OuterProductCell))
  val cell_outs = Wire(Vec(clusterYdim, Vec(clusterXdim, UInt(opuParams.cWidth.W))))
  val pipe = Reg(UInt(opuParams.cWidth.W))
  val pipe_h = Reg(UInt(opuParams.cWidth.W))

  for (i <- 0 until clusterYdim) {
    for (j <- 0 until clusterXdim) {
      val cell = cells(i)(j)

      cell.io.in_l  := io.in_l(i)
      cell.io.in_t  := io.in_t(j)
      cell.io.mrf_idx := io.mrf_idx
      cell.io.signed_l := io.signed_l
      cell.io.signed_t := io.signed_t
      if (useMxOPU) {
        cell.io.fp8.get := io.fp8.get
        cell.io.e5m2_l.get := io.e5m2_l.get
        cell.io.e5m2_t.get := io.e5m2_t.get
        cell.io.rm.get := io.rm.get
      }
      val en = io.row_en(i) && io.col_en(j)
      cell.io.macc := io.macc && en
      cell.io.zero := io.zero && en
      cell_outs(i)(j) := cell.io.out

      // Row moves select cell (row_idx, col_idx); column moves select the
      // transposed cell (col_idx, row_idx).
      cell.io.mvin := (io.mvin     && i.U === io.row_idx && j.U === io.col_idx && io.mv_en_row) ||
                      (io.mvin_col && j.U === io.row_idx && i.U === io.col_idx && io.mv_en_col)
      cell.io.mvin_data := Mux(io.mvin_col, io.in_l.asUInt, io.in_t.asUInt)
    }
  }

  pipe := Mux(io.shift,
    io.in_pipe,
    cell_outs(io.row_idx)(io.col_idx)
  )
  pipe_h := Mux(io.shift_h,
    io.in_pipe_h,
    cell_outs(io.col_idx)(io.row_idx)
  )

  io.out_pipe := pipe
  io.out_pipe_h := pipe_h
}

class OuterProductControl(implicit p: Parameters) extends CoreBundle()(p) with HasOPUParams {
  val clock_enable = Bool()

  val in_l      = Vec(yDim, Vec(clusterYdim, UInt(opuParams.aWidth.W)))
  val in_t      = Vec(xDim, Vec(clusterXdim, UInt(opuParams.bWidth.W)))

  // same values broadcast horizontally (one per cluster row)
  val mrf_idx    = Vec(yDim, UInt(cellRegIdxBits.W))
  val row_idx    = Vec(yDim, UInt(log2Ceil(clusterYdim).W))
  val col_idx    = Vec(yDim, UInt(log2Ceil(clusterXdim).W))
  val macc       = Vec(yDim, Bool())
  val zero       = Vec(yDim, Bool())
  val shift      = Vec(yDim, Bool())
  val mvin       = Vec(yDim, Bool())
  val row_en     = Vec(yDim, Vec(clusterYdim, Bool()))

  // same values broadcast vertically (one per cluster column)
  val mvin_col   = Vec(xDim, Bool())
  val shift_h    = Vec(xDim, Bool())
  val col_en     = Vec(xDim, Vec(clusterXdim, Bool()))

  // per element position in a DLEN-bit group of 32-bit elements
  val mv_en      = Vec(xDim, Bool())

  // operand formats (whole array)
  val signed_l   = Bool()
  val signed_t   = Bool()
  val fp8        = if (useMxOPU) Some(Bool()) else None
  val e5m2_l     = if (useMxOPU) Some(Bool()) else None
  val e5m2_t     = if (useMxOPU) Some(Bool()) else None
  val rm         = if (useMxOPU) Some(UInt(3.W)) else None
}


class OuterProductUnit(implicit p: Parameters) extends CoreModule()(p) with HasOPUParams {
  require(xDim == yDim && clusterXdim == clusterYdim, "OPU array must be square")

  val io = IO(new Bundle {
    val op = Input(new OuterProductControl)
    val out = Output(Vec(xDim, UInt(opuParams.cWidth.W)))     // row readout, one element per cluster column
    val out_col = Output(Vec(yDim, UInt(opuParams.cWidth.W))) // column readout, one element per cluster row
    val YOU_SHALL_PASS = Output(Bool())
  })

  // clock gating
  val gated_clock = ClockGate(clock, io.op.clock_enable, "opu_clock_gate")

  // Force OuterProductUnit to have logic to be syn-mappable
  io.YOU_SHALL_PASS := io.op.macc(0) & io.op.macc(0) | io.op.shift(0)
  dontTouch(io.YOU_SHALL_PASS)

  val clusters = Seq.fill(yDim, xDim)(withClock(gated_clock) { Module(new OuterProductCluster) })

  for (i <- 0 until yDim) {
    for (j <- 0 until xDim) {
      val cluster = clusters(i)(j)
      // column broadcast signals
      cluster.io.in_t      := io.op.in_t(j)
      cluster.io.mvin_col  := io.op.mvin_col(j)
      cluster.io.shift_h   := io.op.shift_h(j)
      cluster.io.col_en    := io.op.col_en(j)
      cluster.io.mv_en_row := io.op.mv_en(j)
      // row broadcast signals
      cluster.io.in_l      := io.op.in_l(i)
      cluster.io.mrf_idx   := io.op.mrf_idx(i)
      cluster.io.row_idx   := io.op.row_idx(i)
      cluster.io.col_idx   := io.op.col_idx(i)
      cluster.io.macc      := io.op.macc(i)
      cluster.io.zero      := io.op.zero(i)
      cluster.io.shift     := io.op.shift(i)
      cluster.io.mvin      := io.op.mvin(i)
      cluster.io.row_en    := io.op.row_en(i)
      cluster.io.mv_en_col := io.op.mv_en(i)
      // whole-array signals
      cluster.io.signed_l := io.op.signed_l
      cluster.io.signed_t := io.op.signed_t
      if (useMxOPU) {
        cluster.io.fp8.get    := io.op.fp8.get
        cluster.io.e5m2_l.get := io.op.e5m2_l.get
        cluster.io.e5m2_t.get := io.op.e5m2_t.get
        cluster.io.rm.get     := io.op.rm.get
      }
    }
  }

  // vertical readout chain (row moves)
  for (j <- 0 until xDim) {
    clusters(0)(j).io.in_pipe := 0.U
    for (i <- 1 until yDim) {
      clusters(i)(j).io.in_pipe := clusters(i-1)(j).io.out_pipe
    }
    io.out(j) := clusters(yDim-1)(j).io.out_pipe
  }
  // horizontal readout chain (column moves)
  for (i <- 0 until yDim) {
    clusters(i)(0).io.in_pipe_h := 0.U
    for (j <- 1 until xDim) {
      clusters(i)(j).io.in_pipe_h := clusters(i)(j-1).io.out_pipe_h
    }
    io.out_col(i) := clusters(i)(xDim-1).io.out_pipe_h
  }
}
