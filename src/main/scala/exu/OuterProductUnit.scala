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

// FP8 multiply-accumulate into FP32, c + a*b rounded once with rm, in two
// stages like MulAddRecFNPipe(2, 8, 24). A product of two FP8 values has at
// most 8 significant bits and lies within FP32's normal range, so it is exact
// in FP32: the product is formed directly, and one FP32 addition and one
// rounding replace the 24x24 multiply-add.
class FP8MacPipe extends Module {
  val io = IO(new Bundle {
    val validin = Input(Bool())
    val a = Input(UInt(8.W))
    val b = Input(UInt(8.W))
    val e5m2_a = Input(Bool())
    val e5m2_b = Input(Bool())
    val c = Input(UInt(32.W))
    val rm = Input(UInt(3.W))
    val validout = Output(Bool())
    val out = Output(UInt(32.W))
  })
  val S = FType.S
  val E5M3 = MXFType.E5M3

  val a = rawFloatFromFN(E5M3.exp, E5M3.sig, fp8ToE5M3(io.a, io.e5m2_a))
  val b = rawFloatFromFN(E5M3.exp, E5M3.sig, fp8ToE5M3(io.b, io.e5m2_b))
  val sig = a.sig(3, 0) * b.sig(3, 0) // 1.xxx * 1.yyy, in [1, 4)
  val carry = sig(7)
  val prod = Wire(new RawFloat(S.exp, S.sig))
  prod.isNaN := a.isNaN || b.isNaN || (a.isInf && b.isZero) || (a.isZero && b.isInf)
  prod.isInf := (a.isInf || b.isInf) && !prod.isNaN
  prod.isZero := (a.isZero || b.isZero) && !prod.isNaN
  prod.sign := a.sign ^ b.sign
  // raw exponents are biased by 2^expWidth; AddRawFN also reads a zero's
  // exponent and significand, so a zero product gets those of a recoded zero
  prod.sExp := Mux(prod.isZero, 0.S,
    (a.sExp +& b.sExp +& carry.zext +& ((1 << S.exp) - 2 * (1 << E5M3.exp)).S)(S.exp + 1, 0).asSInt)
  prod.sig := Mux(prod.isZero, 0.U, Cat(0.U(1.W), Mux(carry, sig, sig(6, 0) ## 0.U(1.W)), 0.U((S.sig - 8).W)))

  val valid1 = RegNext(io.validin, false.B)
  val prod1 = RegEnable(prod, io.validin)
  val c1 = RegEnable(S.recode(io.c), io.validin)
  val rm1 = RegEnable(io.rm, io.validin)

  val add = Module(new AddRawFN(S.exp, S.sig))
  add.io.subOp := false.B
  add.io.a := rawFloatFromRecFN(S.exp, S.sig, c1)
  add.io.b := prod1
  add.io.roundingMode := rm1

  val valid2 = RegNext(valid1, false.B)
  val sum2 = RegEnable(add.io.rawOut, valid1)
  val invalid2 = RegEnable(add.io.invalidExc, valid1)
  val rm2 = RegEnable(rm1, valid1)

  val round = Module(new RoundRawFNToRecFN(S.exp, S.sig, 0))
  round.io.invalidExc := invalid2
  round.io.infiniteExc := false.B
  round.io.in := sum2
  round.io.roundingMode := rm2
  round.io.detectTininess := consts.tininess_afterRounding
  io.validout := valid2
  io.out := S.ieee(round.io.out)
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
  // Operand isolation: the int8 multiply-add only sees its operands, and the
  // accumulator, while it's doing an int8 MAC (its result is used only then),
  // so FP8 MACs and readouts through regs(mrf_idx) don't toggle it.
  val in_l_int = Mux(int_macc, io.in_l, 0.U)
  val in_t_int = Mux(int_macc, io.in_t, 0.U)
  val a_ext = Cat(io.signed_l && in_l_int(opuParams.aWidth-1), in_l_int).asSInt
  val b_ext = Cat(io.signed_t && in_t_int(opuParams.bWidth-1), in_t_int).asSInt
  val prod = a_ext * b_ext
  val sum_int = (Mux(int_macc, regs(io.mrf_idx), 0.U).asSInt + prod).asUInt

  val mrf_idx_pipe = Wire(UInt(cellRegIdxBits.W))
  val fp8_macc_complete = Wire(Bool())
  val sum_fp8 = Wire(UInt(opuParams.cWidth.W))

  if (useMxOPU) {
    val f8macc = io.macc && is_fp8
    // Operand isolation: the FP8 path only sees its operands and the
    // accumulator while it's doing an FP8 MAC. FP8MacPipe takes a, b and c in
    // the cycle validin (f8macc) is set and registers nothing otherwise, so
    // int8 MACs and readouts through regs(mrf_idx) don't toggle it.
    require(fp8MaccLatency == 2, "FP8MacPipe has two stages")
    val mac = Module(new FP8MacPipe)
    mac.io.validin := f8macc
    mac.io.a := Mux(f8macc, io.in_l, 0.U)
    mac.io.b := Mux(f8macc, io.in_t, 0.U)
    mac.io.e5m2_a := io.e5m2_l.get
    mac.io.e5m2_b := io.e5m2_t.get
    mac.io.c := Mux(f8macc, regs(io.mrf_idx), 0.U)
    mac.io.rm := io.rm.get
    // Pipeline the destination index to match the MAC's latency
    mrf_idx_pipe := Pipe(f8macc, io.mrf_idx, fp8MaccLatency).bits

    sum_fp8 := mac.io.out
    fp8_macc_complete := mac.io.validout
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

    // Vertical readout pipe (row moves) and horizontal readout pipe (column
    // moves, only wired up when useOpuColReadout is set)
    val in_pipe     = Input(UInt(opuParams.cWidth.W))
    val out_pipe    = Output(UInt(opuParams.cWidth.W))
    val in_pipe_h   = if (useOpuColReadout) Some(Input(UInt(opuParams.cWidth.W))) else None
    val out_pipe_h  = if (useOpuColReadout) Some(Output(UInt(opuParams.cWidth.W))) else None

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
  io.out_pipe := pipe

  if (useOpuColReadout) {
    val pipe_h = Reg(UInt(opuParams.cWidth.W))
    pipe_h := Mux(io.shift_h,
      io.in_pipe_h.get,
      cell_outs(io.col_idx)(io.row_idx)
    )
    io.out_pipe_h.get := pipe_h
  }
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
    val out = Output(Vec(xDim, UInt(opuParams.cWidth.W))) // row readout, one element per cluster column
    // column readout, one element per cluster row
    val out_col = if (useOpuColReadout) Some(Output(Vec(yDim, UInt(opuParams.cWidth.W)))) else None
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
  if (useOpuColReadout) {
    for (i <- 0 until yDim) {
      clusters(i)(0).io.in_pipe_h.get := 0.U
      for (j <- 1 until xDim) {
        clusters(i)(j).io.in_pipe_h.get := clusters(i)(j-1).io.out_pipe_h.get
      }
      io.out_col.get(i) := clusters(i)(xDim-1).io.out_pipe_h.get
    }
  }

}
