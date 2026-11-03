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

  val nMrfRegs : Int = 4,
  val nScaleEntries : Int = 4 // OPFMACC.S scale cache: scale registers held
)

trait HasOPUParams extends HasVectorParams { this: HasCoreParameters =>
  def maxLMUL = 2 // TODO: make this dynamic
  def regsPerTileReg = (vLen/dLen) * (vLen/dLen)
  def regsPerCell = regsPerTileReg * opuParams.nMrfRegs
  def cellRegIdxBits = log2Ceil(regsPerCell) 
  def prodWidth = opuParams.aWidth + opuParams.bWidth

  def clusterXdim = opuParams.cWidth / opuParams.bWidth
  def clusterYdim = opuParams.cWidth / opuParams.aWidth

  def yDim = (dLen / opuParams.aWidth) / clusterYdim
  def xDim = (dLen / opuParams.bWidth) / clusterXdim

}

class OuterProductCell(implicit p: Parameters) extends CoreModule()(p) with HasOPUParams {

  val io = IO(new Bundle{
    // Data signals
    val in_l = Input(SInt(opuParams.aWidth.W)) // left input
    val in_t = Input(SInt(opuParams.bWidth.W)) // top input
    // Block scales for OPFMACC.S, P3109 Binary8p1uf: 0 is zero, 255 is NaN,
    // otherwise 2^(code - 128); 128 (2^0) for every other operation
    val scale_l = Input(UInt(8.W))
    val scale_t = Input(UInt(8.W))

    // Contol Signals
    val altfmt = if (useMxOPU) Some(Input(Bool())) else None // alternate format for outer product
    val fp8 = if (useMxOPU) Some(Input(Bool())) else None // FP8 format for outer product
    val mrf_idx = Input(UInt(cellRegIdxBits.W)) // Index for µarch register to write
    val macc = Input(Bool())
    val mvin = Input(Bool())
    val mvin_bcast = Input(Bool())
    val mvin_bcast_col = Input(Bool())
    val mvin_data = Input(UInt(opuParams.cWidth.W))
    val out = Output(UInt(opuParams.cWidth.W))
  })
  // Matrix Register + Logic
  val regs = Reg(Vec(regsPerCell, UInt(opuParams.cWidth.W)))
  val prod = Mux(io.macc, io.in_l * io.in_t, 0.S)
  val sum_int8 = prod + regs(io.mrf_idx).asSInt

  val int_macc = if (useMxOPU) (io.macc && !io.fp8.get) else io.macc

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
    val f8macc = io.macc && io.fp8.get
    def read8(u: UInt) = vParams.p3109 match {
      case Some(f) => p3109ToE5M3(u, io.altfmt.get, f)
      case None    => fp8ToE5M3(u, io.altfmt.get)
    }
    // A scaled operand can leave FP32's range even when the result does not,
    // so a, b and c are widened to a recoded format with a 10-bit exponent,
    // the scales are added to a's and b's exponents (exact there), and the
    // exact a*b + c is rounded once, to FP32. With both scales 2^0 this is the
    // unscaled OPFMACC: the same exact value, rounded once.
    val wide = FType(FType.S.exp + 2, FType.S.sig)
    def scaleRec(x: UInt, s: UInt, t: FType): UInt = {
      val sign  = x(t.exp + t.sig)
      val exp   = x(t.exp + t.sig - 1, t.sig - 1)
      val fract = x(t.sig - 2, 0)
      val isZero    = exp(t.exp, t.exp - 2) === 0.U
      val isSpecial = exp(t.exp, t.exp - 1).andR // Inf or NaN
      val qnan   = Cat(0.U(1.W), 7.U(3.W), 0.U((t.exp - 2).W), 1.U(1.W), 0.U((t.sig - 2).W))
      val scaled = Cat(sign, (exp.zext + (s.zext - 128.S))(t.exp, 0), fract)
      Mux(s === 255.U || (s === 0.U && isSpecial), qnan,  // NaN scale; 0 * Inf, 0 * NaN
        Mux(s === 0.U, Cat(sign, 0.U((t.exp + t.sig).W)), // zero scale
          Mux(isZero || isSpecial, x, scaled)))
    }
    val f8a = MXFType.E5M3.recode(read8(io.in_l.asUInt))
    val f8b = MXFType.E5M3.recode(read8(io.in_t.asUInt))
    val f8aw = scaleRec(widen(f8a, MXFType.E5M3, wide, f8macc), io.scale_l, wide)
    val f8bw = scaleRec(widen(f8b, MXFType.E5M3, wide, f8macc), io.scale_t, wide)
    val latency = 2
    val fma = Module(new MulAddRecFNPipeUnrounded(latency, wide.exp, wide.sig))
    fma.io.validin := f8macc
    fma.io.op := 0.U // FMA
    fma.io.roundingMode := hardfloat.consts.round_near_even
    fma.io.detectTininess := hardfloat.consts.tininess_afterRounding
    fma.io.a := f8aw
    fma.io.b := f8bw
    fma.io.c := widen(FType.S.recode(regs(io.mrf_idx)), FType.S, wide, f8macc)
    val round = Module(new hardfloat.RoundAnyRawFNToRecFN(wide.exp, wide.sig + 2, FType.S.exp, FType.S.sig, 0))
    round.io.invalidExc := fma.io.invalidExc
    round.io.infiniteExc := false.B
    round.io.in := fma.io.out
    round.io.roundingMode := hardfloat.consts.round_near_even
    round.io.detectTininess := hardfloat.consts.tininess_afterRounding
    // Pipeline control signals to match FMA latency
    mrf_idx_pipe := Pipe(f8macc, io.mrf_idx, latency).bits
    val _fp8_pipe = Pipe(f8macc, io.fp8.get, latency).bits
    val _macc_pipe = Pipe(f8macc, io.macc, latency).bits

    // P3109 has one zero, and encodes it in binary32 as +0 (Interim Report
    // v4.0.3, 4.8.2), where IEEE 754 can give -0; an OCP build keeps IEEE's.
    val sum32 = FType.S.ieee(round.io.out)
    val sum32_out = if (vParams.p3109.isDefined) Mux(sum32(30, 0) === 0.U, 0.U, sum32) else sum32
    sum_fp8 := Mux(fma.io.validout, sum32_out, 0.U)
    fp8_macc_complete := fma.io.validout
  } else {
    mrf_idx_pipe := 0.U
    fp8_macc_complete := false.B
    sum_fp8 := 0.U
  }

  for (i <- 0 until regsPerCell) {
    // Integer MACC: use current control signals
    val tile_match_int      = (io.mrf_idx >> log2Ceil(regsPerTileReg)) === (i >> log2Ceil(regsPerTileReg)).U
    val subtile_match_int   = io.mrf_idx(log2Ceil(regsPerTileReg)-1,0) === (i % regsPerTileReg).U
    
    // MVIN paths: use current control signals
    val bcast_col_match = (io.mrf_idx >> log2Ceil(vLen/dLen)) === (i >> log2Ceil(vLen/dLen)).U
    val bcast_match     = io.mrf_idx(log2Ceil(vLen/dLen)-1,0) === (i % (vLen/dLen)).U
    
    if (useMxOPU) {
      // FP8 MACC: use pipelined control signals
      val tile_match_fp8      = (mrf_idx_pipe >> log2Ceil(regsPerTileReg)) === (i >> log2Ceil(regsPerTileReg)).U
      val subtile_match_fp8   = mrf_idx_pipe(log2Ceil(regsPerTileReg)-1,0) === (i % regsPerTileReg).U
      when ((tile_match_int && int_macc && subtile_match_int) ||
            (tile_match_fp8 && fp8_macc_complete && subtile_match_fp8) ||
            (tile_match_int && io.mvin && subtile_match_int) ||
            (tile_match_int && io.mvin_bcast && bcast_match) ||
            (tile_match_int && io.mvin_bcast_col && bcast_col_match)) {
        regs(i) := Mux(int_macc, sum_int8.asUInt,
                  Mux(fp8_macc_complete, sum_fp8,
                      io.mvin_data))
        }
    } else {
      when ((tile_match_int && int_macc && subtile_match_int) ||
            (tile_match_int && io.mvin && subtile_match_int) ||
            (tile_match_int && io.mvin_bcast && bcast_match) ||
            (tile_match_int && io.mvin_bcast_col && bcast_col_match)) {
      regs(i) := Mux(int_macc, sum_int8.asUInt,
                     io.mvin_data)
      }
    }
  }
  io.out := regs(io.mrf_idx)
}

class OuterProductCluster(implicit p : Parameters) extends CoreModule()(p) with HasOPUParams {
  val io = IO(new Bundle{
    val in_l      = Input(Vec(clusterYdim, UInt(opuParams.aWidth.W)))
    val in_t      = Input(Vec(clusterXdim, UInt(opuParams.bWidth.W)))
    val scale_l   = Input(Vec(clusterYdim, UInt(8.W)))
    val scale_t   = Input(Vec(clusterXdim, UInt(8.W)))

    val in_pipe   = Input(UInt(opuParams.cWidth.W))
    val out_pipe  = Output(UInt(opuParams.cWidth.W))

    val mrf_idx = Input(UInt(cellRegIdxBits.W))
    val row_idx = Input(UInt(log2Ceil(clusterYdim).W))
    val col_idx = Input(UInt(log2Ceil(clusterXdim).W))

    val macc  = Input(Bool())
    val shift = Input(Bool())
    val mvin  = Input(Bool())
    val mvin_bcast = Input(Bool())
    val altfmt = if (useMxOPU) Some(Input(Bool())) else None // alternate format for outer product
    val fp8 = if (useMxOPU) Some(Input(Bool())) else None // FP8 format for outer product
    val mvin_col = Input(Bool())
    val mvin_bcast_col = Input(Bool())
  })

  val cells = Seq.fill(clusterXdim, clusterYdim)(Module(new OuterProductCell))
  val cell_outs = Wire(Vec(clusterYdim, Vec(clusterXdim, UInt(opuParams.cWidth.W))))
  val pipe = Reg(UInt(opuParams.cWidth.W))

  for (i <- 0 until clusterYdim) {
    for (j <- 0 until clusterXdim) {
      val cell = cells(i)(j)

      cell.io.in_l  := io.in_l(i).asSInt
      cell.io.in_t  := io.in_t(j).asSInt
      cell.io.scale_l := io.scale_l(i)
      cell.io.scale_t := io.scale_t(j)
      cell.io.mrf_idx := io.mrf_idx
      if (useMxOPU) {
        cell.io.altfmt.get := io.altfmt.get
        cell.io.fp8.get := io.fp8.get
      }
      cell.io.macc := io.macc
      cell.io.mvin_bcast_col := io.mvin_bcast_col
      cell_outs(i)(j) := cell.io.out.asUInt

      cell.io.mvin_bcast := Mux(io.mvin_bcast, 
        j.U === io.col_idx,
        false.B
      )
      cell.io.mvin_bcast_col := Mux(io.mvin_bcast_col, 
        i.U === io.col_idx, 
        false.B
      )
      
      cell.io.mvin := Mux(io.mvin_col, 
        j.U === io.row_idx && i.U === io.col_idx,
        Mux(io.mvin, 
          i.U === io.row_idx && j.U === io.col_idx,
          false.B
        )
      )
      cell.io.mvin_data := 
        Mux(io.mvin_col || io.mvin_bcast_col, io.in_l.asUInt,
          Mux(io.mvin || io.mvin_bcast, io.in_t.asUInt,
            DontCare
          )
        )
    }
  }

  pipe := Mux(io.shift,
    io.in_pipe,
    cell_outs(io.row_idx)(io.col_idx)
  )

  io.out_pipe := pipe
}

class OuterProductControl(implicit p: Parameters) extends CoreBundle()(p) with HasOPUParams {
  val clock_enable = Bool()

  val in_l      = Vec(yDim, Vec(clusterYdim, UInt(opuParams.aWidth.W)))
  val in_t      = Vec(xDim, Vec(clusterXdim, UInt(opuParams.bWidth.W)))
  // OPFMACC.S scales. The sequencer keeps the scale cache's tags and decides
  // fills; the cached scale registers themselves are in OuterProductUnit.
  // sa_* are A's row scales (vs1+1), sb_* B's column scales (vs2+1).
  val scaled   = Bool() // apply the scales at sa_*/sb_*; else 2^0
  val sa_entry = UInt(log2Ceil(opuParams.nScaleEntries).W) // cache entry
  val sb_entry = UInt(log2Ceil(opuParams.nScaleEntries).W)
  val sa_piece = UInt(log2Ceil(egsPerVReg).W) // element group in the entry
  val sb_piece = UInt(log2Ceil(egsPerVReg).W)
  val sa_fill  = Bool() // store in_l at sa_entry, sa_piece
  val sb_fill  = Bool() // store in_t at sb_entry, sb_piece

  // same values broadcast horizontally
  val mrf_idx    = Vec(yDim, UInt(cellRegIdxBits.W)) 
  val row_idx    = Vec(yDim, UInt(log2Ceil(clusterYdim).W))
  val col_idx    = Vec(yDim, UInt(log2Ceil(clusterXdim).W))
  val macc       = Vec(yDim, Bool())
  val shift      = Vec(yDim, Bool())
  val mvin       = Vec(yDim, Bool())
  val mvin_bcast = Vec(yDim, Bool())
  val altfmt     = if (useMxOPU) Some(Vec(yDim, Bool())) else None // alternate format for outer product
  val fp8        = if (useMxOPU) Some(Vec(yDim, Bool())) else None // FP8 format for outer product
  //mvin_col broadcasts vertically
  val mvin_bcast_col = Vec(xDim, Bool())
  val mvin_col   = Vec(xDim, Bool()) // column write
}


class OuterProductUnit(implicit p: Parameters) extends CoreModule()(p) with HasOPUParams {
  val io = IO(new Bundle {
    val op = Input(new OuterProductControl)
    val out = Output(Vec(xDim, UInt(opuParams.cWidth.W)))
    val YOU_SHALL_PASS = Output(Bool())
  })

  // clock gating
  val gated_clock = ClockGate(clock, io.op.clock_enable, "opu_clock_gate")

  // Force OuterProductUnit to have logic to be syn-mappable
  io.YOU_SHALL_PASS := io.op.macc(0) & io.op.macc(0) | io.op.shift(0)
  dontTouch(io.YOU_SHALL_PASS)

  val clusters = Seq.fill(yDim, xDim)(withClock(gated_clock) { Module(new OuterProductCluster) })

  // OPFMACC.S scale cache data, one scale register per entry, on the ungated
  // clock so that fills land whatever the array's clock does. Fills arrive
  // on in_l / in_t as raw register-file data; scales are read with the
  // multiply-accumulate that uses them, a cycle or more after their fill.
  val scale_buf = Reg(Vec(opuParams.nScaleEntries, Vec(egsPerVReg, UInt(dLen.W))))
  when (io.op.sa_fill) { scale_buf(io.op.sa_entry)(io.op.sa_piece) := io.op.in_l.asUInt }
  when (io.op.sb_fill) { scale_buf(io.op.sb_entry)(io.op.sb_piece) := io.op.in_t.asUInt }
  // laid out like the Backend lays out in_l and in_t for a macc
  val sA_bytes = scale_buf(io.op.sa_entry)(io.op.sa_piece).asTypeOf(Vec(yDim * clusterYdim, UInt(8.W)))
  val sB_bytes = scale_buf(io.op.sb_entry)(io.op.sb_piece).asTypeOf(Vec(xDim * clusterXdim, UInt(8.W)))
  val scale_l = Wire(Vec(yDim, Vec(clusterYdim, UInt(8.W))))
  val scale_t = Wire(Vec(xDim, Vec(clusterXdim, UInt(8.W))))
  for (i <- 0 until yDim; j <- 0 until clusterYdim) {
    scale_l(i)(j) := Mux(io.op.scaled, sA_bytes(i + j * yDim), 128.U)
  }
  for (i <- 0 until xDim; j <- 0 until clusterXdim) {
    scale_t(i)(j) := Mux(io.op.scaled, sB_bytes(i + j * xDim), 128.U)
  }

  for (j <- 0 until xDim) {
    for (i <- 0 until yDim) {
      val cluster = clusters(i)(j)
      // column broadcast signals
      cluster.io.in_t      := io.op.in_t(j)
      cluster.io.scale_t   := scale_t(j)
      cluster.io.mvin_bcast_col := io.op.mvin_bcast_col(j)
      cluster.io.mvin_col   := io.op.mvin_col(j)
      // row broadcast signals
      cluster.io.in_l      := io.op.in_l(i)
      cluster.io.scale_l   := scale_l(i)
      cluster.io.mrf_idx    := io.op.mrf_idx(i)
      cluster.io.row_idx    := io.op.row_idx(i)
      cluster.io.col_idx    := io.op.col_idx(i)
      cluster.io.macc       := io.op.macc(i)
      cluster.io.shift      := io.op.shift(i)
      cluster.io.mvin_bcast := io.op.mvin_bcast(i)
      cluster.io.mvin       := io.op.mvin(i)
      if (useMxOPU) {
        cluster.io.altfmt.get := io.op.altfmt.get(i)
        cluster.io.fp8.get := io.op.fp8.get(i)
      }
    }

    clusters(0)(j).io.in_pipe := 0.U
    for (i <- 1 until yDim) {
      clusters(i)(j).io.in_pipe := clusters(i-1)(j).io.out_pipe
    }
    io.out(j) := clusters(yDim-1)(j).io.out_pipe
  }
}
