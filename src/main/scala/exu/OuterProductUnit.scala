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

  val nMrfRegs : Int = 4
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

    // Contol Signals
    val mrf_idx = Input(UInt(cellRegIdxBits.W)) // Index for µarch register to write
    val altfmt = Input(Bool()) // alternate format for outer product
    val fp8 = Input(Bool()) // FP8 format for outer product
    val macc = Input(Bool())
    val mvin = Input(Bool())
    val mvin_bcast = Input(Bool())
    val mvin_bcast_col = Input(Bool())
    val mvin_data = Input(UInt(opuParams.cWidth.W))
    val out = Output(UInt(opuParams.cWidth.W))
  })

    def widen(in: UInt, inT: FType, outT: FType, active: Bool): UInt = {
      val widen = Module(new hardfloat.RecFNToRecFN(inT.exp, inT.sig, outT.exp, outT.sig))
      widen.io.in := Mux(active, in, 0.U)
      widen.io.roundingMode := hardfloat.consts.round_near_even
      widen.io.detectTininess := hardfloat.consts.tininess_afterRounding
      widen.io.out
    }

  // Matrix Register + Logic
  val regs = Reg(Vec(regsPerCell, UInt(opuParams.cWidth.W)))

  val f8macc = io.macc && io.fp8
  val f8a = MXFType.E5M3.recode(fp8ToE5M3(io.in_l.asUInt, io.altfmt))
  val f8b = MXFType.E5M3.recode(fp8ToE5M3(io.in_t.asUInt, io.altfmt))
  val f8aw = widen(f8a, MXFType.E5M3, FType.S, f8macc)
  val f8bw = widen(f8b, MXFType.E5M3, FType.S, f8macc)
  val latency = 1
  val fma = Module(new MulAddRecFNPipe(latency, FType.S.exp, FType.S.sig))
  fma.io.validin := f8macc
  fma.io.op := 0.U // FMA
  fma.io.roundingMode := hardfloat.consts.round_near_even
  fma.io.detectTininess := hardfloat.consts.tininess_afterRounding
  fma.io.a := f8aw
  fma.io.b := f8bw
  fma.io.c := FType.S.recode(regs(io.mrf_idx))
  
  // Pipeline control signals to match FMA latency
  val mrf_idx_pipe = Pipe(f8macc, io.mrf_idx, latency).bits
  val fp8_pipe = Pipe(f8macc, io.fp8, latency).bits
  val macc_pipe = Pipe(f8macc, io.macc, latency).bits
  
  val sum_fp8 = Mux(fma.io.validout, FType.S.ieee(fma.io.out), 0.U)

  val prod = Mux(io.macc, io.in_l * io.in_t, 0.S)
  val sum_int8 = prod + regs(io.mrf_idx).asSInt

  // Data going into MRF
  // Separate integer and FP8 MACC paths
  val int_macc = io.macc && !io.fp8
  val fp8_macc_complete = fma.io.validout
  
  for (i <- 0 until regsPerCell) {
    // Integer MACC: use current control signals
    val tile_match_int      = (io.mrf_idx >> log2Ceil(regsPerTileReg)) === (i >> log2Ceil(regsPerTileReg)).U
    val subtile_match_int   = io.mrf_idx(log2Ceil(regsPerTileReg)-1,0) === (i % regsPerTileReg).U
    
    // FP8 MACC: use pipelined control signals
    val tile_match_fp8      = (mrf_idx_pipe >> log2Ceil(regsPerTileReg)) === (i >> log2Ceil(regsPerTileReg)).U
    val subtile_match_fp8   = mrf_idx_pipe(log2Ceil(regsPerTileReg)-1,0) === (i % regsPerTileReg).U
    
    // MVIN paths: use current control signals
    val bcast_col_match = (io.mrf_idx >> log2Ceil(vLen/dLen)) === (i >> log2Ceil(vLen/dLen)).U
    val bcast_match     = io.mrf_idx(log2Ceil(vLen/dLen)-1,0) === (i % (vLen/dLen)).U
    
    when ((tile_match_int && int_macc && subtile_match_int) ||
          (tile_match_fp8 && fp8_macc_complete && subtile_match_fp8) ||
          (tile_match_int && io.mvin && subtile_match_int) ||
          (tile_match_int && io.mvin_bcast && bcast_match) ||
          (tile_match_int && io.mvin_bcast_col && bcast_col_match)) {
      regs(i) := Mux(int_macc, sum_int8.asUInt,
                 Mux(fp8_macc_complete, sum_fp8,
                     io.mvin_data))
    }
  }
  io.out := regs(io.mrf_idx)
}

class OuterProductCluster(implicit p : Parameters) extends CoreModule()(p) with HasOPUParams {
  val io = IO(new Bundle{
    val in_l      = Input(Vec(clusterYdim, UInt(opuParams.aWidth.W)))
    val in_t      = Input(Vec(clusterXdim, UInt(opuParams.bWidth.W)))

    val in_pipe   = Input(UInt(opuParams.cWidth.W))
    val out_pipe  = Output(UInt(opuParams.cWidth.W))

    val mrf_idx = Input(UInt(cellRegIdxBits.W))
    val row_idx = Input(UInt(log2Ceil(clusterYdim).W))
    val col_idx = Input(UInt(log2Ceil(clusterXdim).W))

    val macc  = Input(Bool())
    val shift = Input(Bool())
    val mvin  = Input(Bool())
    val mvin_bcast = Input(Bool())
    val altfmt = Input(Bool()) // alternate format for outer product
    val fp8 = Input(Bool()) // FP8 format for outer product
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
      cell.io.mrf_idx := io.mrf_idx
      cell.io.altfmt := io.altfmt
      cell.io.fp8 := io.fp8
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

  // same values broadcast horizontally
  val mrf_idx    = Vec(yDim, UInt(cellRegIdxBits.W)) 
  val row_idx    = Vec(yDim, UInt(log2Ceil(clusterYdim).W))
  val col_idx    = Vec(yDim, UInt(log2Ceil(clusterXdim).W))
  val macc       = Vec(yDim, Bool())
  val shift      = Vec(yDim, Bool())
  val mvin       = Vec(yDim, Bool())
  val mvin_bcast = Vec(yDim, Bool())
  val altfmt     = Vec(yDim, Bool()) // alternate format for outer product
  val fp8        = Vec(yDim, Bool()) // FP8 format for outer product
  val mvin_bcast_col = Vec(xDim, Bool())
  //mvin_col broadcasts vertically
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

  for (j <- 0 until xDim) {
    for (i <- 0 until yDim) {
      val cluster = clusters(i)(j)
      // column broadcast signals
      cluster.io.in_t      := io.op.in_t(j)
      cluster.io.mvin_bcast_col := io.op.mvin_bcast_col(j)
      cluster.io.mvin_col   := io.op.mvin_col(j)
      // row broadcast signals
      cluster.io.in_l      := io.op.in_l(i)
      cluster.io.mrf_idx    := io.op.mrf_idx(i)
      cluster.io.row_idx    := io.op.row_idx(i)
      cluster.io.col_idx    := io.op.col_idx(i)
      cluster.io.macc       := io.op.macc(i)
      cluster.io.shift      := io.op.shift(i)
      cluster.io.altfmt     := io.op.altfmt(i)
      cluster.io.fp8        := io.op.fp8(i)
      cluster.io.mvin_bcast := io.op.mvin_bcast(i)
      cluster.io.mvin       := io.op.mvin(i)
    }

    clusters(0)(j).io.in_pipe := 0.U
    for (i <- 1 until yDim) {
      clusters(i)(j).io.in_pipe := clusters(i-1)(j).io.out_pipe
    }
    io.out(j) := clusters(yDim-1)(j).io.out_pipe
  }
}
