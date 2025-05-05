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
object OPUTypes extends Enumeration { val INT32, INT16, INT8 = Value }

// Parameters for configured OPU
case class OPUParameters (
  val aWidth : Int = 8,
  val bWidth : Int = 8,
  val cWidth : Int = 32, // Accumulator size

  val nMrfRegs : Int = 2
)

trait HasOPUParams extends HasVectorParams { this: HasCoreParameters =>
  def varchRatio = vLen / dLen
  def regsPerTileReg = varchRatio * varchRatio
  def regsPerCell = regsPerTileReg * opuParams.nMrfRegs
  def cellRegIdxBits = log2Ceil(regsPerCell)
  def prodWidth = opuParams.aWidth + opuParams.bWidth

  def wideningFactor = opuParams.cWidth / opuParams.aWidth

  def clusterXdim = opuParams.cWidth / opuParams.bWidth
  def clusterYdim = clusterXdim

  def yDim = (dLen / opuParams.aWidth) / clusterYdim
  def xDim = (dLen / opuParams.bWidth) / clusterXdim

}


/*
 * A single cell in the Outer Product Unit MACC array
 * Accumulation register alse serve as pipeline registers
 * during read out
 */
class OuterProductCell(implicit p: Parameters) extends CoreModule()(p) with HasOPUParams {

  val io = IO(new Bundle{
    // Data signals
    val in_l = Input(SInt(opuParams.aWidth.W)) // left input
    val in_t = Input(SInt(opuParams.bWidth.W)) // top input

    // Contol Signals
    val mrf_idx = Input(UInt(cellRegIdxBits.W)) // Index for µarch register to write

    val macc = Input(Bool())
    val mvin = Input(Bool())
    val mvin_bcast = Input(Bool())
    val mvin_data = Input(SInt(opuParams.cWidth.W))
    val out = Output(SInt(opuParams.cWidth.W))
  })
  // Matrix Register + Logic
  val regs = Reg(Vec(regsPerCell, SInt(opuParams.cWidth.W)))

  // TODO: Need to check for overflow and saturate to accumulator width
  val prod = Mux(io.macc, io.in_l * io.in_t, 0.S)
  val sum = prod + regs(io.mrf_idx)

  // Data going into MRF
  for (i <- 0 until regsPerCell) {
    val tile_match = (io.mrf_idx >> log2Ceil(regsPerTileReg)) === (i >> log2Ceil(regsPerTileReg)).U
    val subtile_match = io.mrf_idx(log2Ceil(regsPerTileReg)-1,0) === (i % regsPerTileReg).U

    when (tile_match && (((io.mvin || io.macc) && subtile_match) || io.mvin_bcast)) {
      regs(i) := Mux(io.macc, sum, io.mvin_data)
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
  })

  val cells = Seq.fill(clusterXdim, clusterYdim)(Module(new OuterProductCell))
  val cell_outs = Wire(Vec(clusterYdim, Vec(clusterXdim, UInt(opuParams.cWidth.W))))
  val pipe = Reg(UInt(opuParams.cWidth.W))

  for (i <- 0 until clusterYdim) {
    for (j <- 0 until clusterXdim) {
      val cell = cells(i)(j)

      cell.io.in_l  := io.in_l(i).asSInt
      cell.io.in_t  := io.in_t(j).asSInt

      cell.io.macc := io.macc
      cell.io.mvin := io.mvin && i.U === io.row_idx && j.U === io.col_idx
      cell.io.mvin_bcast := io.mvin_bcast && j.U === io.col_idx
      cell.io.mvin_data := io.in_t.asUInt.asSInt
      cell.io.mrf_idx := io.mrf_idx
      cell_outs(i)(j) := cell.io.out.asUInt
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
  val mvin       = Vec(yDim, Bool())
  val mvin_bcast = Vec(yDim, Bool())
  val shift      = Vec(yDim, Bool())
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
      cluster.io.in_l      := io.op.in_l(i)
      cluster.io.in_t      := io.op.in_t(j)

      cluster.io.mrf_idx    := io.op.mrf_idx(i)
      cluster.io.row_idx    := io.op.row_idx(i)
      cluster.io.col_idx    := io.op.col_idx(i)
      cluster.io.macc       := io.op.macc(i)
      cluster.io.mvin       := io.op.mvin(i)
      cluster.io.mvin_bcast := io.op.mvin_bcast(i)
      cluster.io.shift      := io.op.shift(i)
    }

    clusters(0)(j).io.in_pipe := 0.U
    for (i <- 1 until yDim) {
      clusters(i)(j).io.in_pipe := clusters(i-1)(j).io.out_pipe
    }
    io.out(j) := clusters(yDim-1)(j).io.out_pipe
  }
}
