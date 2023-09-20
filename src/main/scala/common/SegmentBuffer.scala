package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

class LoadSegmentBuffer(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Bundle {
      val data = UInt(dLen.W)
      val eew = UInt(2.W)
      val nf = UInt(3.W)
      val eidx = UInt(log2Ceil(maxVLMax).W)
      val sidx = UInt(3.W)
      val sidx_tail = Bool()
      val tail = Bool()
    }))
    val out = Decoupled(UInt(dLen.W))
    val busy = Output(Bool())
  })

  val rows = 8
  val cols = dLenB

  val wdata = Wire(Vec(4, UInt((rows*8*8).W)))
  val warr = wdata(io.in.bits.eew).asTypeOf(Vec(rows, Vec(8, UInt(8.W))))
  val wrow = WireInit(0.U(8.W))
  val wcol = WireInit(0.U(cols.W))
  val wmode = Wire(Bool())
  val array = Seq.tabulate(rows, cols, 2) { case (_,_,_) => Reg(UInt(8.W)) }

  for (r <- 0 until 8) {
    for (c <- 0 until cols) {
      for (s <- 0 until 2) {
        when (wrow(r) && wcol(c) && wmode === s.U) {
          array(r)(c)(s) := warr(r)(c % 8)
        }
      }
    }
  }

  val modes = RegInit(VecInit.fill(2)(false.B))
  val in_sel = RegInit(false.B)
  val out_sel = RegInit(false.B)
  val out_row = RegInit(0.U(log2Ceil(8).W))
  val out_nf  = Reg(Vec(2, UInt(3.W)))

  io.in.ready := !modes(in_sel)
  io.out.valid := modes(out_sel)
  io.out.bits := Mux1H(UIntToOH(out_row), array.map(row => VecInit(row.map(_(out_sel))).asUInt))

  when (io.in.fire) {
    wrow := ((1.U << (dLenB.U >> io.in.bits.eew)(4,0)) - 1.U)(7,0) << io.in.bits.sidx
  }
  wcol := ((1.U << (1.U << io.in.bits.eew)) - 1.U)(7,0) << (io.in.bits.eidx(log2Ceil(dLenB)-1,0) << io.in.bits.eew)(log2Ceil(dLenB)-1,0)
  wmode := in_sel

  for (eew <- 0 until 4) {
    val in_rows = 8 min (dLenB >> eew)
    val in_cols = 8 >> eew
    val in_elems = dLenB >> eew

    val col = Wire(Vec(in_rows, UInt((8 << eew).W)))
    val arr = Wire(Vec(in_rows, Vec(in_cols, UInt((8 << eew).W))))

    col := io.in.bits.data.asTypeOf(Vec(in_rows, UInt((8 << eew).W)))
    for (r <- 0 until in_rows) {
      for (c <- 0 until in_cols) {
        arr(r)(c) := col(r)
      }
    }

    wdata(eew) := Fill(8 / in_rows, arr.asUInt)
  }

  when (io.in.fire && io.in.bits.sidx_tail && io.in.bits.tail) {
    in_sel := !in_sel
    modes(in_sel) := true.B
    out_nf(in_sel) := io.in.bits.nf
  }

  when (io.out.fire) {
    when (out_row === out_nf(out_sel)) {
      out_sel := !out_sel
      out_row := 0.U
      modes(out_sel) := false.B
    } .otherwise {
      out_row := out_row + 1.U
    }
  }

  io.busy := modes.orR
}
