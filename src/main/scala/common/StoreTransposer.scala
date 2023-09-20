package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

class StoreTransposePush(cols: Int, w: Int) extends Bundle {
  val wdata = Vec(cols, UInt(w.W))
  val tail = Bool()
  val elems = UInt((1+log2Ceil(cols)).W)
}


class StoreTransposeArray(val cols: Int, val width: Int, val outRows: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new StoreTransposePush(cols, width)))
    val out = Decoupled(new Bundle {
      val data = Vec(outRows, UInt(width.W))
      val count = UInt((1+log2Ceil(outRows)).W)
    })
    val busy = Output(Bool())
  })
  val rowBeats = 8 / outRows

  val modes = RegInit(VecInit.fill(2)(false.B))

  val in_sel = RegInit(false.B)
  val in_rows = RegInit(0.U(3.W))

  val out_sel   = RegInit(false.B)
  val out_rid   = RegInit(0.U(log2Ceil(rowBeats).W))
  val out_row   = if (rowBeats == 1) 0.U else out_rid << log2Ceil(outRows)
  val out_col   = RegInit(0.U(log2Ceil(cols).W))
  val out_rows_max = Reg(Vec(2, UInt(3.W)))
  val out_cols_max = Reg(Vec(2, UInt((1+log2Ceil(cols)).W)))

  val array = Seq.fill(2) { Reg(Vec(8, Vec(cols, UInt(width.W)))) }

  io.busy := modes.orR || in_rows =/= 0.U || out_rid =/= 0.U || out_col =/= 0.U

  io.in.ready := !modes(in_sel)
  io.out.valid := modes(out_sel)
  val out = Seq.fill(8) { Wire(UInt(width.W)) }
  for (r <- 0 until 8) {
    out(r) := Mux1H(UIntToOH(out_sel), array.map(_(r)(out_col)))
  }
  if (rowBeats == 1) {
    io.out.bits.data := VecInit(out)
  } else {
    for (r <- 0 until outRows) {
      io.out.bits.data(r) := VecInit(out.grouped(outRows).map(_(r)).toSeq)(out_rid)
    }
  }

  io.out.bits.count := Mux(
    out_rows_max(out_sel) +& 1.U - out_row >= outRows.U, outRows.U,
    out_rows_max(out_sel) +& 1.U - out_row)

  when (io.in.fire) {
    in_rows := in_rows + 1.U
    when (io.in.bits.tail) {
      out_rows_max(in_sel) := in_rows
      in_rows := 0.U
      out_cols_max(in_sel) := io.in.bits.elems
      in_sel := !in_sel
      modes(in_sel) := true.B
    }
  }
  for (s <- 0 until 2) {
    when (io.in.fire && in_sel === s.U) {
      array(s)(in_rows) := io.in.bits.wdata
    }
  }

  when (io.out.fire) {
    val next_row = out_row +& outRows.U
    out_rid := out_rid + 1.U
    when (next_row > out_rows_max(out_sel)) {
      out_rid := 0.U
      out_col := out_col +& 1.U
      when (out_col === out_cols_max(out_sel)) {
        out_sel := !out_sel
        out_rid := 0.U
        out_col := 0.U
        modes(out_sel) := false.B
      }
    }
  }
}

class StoreMultiTransposePush(bytes: Int) extends Bundle {
  val wdata = UInt((8*bytes).W)
  val tail = Bool()
  val elems = UInt((1+log2Ceil(bytes)).W)
  val eew = UInt(3.W)
}


class StoreMultiTransposeArray(bytes: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new StoreMultiTransposePush(bytes)))
    val out = Decoupled(new Bundle {
      val data = Vec(bytes, UInt(8.W))
      val count = UInt((1+log2Ceil(bytes)).W)
    })
    val busy = Output(Bool())
  })

  val arrays = (0 until 4).map { eew =>
    val cols = bytes >> eew
    val width = 8 << eew
    val outRows = 8 min (bytes >> eew)
    val arr = Module(new StoreTransposeArray(cols, width, outRows))
    arr
  }

  val r_eew = Reg(UInt(2.W))

  arrays.zipWithIndex.foreach { case (a,eew) =>
    a.io.in.valid := io.in.valid && io.in.bits.eew === eew.U
    a.io.in.bits.wdata := io.in.bits.wdata.asTypeOf(Vec(a.cols, UInt(a.width.W)))
    a.io.in.bits.tail  := io.in.bits.tail
    a.io.in.bits.elems := io.in.bits.elems
    a.io.out.ready     := io.out.ready
  }
  io.in.ready := VecInit(arrays.map(_.io.in.ready))(io.in.bits.eew)

  val out_valids = arrays.map(_.io.out.valid)
  io.out.valid := out_valids.orR
  io.out.bits.data := Mux1H(out_valids, arrays.map(_.io.out.bits.asUInt.asTypeOf(Vec(bytes, UInt(8.W)))))
  io.out.bits.count := Mux1H(out_valids, arrays.map(_.io.out.bits.count))


  when (r_eew =/= io.in.bits.eew && arrays.map(_.io.busy).orR) {
    io.in.ready := false.B
    arrays.map(_.io.in.valid := false.B)
  }
  when (io.in.fire) { r_eew := io.in.bits.eew }
  io.busy := arrays.map(_.io.busy).orR
}
