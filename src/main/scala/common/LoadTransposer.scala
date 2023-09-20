package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._


class LoadTransposePush(w: Int, inRows: Int) extends Bundle {
  val nf    = UInt(3.W)
  val wdata = Vec(inRows, UInt(w.W))
  val tail  = Bool()
}

class LoadTransposeArray(val cols: Int, val width: Int, val inRows: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new LoadTransposePush(width, inRows)))
    val out = Decoupled(Vec(cols, UInt(width.W)))
    val busy = Output(Bool())
  })

  val modes = RegInit(VecInit.fill(2)(false.B))
  val max_rid = 8 / inRows

  val in_sel   = RegInit(false.B)
  val in_cols  = RegInit(0.U(log2Ceil(cols).W))
  val in_rid   = RegInit(0.U(log2Ceil(max_rid).W))

  val out_sel      = RegInit(false.B)
  val out_rows     = RegInit(0.U(3.W))
  val out_rows_max = Reg(Vec(2, UInt(3.W)))

  val array = Seq.fill(2) { Reg(Vec(8, Vec(cols, UInt(width.W)))) }

  io.in.ready  := !modes(in_sel)
  io.out.valid := modes(out_sel)
  io.out.bits  := Mux1H(UIntToOH(out_sel), array.map(_(out_rows)))

  io.busy := modes.orR || in_cols =/= 0.U || in_rid =/= 0.U

  when (io.in.fire) { in_rid := in_rid + 1.U }
  when (io.in.fire && io.in.bits.tail) {
    when (in_cols === (cols-1).U) {
      in_sel   := !in_sel
      in_cols  := 0.U
      in_rid   := 0.U
      modes       (in_sel) := true.B
      out_rows_max(in_sel) := io.in.bits.nf
    } .otherwise {
      in_cols  := in_cols + 1.U
      in_rid   := 0.U
    }
  }
  when (io.out.fire) {
    when (out_rows === out_rows_max(out_sel)) {
      out_sel        := !out_sel
      out_rows       := 0.U
      modes(out_sel) := false.B
    } .otherwise {
      out_rows       := out_rows + 1.U
    }
  }

  when (io.in.fire && io.out.fire) {
    assert(in_sel =/= out_sel)
  }

  for (s <- 0 until 2) {
    for (r <- 0 until 8) {
      when (io.in.fire && (r / inRows).U === in_rid && in_sel === s.U) {
        array(s)(r)(in_cols) := io.in.bits.wdata(r % inRows)
      }
    }
  }
}

class LoadMultiTransposePush(w: Int) extends Bundle {
  val eew = UInt(2.W)
  val nf = UInt(3.W)
  val wdata = UInt((8*w).W)
  val tail = Bool()
}


class LoadMultiTransposeArray(bytes: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new LoadMultiTransposePush(bytes)))
    val out = Decoupled(UInt((8*bytes).W))
    val busy = Output(Bool())
  })

  val arrays = (0 until 4).map { eew =>
    val cols = bytes >> eew
    val width = 8 << eew
    val inRows = 8 min (bytes >> eew)
    val arr = Module(new LoadTransposeArray(cols, width, inRows))
    arr
  }

  val r_eew = Reg(UInt(2.W))

  arrays.zipWithIndex.foreach { case (a,eew) =>
    a.io.in.valid := io.in.valid && io.in.bits.eew === eew.U
    a.io.in.bits.nf := io.in.bits.nf
    a.io.in.bits.tail := io.in.bits.tail
    a.io.in.bits.wdata := io.in.bits.wdata.asTypeOf(Vec(a.inRows, UInt(a.width.W)))
    a.io.out.ready := io.out.ready
  }
  io.in.ready := VecInit(arrays.map(_.io.in.ready))(io.in.bits.eew)

  val out_valids = arrays.map(_.io.out.valid)
  assert(PopCount(out_valids) <= 1.U)
  io.out.valid := out_valids.orR
  io.out.bits := Mux1H(out_valids, arrays.map(_.io.out.bits.asUInt))

  when (r_eew =/= io.in.bits.eew && arrays.map(_.io.busy).orR) {
    io.in.ready := false.B
    arrays.map(_.io.in.valid := false.B)
  }

  when (io.in.fire) { r_eew := io.in.bits.eew }
  io.busy := arrays.map(_.io.busy).orR
}
