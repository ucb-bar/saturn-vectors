package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

class RegisterReadXbar(n: Int)(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val banks = 2
  val io = IO(new Bundle {
    val in = Vec(n, Flipped(new VectorReadIO))
    val out = Vec(banks, new VectorReadIO)
  })

  val arbs = Seq.fill(banks) { Module(new RRArbiter(UInt(log2Ceil(egsTotal).W), n)) }
  for (i <- 0 until banks) {
    io.out(i).req <> arbs(i).io.out
  }

  io.in.foreach(_.req.ready := false.B)

  val bankOffset = 1

  for (i <- 0 until n) {
    val bank_sel = UIntToOH(io.in(i).req.bits(bankOffset-1,0))
    for (j <- 0 until banks) {
      arbs(j).io.in(i).valid := io.in(i).req.valid && bank_sel(j)
      arbs(j).io.in(i).bits := io.in(i).req.bits >> bankOffset
    }
    io.in(i).req.ready := Mux1H(bank_sel, arbs.map(_.io.in(i).ready))
    io.in(i).resp := Mux1H(bank_sel, io.out.map(_.resp))
  }
}

class RegisterFileBank(reads: Int, writes: Int, rows: Int)(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val read = Vec(reads, Flipped(new VectorReadIO))
    val write = Vec(writes, Input(Valid(new VectorWrite)))
  })

  val vrf = Mem(rows, Vec(dLen, Bool()))
  for (read <- io.read) {
    read.req.ready := true.B
    read.resp := vrf.read(read.req.bits).asUInt
  }

  for (write <- io.write) {
    when (write.valid) {
      vrf.write(
        write.bits.eg,
        VecInit(write.bits.data.asBools),
        write.bits.mask.asBools)
    }
  }
}
