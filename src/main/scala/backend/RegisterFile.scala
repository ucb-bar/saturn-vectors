package saturn.backend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tile.{CoreModule}
import saturn.common._

class RegisterReadXbar(n: Int, banks: Int)(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val in = Vec(n, Flipped(new VectorReadIO))
    val out = Vec(banks, new VectorReadIO)
  })

  val arbs = Seq.fill(banks) { Module(new RRArbiter(UInt(log2Ceil(egsTotal).W), n)) }
  for (i <- 0 until banks) {
      io.out(i).req <> arbs(i).io.out
  }

  val bankOffset = log2Ceil(banks)

  for (i <- 0 until n) {
    val bank_sel = if (bankOffset == 0) true.B else UIntToOH(io.in(i).req.bits(bankOffset-1,0))
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
    val write = Vec(writes, Input(Valid(new VectorWrite(dLen))))
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

class RegisterFile(reads: Seq[Int])(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {

  val nBanks = vParams.vrfBanking
  // Support 1, 2, and 4 banks for the VRF
  require(nBanks == 1 || nBanks == 2 || nBanks == 4)

  val io = IO(new Bundle {
    val read = MixedVec(reads.map(rc => Vec(rc, Flipped(new VectorReadIO))))
    val write = Vec(nBanks, Input(Valid(new VectorWrite(dLen))))
  })

  val vrf = Seq.fill(nBanks) { Module(new RegisterFileBank(reads.size, 1, egsTotal/nBanks)) }

  reads.zipWithIndex.foreach { case (rc, i) =>
    val xbar = Module(new RegisterReadXbar(rc, nBanks))
    vrf.zipWithIndex.foreach { case (bank, j) =>
      bank.io.read(i) <> xbar.io.out(j)
    }
    xbar.io.in <> io.read(i)
  }

  vrf.zip(io.write).foreach { case (rf, w) =>
    rf.io.write(0) := w
  }
}
