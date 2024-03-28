package saturn.backend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tile.{CoreModule}
import freechips.rocketchip.util._
import saturn.common._

class OldestRRArbiter(val n: Int)(implicit p: Parameters) extends Module {
  val io = IO(new ArbiterIO(new VectorReadReq, n))

  val arb = Module(new RRArbiter(new VectorReadReq, n))
  io <> arb.io
  val oldest_oh = io.in.map(i => i.valid && i.bits.oldest)
  assert(PopCount(oldest_oh) <= 1.U)
  when (oldest_oh.orR) {
    io.chosen := VecInit(oldest_oh).asUInt
    io.out.valid := true.B
    io.out.bits := Mux1H(oldest_oh, io.in.map(_.bits))
    for (i <- 0 until n) {
      io.in(i).ready := oldest_oh(i) && io.out.ready
    }
  }
}

class RegisterReadXbar(n: Int, banks: Int)(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val in = Vec(n, Flipped(new VectorReadIO))
    val out = Vec(banks, new VectorReadIO)
  })

  val arbs = Seq.fill(banks) { Module(new OldestRRArbiter(n)) }
  for (i <- 0 until banks) {
    io.out(i).req <> arbs(i).io.out
  }

  val bankOffset = log2Ceil(banks)

  for (i <- 0 until n) {
    val bank_sel = if (bankOffset == 0) true.B else UIntToOH(io.in(i).req.bits.eg(bankOffset-1,0))
    for (j <- 0 until banks) {
      arbs(j).io.in(i).valid := io.in(i).req.valid && bank_sel(j)
      arbs(j).io.in(i).bits.eg := io.in(i).req.bits.eg >> bankOffset
      arbs(j).io.in(i).bits.oldest := io.in(i).req.bits.oldest
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
    read.resp := vrf.read(read.req.bits.eg).asUInt
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

class RegisterFile(reads: Seq[Int], pipeWrites: Int, llWrites: Int)(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {

  val nBanks = vParams.vrfBanking
  // Support 1, 2, and 4 banks for the VRF
  require(nBanks == 1 || nBanks == 2 || nBanks == 4)

  val io = IO(new Bundle {
    val read = MixedVec(reads.map(rc => Vec(rc, Flipped(new VectorReadIO))))

    val pipe_writes = Vec(pipeWrites, Input(Valid(new VectorWrite(dLen))))
    val ll_writes = Vec(llWrites, Flipped(Decoupled(new VectorWrite(dLen))))
  })

  val vrf = Seq.fill(nBanks) { Module(new RegisterFileBank(reads.size, 1, egsTotal/nBanks)) }

  reads.zipWithIndex.foreach { case (rc, i) =>
    val xbar = Module(new RegisterReadXbar(rc, nBanks))
    vrf.zipWithIndex.foreach { case (bank, j) =>
      bank.io.read(i) <> xbar.io.out(j)
    }
    xbar.io.in <> io.read(i)
  }

  io.ll_writes.foreach(_.ready := false.B)

  vrf.zipWithIndex.foreach { case (rf, i) =>
    val arb = Module(new Arbiter(new VectorWrite(dLen), 1 + llWrites))
    rf.io.write(0).valid := arb.io.out.valid
    rf.io.write(0).bits  := arb.io.out.bits
    arb.io.out.ready := true.B

    val bank_match = io.pipe_writes.map { w => (w.bits.bankId === i.U) && w.valid }
    val bank_write_data = Mux1H(bank_match, io.pipe_writes.map(_.bits.data))
    val bank_write_mask = Mux1H(bank_match, io.pipe_writes.map(_.bits.mask))
    val bank_write_eg   = Mux1H(bank_match, io.pipe_writes.map(_.bits.eg))
    val bank_write_valid = bank_match.orR

    arb.io.in(0).valid := bank_write_valid
    arb.io.in(0).bits.data := bank_write_data
    arb.io.in(0).bits.mask := bank_write_mask
    arb.io.in(0).bits.eg   := bank_write_eg >> vrfBankBits

    when (bank_write_valid) { assert(arb.io.in(0).ready && PopCount(bank_match) === 1.U) }

    io.ll_writes.zipWithIndex.foreach { case (w, j) =>
      arb.io.in(1+j).valid := w.valid && w.bits.bankId === i.U
      arb.io.in(1+j).bits.eg   := w.bits.eg >> vrfBankBits
      arb.io.in(1+j).bits.data := w.bits.data
      arb.io.in(1+j).bits.mask := w.bits.mask
      when (arb.io.in(1+j).ready && w.bits.bankId === i.U) {
        w.ready := true.B
      }
    }
  }
}
