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
  //assert(PopCount(oldest_oh) <= 1.U)
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

class RegisterFileBank(reads: Int, maskReads: Int, rows: Int, maskRows: Int)(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val read = Vec(reads, Flipped(new VectorReadIO))
    val mask_read = Vec(maskReads, Flipped(new VectorReadIO))
    val write = Input(Valid(new VectorWrite(dLen)))
    val ll_write = Flipped(Decoupled(new VectorWrite(dLen)))
  })
  val ll_write_valid = RegInit(false.B)
  val ll_write_bits = Reg(new VectorWrite(dLen))

  val vrf = Mem(rows, Vec(dLen, Bool()))
  val v0_mask = Mem(maskRows, Vec(dLen, Bool()))
  for (read <- io.read) {
    read.req.ready := !(ll_write_valid && read.req.bits.eg === ll_write_bits.eg)
    read.resp := DontCare
    when (read.req.valid) {
      read.resp := vrf.read(read.req.bits.eg).asUInt
    }
  }
  for (mask_read <- io.mask_read) {
    mask_read.req.ready := !(ll_write_valid && mask_read.req.bits.eg === ll_write_bits.eg)
    mask_read.resp := DontCare
    when (mask_read.req.valid) {
      mask_read.resp := v0_mask.read(mask_read.req.bits.eg).asUInt
    }
  }

  val write = WireInit(io.write)
  io.ll_write.ready := false.B
  if (vParams.vrfHiccupBuffer) {
    when (!io.write.valid) { // drain hiccup buffer
      write.valid := ll_write_valid || io.ll_write.valid
      write.bits := Mux(ll_write_valid, ll_write_bits, io.ll_write.bits)
      ll_write_valid := false.B
      when (io.ll_write.valid && ll_write_valid) {
        ll_write_valid := true.B
        ll_write_bits := io.ll_write.bits
      }
      io.ll_write.ready := true.B
    } .elsewhen (!ll_write_valid) { // fill hiccup buffer
      when (io.ll_write.valid) {
        ll_write_valid := true.B
        ll_write_bits := io.ll_write.bits
      }
      io.ll_write.ready := true.B
    }
  } else {
    when (!io.write.valid) {
      io.ll_write.ready := true.B
      write.valid := io.ll_write.valid
      write.bits := io.ll_write.bits
    }
  }

  when (write.valid) {
    vrf.write(
      write.bits.eg,
      VecInit(write.bits.data.asBools),
      write.bits.mask.asBools)
    when (write.bits.eg < maskRows.U) {
      v0_mask.write(
        write.bits.eg,
        VecInit(write.bits.data.asBools),
        write.bits.mask.asBools)
    }
  }
}

class RegisterFile(reads: Seq[Int], maskReads: Seq[Int], pipeWrites: Int, llWrites: Int)(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {

  val nBanks = vParams.vrfBanking
  // Support 1, 2, and 4 banks for the VRF
  require(nBanks == 1 || nBanks == 2 || nBanks == 4)

  val io = IO(new Bundle {
    val read = MixedVec(reads.map(rc => Vec(rc, Flipped(new VectorReadIO))))
    val mask_read = MixedVec(maskReads.map(rc => Vec(rc, Flipped(new VectorReadIO))))

    val pipe_writes = Vec(pipeWrites, Input(Valid(new VectorWrite(dLen))))
    val ll_writes = Vec(llWrites, Flipped(Decoupled(new VectorWrite(dLen))))
  })

  val vrf = Seq.fill(nBanks) { Module(new RegisterFileBank(reads.size, maskReads.size, egsTotal/nBanks, if (egsPerVReg < nBanks) 1 else egsPerVReg / nBanks)) }

  reads.zipWithIndex.foreach { case (rc, i) =>
    val xbar = Module(new RegisterReadXbar(rc, nBanks))
    vrf.zipWithIndex.foreach { case (bank, j) =>
      bank.io.read(i) <> xbar.io.out(j)
    }
    xbar.io.in <> io.read(i)
  }

  maskReads.zipWithIndex.foreach { case (rc, i) =>
    val mask_xbar = Module(new RegisterReadXbar(rc, nBanks))
    vrf.zipWithIndex.foreach { case (bank, j) =>
      bank.io.mask_read(i) <> mask_xbar.io.out(j)
    }
    mask_xbar.io.in <> io.mask_read(i)
  }

  io.ll_writes.foreach(_.ready := false.B)

  vrf.zipWithIndex.foreach { case (rf, i) =>
    val bank_match = io.pipe_writes.map { w => (w.bits.bankId === i.U) && w.valid }
    val bank_write_data = Mux1H(bank_match, io.pipe_writes.map(_.bits.data))
    val bank_write_mask = Mux1H(bank_match, io.pipe_writes.map(_.bits.mask))
    val bank_write_eg   = Mux1H(bank_match, io.pipe_writes.map(_.bits.eg))
    val bank_write_valid = bank_match.orR

    rf.io.write.valid := bank_write_valid
    rf.io.write.bits.data := bank_write_data
    rf.io.write.bits.mask := bank_write_mask
    rf.io.write.bits.eg := bank_write_eg >> vrfBankBits
    when (bank_write_valid) { PopCount(bank_match) === 1.U }

    val ll_arb = Module(new Arbiter(new VectorWrite(dLen), llWrites))
    rf.io.ll_write <> ll_arb.io.out

    io.ll_writes.zipWithIndex.foreach { case (w, j) =>
      ll_arb.io.in(j).valid := w.valid && w.bits.bankId === i.U
      ll_arb.io.in(j).bits.eg   := w.bits.eg >> vrfBankBits
      ll_arb.io.in(j).bits.data := w.bits.data
      ll_arb.io.in(j).bits.mask := w.bits.mask
      when (ll_arb.io.in(j).ready && w.bits.bankId === i.U) {
        w.ready := true.B
      }
    }
  }
}
