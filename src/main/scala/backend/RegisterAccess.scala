package saturn.backend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tile.{CoreModule}
import freechips.rocketchip.util._
import saturn.common._

class RegisterAccess(exSeqs: Int)(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val vls = new Bundle {
      val rvm = Flipped(new VectorReadIO)
    }
    val vss = new Bundle {
      val rvd = Flipped(new VectorReadIO)
      val rvm = Flipped(new VectorReadIO)
    }
    val vxs = Vec(exSeqs, new Bundle {
      val rvs1 = Flipped(new VectorReadIO)
      val rvs2 = Flipped(new VectorReadIO)
      val rvd = Flipped(new VectorReadIO)
      val rvm = Flipped(new VectorReadIO)
    })
    val vps = new Bundle {
      val rvs2 = Flipped(new VectorReadIO)
      val rvm = Flipped(new VectorReadIO)
    }
    val vrs = new Bundle {
      val rvs = Flipped(new VectorReadIO)
    }
    val frontend = new Bundle {
      val rindex = Flipped(new VectorReadIO)
      val rmask = Flipped(new VectorReadIO)
    }

    val pipe_writes = Vec(exSeqs, Input(Valid(new VectorWrite(dLen))))
    val iter_writes = Vec(exSeqs, Flipped(Decoupled(new VectorWrite(dLen))))
    val load_write = Flipped(Decoupled(new VectorWrite(dLen)))
  })

  // 3R1W banks
  //  read0 arbitrates between <- [vxs-rs1], vmu-index
  //  read1 arbitrates between <- [vxs-rs2], frontend-index
  //  read2 arbitrates between <- [vxs-rs3], vss-vrd, vrs-vss

  // Mask read arbitrates between <- [vxs], vls, vss, vps, frontend-mask

  val vrf = Module(new RegisterFile(
    reads = Seq(1 + exSeqs, 1 + exSeqs, 2 + exSeqs),
    maskReads = Seq(4 + exSeqs),
    pipeWrites = exSeqs,
    llWrites = exSeqs + 2 // load + reset
  ))

  val resetting = RegInit(true.B)
  val reset_ctr = RegInit(0.U(log2Ceil(egsTotal).W))

  // LL writes
  vrf.io.ll_writes(0).valid := resetting
  vrf.io.ll_writes(0).bits.eg := reset_ctr
  vrf.io.ll_writes(0).bits.data := 0.U
  vrf.io.ll_writes(0).bits.mask := ~(0.U(dLen.W))
  vrf.io.ll_writes(1) <> io.load_write
  for (i <- 0 until exSeqs) {
    vrf.io.ll_writes(2+i) <> io.iter_writes(i)
  }

  // Pipe writes
  for (i <- 0 until exSeqs) {
    vrf.io.pipe_writes(i) <> io.pipe_writes(i)
  }

  // Reads
  for (i <- 0 until exSeqs) {
    vrf.io.read(0)(i) <> io.vxs(i).rvs1
    vrf.io.read(1)(i) <> io.vxs(i).rvs2
    vrf.io.read(2)(i) <> io.vxs(i).rvd
    vrf.io.mask_read(0)(i) <> io.vxs(i).rvm
  }

  vrf.io.read(0)(exSeqs) <> io.vps.rvs2
  vrf.io.read(1)(exSeqs) <> io.frontend.rindex
  vrf.io.read(2)(exSeqs) <> io.vss.rvd
  vrf.io.read(2)(exSeqs+1) <> io.vrs.rvs

  vrf.io.mask_read(0)(exSeqs)   <> io.vls.rvm
  vrf.io.mask_read(0)(exSeqs+1) <> io.vss.rvm
  vrf.io.mask_read(0)(exSeqs+2) <> io.vps.rvm
  vrf.io.mask_read(0)(exSeqs+3) <> io.frontend.rmask

  when (resetting) {
    io.vls.rvm.req.ready := false.B
    io.vss.rvd.req.ready := false.B
    io.vss.rvm.req.ready := false.B
    io.vxs.foreach(_.rvs1.req.ready := false.B)
    io.vxs.foreach(_.rvs2.req.ready := false.B)
    io.vxs.foreach(_.rvd.req.ready := false.B)
    io.vxs.foreach(_.rvm.req.ready := false.B)
    io.vps.rvs2.req.ready := false.B
    io.vps.rvm.req.ready := false.B
    reset_ctr := reset_ctr + 1.U
    when (~reset_ctr === 0.U) { resetting := false.B }
  }


}
