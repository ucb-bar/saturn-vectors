package vector.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import vector.common._

class ExecutionUnit(genFUs: Seq[() => FunctionalUnit])(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val fus = genFUs.map(gen => Module(gen()))
  val pipe_fus: Seq[PipelinedFunctionalUnit] = fus.collect { case p: PipelinedFunctionalUnit => p }
  val iter_fus: Seq[IterativeFunctionalUnit] = fus.collect { case i: IterativeFunctionalUnit => i }

  val pipe_depth = (pipe_fus.map(_.depth) :+ 0).max
  val nHazards = pipe_depth + iter_fus.size

  val io = IO(new Bundle {
    val iss = Flipped(Decoupled(new VectorMicroOp))

    val writes = Vec(2, Valid(new VectorWrite))
    val vat_release = Valid(UInt(vParams.vatSz.W))
    val hazards = Vec(nHazards, Valid(new PipeHazard))
    val busy = Output(Bool())
  })

  fus.map(_.io.iss).foreach { iss =>
    iss.funct3 := io.iss.bits.funct3
    iss.funct6 := io.iss.bits.funct6
  }
  iter_fus.map(_.io.iss_op).foreach { iss =>
    iss.valid := io.iss.valid
    iss.bits := io.iss.bits
  }

  val pipe_write_hazard = WireInit(false.B)
  val readies = fus.map(_.io.iss.ready)
  io.iss.ready := readies.orR && !pipe_write_hazard
  when (io.iss.valid) { assert(PopCount(readies) <= 1.U) }

  val pipe_write = WireInit(false.B)

  io.vat_release.valid := false.B
  io.vat_release.bits := DontCare

  io.writes.foreach(_.valid := false.B)
  io.writes.foreach(_.bits := DontCare)
  io.busy := false.B

  if (pipe_fus.size > 0) {
    val pipe_iss_depth = Mux1H(pipe_fus.map(_.io.iss.ready), pipe_fus.map(_.depth.U))

    val pipe_valids = Seq.fill(pipe_depth)(RegInit(false.B))
    val pipe_bits = Seq.fill(pipe_depth)(Reg(new VectorMicroOp))
    val pipe_latencies = Seq.fill(pipe_depth)(Reg(UInt(log2Ceil(pipe_depth).W)))

    pipe_write_hazard := (0 until pipe_depth).map { i =>
      pipe_valids(i) && pipe_latencies(i) === pipe_iss_depth
    }.orR

    val pipe_iss = io.iss.fire && pipe_fus.map(_.io.iss.ready).orR
    pipe_valids.head := pipe_iss
    when (pipe_iss) {
      pipe_bits.head := io.iss.bits
      pipe_latencies.head := pipe_iss_depth - 1.U
    }
    for (i <- 1 until pipe_depth) {
      val fire = pipe_valids(i-1) && pipe_latencies(i-1) =/= 0.U
      pipe_valids(i) := fire
      when (fire) {
        pipe_bits(i) := pipe_bits(i-1)
        pipe_latencies(i) := pipe_latencies(i-1) - 1.U
      }
    }
    for (i <- 0 until pipe_depth) {
      pipe_fus.foreach(_.io.pipe(0).valid := pipe_valids(i))
      pipe_fus.foreach(_.io.pipe(0).bits  := pipe_bits(i))
    }
    val write_sel = pipe_valids.zip(pipe_bits).zip(pipe_latencies).map { case ((v, b), l) =>
      v && l === 0.U
    }
    val vat_release = Mux1H(write_sel, pipe_bits.map(_.last))
    pipe_write := write_sel.orR
    when (write_sel.orR) {
      io.writes            := Mux1H(write_sel, pipe_fus.map(_.io.writes))
      io.vat_release.valid := vat_release
      io.vat_release.bits  := Mux1H(write_sel, pipe_bits.map(_.vat))
    }
    when (pipe_valids.orR) { io.busy := true.B }
    for (i <- 0 until pipe_depth) {
      io.hazards(i).valid := pipe_valids(i)
      io.hazards(i).bits.vat    := pipe_bits(i).vat
      io.hazards(i).bits.eg     := pipe_bits(i).wvd_eg
      io.hazards(i).bits.widen2 := pipe_bits(i).wvd_widen2
    }
  }

  if (iter_fus.size > 0) {
    val iter_write_arb = Module(new Arbiter(new VectorWrite, iter_fus.size))
    iter_write_arb.io.in.zip(iter_fus.map(_.io.write)).foreach { case (l,r) => l <> r }
    iter_write_arb.io.out.ready := !pipe_write
    when (!pipe_write) {
      for (b <- 0 until 2) {
        io.writes(b).valid   := iter_write_arb.io.out.valid && iter_write_arb.io.out.bits.eg(0) === b.U
        io.writes(b).bits.eg := iter_write_arb.io.out.bits.eg >> 1
        io.writes(b).bits.mask := iter_write_arb.io.out.bits.mask
        io.writes(b).bits.data := iter_write_arb.io.out.bits.data
      }
      io.vat_release := Mux1H(iter_write_arb.io.in.map(_.fire()), iter_fus.map(_.io.vat))
    }
    when (iter_fus.map(_.io.busy).orR) { io.busy := true.B }
    for (i <- 0 until iter_fus.size) {
      io.hazards(i).valid := iter_fus(i).io.hazard
    }
  }
}
