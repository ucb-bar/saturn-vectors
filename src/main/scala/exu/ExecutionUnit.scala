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
    val iss_sub_dlen = Output(UInt(log2Ceil(dLenB).W))
    val iss = Flipped(Decoupled(new VectorMicroOp))

    val writes = Vec(2, Valid(new VectorWrite(dLen)))
    val vat_release = Vec(2, Valid(UInt(vParams.vatSz.W)))
    val hazards = Vec(nHazards, Valid(new PipeHazard))
    val busy = Output(Bool())

    val set_vxsat = Output(Bool())
  })

  fus.map(_.io.iss).foreach { iss =>
    iss.op := io.iss.bits
    iss.valid := io.iss.valid
  }

  val pipe_write_hazard = WireInit(false.B)
  val readies = fus.map(_.io.iss.ready)
  io.iss.ready := readies.orR && !pipe_write_hazard
  io.iss_sub_dlen := Mux1H(readies, fus.map(_.io.iss.sub_dlen))
  when (io.iss.valid) { assert(PopCount(readies) <= 1.U) }

  val pipe_writes = Seq.fill(2)(WireInit(false.B))

  io.vat_release.foreach(_.valid := false.B)
  io.vat_release.foreach(_.bits := DontCare)

  io.writes.foreach(_.valid := false.B)
  io.writes.foreach(_.bits := DontCare)
  io.busy := false.B
  io.set_vxsat := fus.map(_.io.set_vxsat).orR

  if (pipe_fus.size > 0) {
    val iss_wbank = Mux(io.iss.bits.wvd_widen2, 3.U, UIntToOH(io.iss.bits.wvd_eg(0)))
    val pipe_iss_depth = Mux1H(pipe_fus.map(_.io.iss.ready), pipe_fus.map(_.depth.U))

    val pipe_valids    = Seq.fill(pipe_depth)(RegInit(false.B))
    val pipe_sels      = Seq.fill(pipe_depth)(Reg(UInt(pipe_fus.size.W)))
    val pipe_bits      = Seq.fill(pipe_depth)(Reg(new VectorMicroOp))
    val pipe_latencies = Seq.fill(pipe_depth)(Reg(UInt(log2Ceil(pipe_depth).W)))
    val pipe_wbanks    = pipe_bits.map { b => Mux(b.wvd_widen2, 3.U, UIntToOH(b.wvd_eg(0))) }


    pipe_write_hazard := (0 until pipe_depth).map { i =>
      pipe_valids(i) && pipe_latencies(i) === pipe_iss_depth && ((pipe_wbanks(i) & iss_wbank) =/= 0.U)
    }.orR

    val pipe_iss = io.iss.fire && pipe_fus.map(_.io.iss.ready).orR
    pipe_valids.head := pipe_iss
    when (pipe_iss) {
      pipe_bits.head      := io.iss.bits
      pipe_latencies.head := pipe_iss_depth - 1.U
      pipe_sels.head      := VecInit(pipe_fus.map(_.io.iss.ready)).asUInt
    }
    for (i <- 1 until pipe_depth) {
      val fire = pipe_valids(i-1) && pipe_latencies(i-1) =/= 0.U
      pipe_valids(i) := fire
      when (fire) {
        pipe_bits(i)      := pipe_bits(i-1)
        pipe_latencies(i) := pipe_latencies(i-1) - 1.U
        pipe_sels(i)      := pipe_sels(i-1)
      }
    }
    for ((fu, j) <- pipe_fus.zipWithIndex) {
      for (i <- 0 until fu.depth) {
        fu.io.pipe(i).valid := pipe_valids(i) && pipe_sels(i)(j)
        fu.io.pipe(i).bits  := pipe_bits(i)
      }
    }

    val pipe_narrow_writes = pipe_fus.map { pipe => 
      val writes = Wire(Vec(2, Valid(new VectorWrite(dLen))))
      for (b <- 0 until 2) {
        if (pipe.wideWrite) {
          val mask = pipe.io.write.bits.mask.asTypeOf(Vec(2, UInt(dLen.W)))(b)
          val data = pipe.io.write.bits.data.asTypeOf(Vec(2, UInt(dLen.W)))(b)
          writes(b).valid      := pipe.io.write.valid && mask =/= 0.U
          writes(b).bits.eg    := pipe.io.write.bits.eg
          writes(b).bits.data  := data
          writes(b).bits.mask  := mask
        } else {
          writes(b).valid      := pipe.io.write.valid && pipe.io.write.bits.eg(0) === b.U
          writes(b).bits.eg    := pipe.io.write.bits.eg >> 1
          writes(b).bits.data  := pipe.io.write.bits.data
          writes(b).bits.mask  := pipe.io.write.bits.mask
        }
      }
      writes
    }

    for (b <- 0 until 2) {
      val write_sel = pipe_valids.zip(pipe_latencies).zip(pipe_wbanks)
        .map { case ((v, l), k) => v && l === 0.U && k(b) }
      val fu_sel = Mux1H(write_sel, pipe_sels)
      val vat_release = Mux1H(write_sel, pipe_bits.map(_.last))
      pipe_writes(b) := write_sel.orR
      when (write_sel.orR) {
        io.writes(b) := Mux1H(fu_sel, pipe_narrow_writes.map(_(b)))
        io.vat_release(b).valid   := vat_release
        io.vat_release(b).bits    := Mux1H(write_sel, pipe_bits.map(_.vat))
      }
    }
    when (pipe_valids.orR) { io.busy := true.B }
    for (i <- 0 until pipe_depth) {
      io.hazards(i).valid       := pipe_valids(i)
      io.hazards(i).bits.vat    := pipe_bits(i).vat
      io.hazards(i).bits.eg     := pipe_bits(i).wvd_eg
      io.hazards(i).bits.widen2 := pipe_bits(i).wvd_widen2
    }
  }

  if (iter_fus.size > 0) {
    val iter_write_arb = Module(new Arbiter(new VectorWrite(dLen), iter_fus.size))
    val iter_write_bank = UIntToOH(iter_write_arb.io.out.bits.eg(0))
    iter_write_arb.io.in.zip(iter_fus.map(_.io.write)).foreach { case (l,r) => l <> r }
    iter_write_arb.io.out.ready := !Mux1H(iter_write_bank, pipe_writes)
    for (b <- 0 until 2) {
      when (!pipe_writes(b)) {
        io.writes(b).valid     := iter_write_arb.io.out.valid && iter_write_arb.io.out.bits.eg(0) === b.U
        io.writes(b).bits.eg   := iter_write_arb.io.out.bits.eg >> 1
        io.writes(b).bits.mask := iter_write_arb.io.out.bits.mask
        io.writes(b).bits.data := iter_write_arb.io.out.bits.data
        io.vat_release(b).valid := iter_write_arb.io.out.fire() && Mux1H(iter_write_arb.io.in.map(_.ready), iter_fus.map(_.io.vat.valid))
        io.vat_release(b).bits  := Mux1H(iter_write_arb.io.in.map(_.fire()), iter_fus.map(_.io.vat.bits))

      }
    }
    when (iter_fus.map(_.io.busy).orR) { io.busy := true.B }
    for (i <- 0 until iter_fus.size) {
      io.hazards(i+pipe_depth) := iter_fus(i).io.hazard
    }
  }
}
