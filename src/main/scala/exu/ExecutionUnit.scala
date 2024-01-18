package vector.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import vector.common._

class ExecutionUnit(fus: FunctionalUnit*)(implicit val p: Parameters) extends HasCoreParameters with HasVectorParams {
  val supported_insns = fus.map(_.supported_insns).flatten
  val pipe_fus: Seq[PipelinedFunctionalUnit] = fus.collect { case p: PipelinedFunctionalUnit => p }
  val iter_fus: Seq[IterativeFunctionalUnit] = fus.collect { case i: IterativeFunctionalUnit => i }

  val pipe_depth = (pipe_fus.map(_.depth) :+ 0).max
  val nHazards = pipe_depth + iter_fus.size

  val iss_sub_dlen = Wire(UInt(log2Ceil(dLenB).W))
  val iss = Wire(Decoupled(new ExecuteMicroOp))

  val write = Wire(Valid(new VectorWrite(dLen)))
  val acc_write = Wire(Valid(new VectorWrite(dLen)))
  val vat_release = Wire(Valid(UInt(vParams.vatSz.W)))
  val hazards = Wire(Vec(nHazards, Valid(new PipeHazard)))
  val busy = Wire(Bool())

  val set_vxsat = Wire(Bool())
  val set_fflags = Wire(Valid(UInt(5.W)))
  val scalar_write = Wire(Decoupled(new ScalarWrite))

  fus.foreach { fu =>
    fu.io.iss.op := iss.bits
    fu.io.iss.valid := iss.valid
  }

  val pipe_write_hazard = WireInit(false.B)
  val readies = fus.map(_.io.iss.ready)
  iss.ready := readies.orR && !pipe_write_hazard
  iss_sub_dlen := Mux1H(readies, fus.map(_.io.iss.sub_dlen))
  when (iss.valid) { assert(PopCount(readies) <= 1.U) }

  val pipe_write = WireInit(false.B)

  vat_release.valid := false.B
  vat_release.bits := DontCare

  write.valid := false.B
  write.bits := DontCare
  acc_write.valid := false.B
  acc_write.bits := DontCare
  busy := false.B
  set_vxsat := fus.map(_.io.set_vxsat).orR
  set_fflags.valid := fus.map(_.io.set_fflags.valid).orR
  set_fflags.bits := fus.map(f => Mux(f.io.set_fflags.valid, f.io.set_fflags.bits, 0.U)).reduce(_|_)


  val scalar_write_arb = Module(new Arbiter(new ScalarWrite, fus.size))
  scalar_write_arb.io.in.zip(fus.map(_.io.scalar_write)).foreach { case (l, r) => l <> r }
  scalar_write <> scalar_write_arb.io.out

  if (pipe_fus.size > 0) {
    val pipe_iss_depth = Mux1H(pipe_fus.map(_.io.iss.ready), pipe_fus.map(_.depth.U))

    val pipe_valids    = Seq.fill(pipe_depth)(RegInit(false.B))
    val pipe_sels      = Seq.fill(pipe_depth)(Reg(UInt(pipe_fus.size.W)))
    val pipe_bits      = Seq.fill(pipe_depth)(Reg(new ExecuteMicroOp))
    val pipe_latencies = Seq.fill(pipe_depth)(Reg(UInt(log2Ceil(pipe_depth).W)))


    pipe_write_hazard := (0 until pipe_depth).map { i =>
      pipe_valids(i) && pipe_latencies(i) === pipe_iss_depth
    }.orR

    val pipe_iss = iss.fire && pipe_fus.map(_.io.iss.ready).orR
    pipe_valids.head := pipe_iss
    when (pipe_iss) {
      pipe_bits.head      := iss.bits
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

    val write_sel = pipe_valids.zip(pipe_latencies).map { case (v,l) => v && l === 0.U }
    val fu_sel = Mux1H(write_sel, pipe_sels)
    pipe_write := write_sel.orR
    when (write_sel.orR) {
      val acc = Mux1H(write_sel, pipe_bits.map(_.acc))
      val tail = Mux1H(write_sel, pipe_bits.map(_.tail))
      write.valid := Mux1H(fu_sel, pipe_fus.map(_.io.write.valid)) && (!acc || tail)
      write.bits := Mux1H(fu_sel, pipe_fus.map(_.io.write.bits))
      acc_write.valid := acc && !tail
      acc_write.bits := Mux1H(fu_sel, pipe_fus.map(_.io.write.bits))
      vat_release.valid := Mux1H(write_sel, pipe_bits.map(_.tail))
      vat_release.bits := Mux1H(write_sel, pipe_bits.map(_.vat))
    }

    when (pipe_valids.orR) { busy := true.B }
    for (i <- 0 until pipe_depth) {
      hazards(i).valid       := pipe_valids(i)
      hazards(i).bits.vat    := pipe_bits(i).vat
      hazards(i).bits.eg     := pipe_bits(i).wvd_eg
    }
  }

  if (iter_fus.size > 0) {
    val iter_write_arb = Module(new Arbiter(new VectorWrite(dLen), iter_fus.size))
    iter_write_arb.io.in.zip(iter_fus.map(_.io.write)).foreach { case (l,r) => l <> r }
    iter_write_arb.io.out.ready := !pipe_write

    when (!pipe_write) {
      write.valid     := iter_write_arb.io.out.valid
      write.bits.eg   := iter_write_arb.io.out.bits.eg
      write.bits.mask := iter_write_arb.io.out.bits.mask
      write.bits.data := iter_write_arb.io.out.bits.data
      vat_release.valid := iter_write_arb.io.out.fire() && Mux1H(iter_write_arb.io.in.map(_.ready), iter_fus.map(_.io.vat.valid))
      vat_release.bits  := Mux1H(iter_write_arb.io.in.map(_.fire()), iter_fus.map(_.io.vat.bits))
    }
    when (iter_fus.map(_.io.busy).orR) { busy := true.B }
    for (i <- 0 until iter_fus.size) {
      hazards(i+pipe_depth) := iter_fus(i).io.hazard
    }
  }
}
