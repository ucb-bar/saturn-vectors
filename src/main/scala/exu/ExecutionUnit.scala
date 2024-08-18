package saturn.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import saturn.common._

class ExecutionUnit(genFUs: Seq[FunctionalUnitFactory])(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val fus = genFUs.map(gen => Module(gen.generate(p)))

  val pipe_fus: Seq[PipelinedFunctionalUnit] = fus.collect { case p: PipelinedFunctionalUnit => p }
  val iter_fus: Seq[IterativeFunctionalUnit] = fus.collect { case i: IterativeFunctionalUnit => i }

  val pipe_depth = (pipe_fus.map(_.depth) :+ 0).max

  val io = IO(new Bundle {
    val iss = Flipped(Decoupled(new ExecuteMicroOpWithData))
    val iter_hazards = Output(Vec(iter_fus.size, Valid(new PipeHazard(pipe_depth))))
    val iter_write = Decoupled(new VectorWrite(dLen))
    val pipe_write = Output(Valid(new VectorWrite(dLen)))
    val acc_write = Output(Valid(new VectorWrite(dLen)))
    val scalar_write = Decoupled(new ScalarWrite)

    val pipe_hazards = Output(Vec(pipe_depth, Valid(new PipeHazard(pipe_depth))))
    val issue_pipe_latency = Output(UInt((log2Ceil(pipe_depth) + 1).W))

    val shared_fp_req = Decoupled(new FPInput())
    val shared_fp_resp = Flipped(Valid(new FPResult()))

    val set_vxsat = Output(Bool())
    val set_fflags = Output(Valid(UInt(5.W)))
    val busy = Output(Bool())
  })

  val sharedFPUnits = fus.collect { case fp: HasSharedFPUIO => fp }
  val hasSharedFPUnits = sharedFPUnits.size > 0

  io.shared_fp_req.valid := false.B
  io.shared_fp_req.bits := DontCare
  if (sharedFPUnits.size > 0) {
    val shared_fp_arb = Module(new Arbiter(new FPInput, sharedFPUnits.size))
    for ((u, i) <- sharedFPUnits.zipWithIndex) {
      val otherUnits = sharedFPUnits.zipWithIndex.filter(_._2 != i).map(_._1)
      val other_busy = otherUnits.map(_.io_fp_active).orR
      u.io_fp_req.ready := shared_fp_arb.io.in(i).ready && !other_busy
      shared_fp_arb.io.in(i).valid := u.io_fp_req.valid && !other_busy
      shared_fp_arb.io.in(i).bits := u.io_fp_req.bits
      u.io_fp_resp := io.shared_fp_resp
    }
    io.shared_fp_req <> shared_fp_arb.io.out
  }

  val pipe_stall = WireInit(false.B)

  fus.foreach { fu =>
    fu.io.iss.op := io.iss.bits
    fu.io.iss.valid := io.iss.valid && !pipe_stall
  }

  val pipe_write_hazard = WireInit(false.B)
  val readies = fus.map(_.io.iss.ready)
  io.iss.ready := readies.orR && !pipe_write_hazard && !pipe_stall
  when (io.iss.valid) { assert(PopCount(readies) <= 1.U) }

  io.issue_pipe_latency  := Mux1H(pipe_fus.map(_.io.iss.ready), pipe_fus.map(_.depth.U))

  val pipe_write = WireInit(false.B)

  io.pipe_write.valid := false.B
  io.pipe_write.bits := DontCare
  io.iter_write.valid := false.B
  io.iter_write.bits := DontCare
  io.acc_write.valid := false.B
  io.acc_write.bits := DontCare
  io.busy := false.B
  io.set_vxsat := fus.map(_.io.set_vxsat).orR
  io.set_fflags.valid := fus.map(_.io.set_fflags.valid).orR
  io.set_fflags.bits := fus.map(f => Mux(f.io.set_fflags.valid, f.io.set_fflags.bits, 0.U)).reduce(_|_)


  val scalar_write_arb = Module(new Arbiter(new ScalarWrite, fus.size))
  scalar_write_arb.io.in.zip(fus.map(_.io.scalar_write)).foreach { case (l, r) => l <> r }
  io.scalar_write <> scalar_write_arb.io.out

  if (pipe_fus.size > 0) {
    val pipe_iss_depth = Mux1H(pipe_fus.map(_.io.iss.ready), pipe_fus.map(_.depth.U))

    val pipe_valids    = Seq.fill(pipe_depth)(RegInit(false.B))
    val pipe_sels      = Seq.fill(pipe_depth)(Reg(UInt(pipe_fus.size.W)))
    val pipe_bits      = Seq.fill(pipe_depth)(Reg(new ExecuteMicroOpWithData))
    val pipe_latencies = Seq.fill(pipe_depth)(Reg(UInt(log2Ceil(pipe_depth).W)))

    pipe_stall := Mux1H(pipe_sels.head, pipe_fus.map(_.io.pipe0_stall))

    pipe_write_hazard := (0 until pipe_depth).map { i =>
      pipe_valids(i) && pipe_latencies(i) === pipe_iss_depth
    }.orR

    val pipe_iss = io.iss.fire && pipe_fus.map(_.io.iss.ready).orR
    when (!pipe_stall) {
      pipe_valids.head := pipe_iss
      when (pipe_iss) {
        pipe_bits.head      := io.iss.bits
        pipe_latencies.head := pipe_iss_depth - 1.U
        pipe_sels.head      := VecInit(pipe_fus.map(_.io.iss.ready)).asUInt
      }
    }
    for (i <- 1 until pipe_depth) {
      val fire = pipe_valids(i-1) && pipe_latencies(i-1) =/= 0.U && !((i == 1).B && pipe_stall)
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
        fu.io.pipe(i).bits  := Mux(pipe_valids(i) && pipe_sels(i)(j),
          pipe_bits(i), 0.U.asTypeOf(new ExecuteMicroOpWithData))
      }
    }

    val write_sel = pipe_valids.zip(pipe_latencies).map { case (v,l) => v && l === 0.U }
    val fu_sel = Mux1H(write_sel, pipe_sels)
    pipe_write := write_sel.orR
    when (write_sel.orR) {
      val acc = Mux1H(write_sel, pipe_bits.map(_.acc))
      val tail = Mux1H(write_sel, pipe_bits.map(_.tail))
      io.pipe_write.valid := Mux1H(fu_sel, pipe_fus.map(_.io.write.valid)) && (!acc || tail)
      io.pipe_write.bits := Mux1H(fu_sel, pipe_fus.map(_.io.write.bits))
      io.acc_write.valid := acc && !tail
      io.acc_write.bits := Mux1H(fu_sel, pipe_fus.map(_.io.write.bits))
    }

    when (pipe_valids.orR) { io.busy := true.B }
    for (i <- 0 until pipe_depth) {
      io.pipe_hazards(i).valid       := pipe_valids(i)
      io.pipe_hazards(i).bits.eg     := pipe_bits(i).wvd_eg
      when (pipe_latencies(i) === 0.U) { // hack to deal with compress unit
        io.pipe_hazards(i).bits.eg   := Mux1H(pipe_sels(i), pipe_fus.map(_.io.write.bits.eg))
      }
      io.pipe_hazards(i).bits.latency := pipe_latencies(i)
    }
  }

  if (iter_fus.size > 0) {
    val iter_write_arb = Module(new Arbiter(new VectorWrite(dLen), iter_fus.size))
    iter_write_arb.io.in.zip(iter_fus.map(_.io.write)).foreach { case (l,r) => l <> r }
    iter_write_arb.io.out.ready := !pipe_write && io.iter_write.ready

    val acc = Mux1H(iter_write_arb.io.in.map(_.fire), iter_fus.map(_.io.acc))
    val tail = Mux1H(iter_write_arb.io.in.map(_.fire), iter_fus.map(_.io.tail))
    io.iter_write.valid     := iter_write_arb.io.out.valid && (!acc || tail) && !pipe_write
    io.iter_write.bits.eg   := iter_write_arb.io.out.bits.eg
    io.iter_write.bits.mask := iter_write_arb.io.out.bits.mask
    io.iter_write.bits.data := iter_write_arb.io.out.bits.data
    when (!pipe_write) {
      io.acc_write.valid := iter_write_arb.io.out.valid && acc
      io.acc_write.bits.eg   := Mux1H(iter_write_arb.io.in.map(_.fire), iter_fus.map(_.io.write.bits.eg))
      io.acc_write.bits.data := Mux1H(iter_write_arb.io.in.map(_.fire), iter_fus.map(_.io.write.bits.data))
      io.acc_write.bits.mask := Mux1H(iter_write_arb.io.in.map(_.fire), iter_fus.map(_.io.write.bits.mask))
    }
    when (iter_fus.map(_.io.busy).orR) { io.busy := true.B }
    for (i <- 0 until iter_fus.size) {
      io.iter_hazards(i) := iter_fus(i).io.hazard
    }
  }
}
