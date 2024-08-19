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

  val maxPipeDepth = (pipe_fus.map(_.depth) :+ 0).max

  val io = IO(new Bundle {
    val iss = Flipped(Decoupled(new ExecuteMicroOpWithData))
    val iter_hazards = Output(Vec(iter_fus.size, Valid(new PipeHazard(maxPipeDepth))))
    val iter_write = Decoupled(new VectorWrite(dLen))
    val pipe_write = Output(Valid(new VectorWrite(dLen)))
    val acc_write = Output(Valid(new VectorWrite(dLen)))
    val scalar_write = Decoupled(new ScalarWrite)

    val pipe_hazards = Output(Vec(maxPipeDepth, Valid(new PipeHazard(maxPipeDepth))))

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
    val pipe_fp_units = sharedFPUnits.collect { case p: PipelinedFunctionalUnit with HasSharedFPUIO => p }
    val iter_fp_units = sharedFPUnits.collect { case i: IterativeFunctionalUnit with HasSharedFPUIO => i }
    require(pipe_fp_units.size <= 1 && iter_fp_units.size <= 1)
    val pu = pipe_fp_units.headOption
    val iu = iter_fp_units.headOption

    if (pu.isEmpty) {
      iu.foreach { iu => io.shared_fp_req <> iu.io_fp_req }
    } else {
      pu.map { pu =>
        io.shared_fp_req <> pu.io_fp_req
        iu.foreach { iu =>
          val use_iu = !pu.io.pipe.map(_.valid).orR && iu.io.busy
          val use_pu = !iu.io.busy
          io.shared_fp_req.valid := (use_iu && iu.io_fp_req.valid) || (use_pu && pu.io_fp_req.valid)
          io.shared_fp_req.bits := Mux1H(Seq(
            use_iu -> iu.io_fp_req.bits,
            use_pu -> pu.io_fp_req.bits
          ))
          iu.io_fp_req.ready := use_iu && io.shared_fp_req.ready
          pu.io_fp_req.ready := use_pu && io.shared_fp_req.ready
        }
      }
    }
    sharedFPUnits.foreach(_.io_fp_resp := io.shared_fp_resp)
  }

  val readies = fus.map(_.io.iss.ready)
  io.iss.ready := readies.orR
  fus.foreach { fu =>
    fu.io.iss.op := io.iss.bits
    fu.io.iss.valid := io.iss.valid
  }

  when (io.iss.valid) { assert(PopCount(readies) <= 1.U) }

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

    val pipe_valids    = Seq.fill(maxPipeDepth)(RegInit(false.B))
    val pipe_sels      = Seq.fill(maxPipeDepth)(Reg(UInt(pipe_fus.size.W)))
    val pipe_bits      = Seq.fill(maxPipeDepth)(Reg(new ExecuteMicroOpWithData))

    val pipe_iss = io.iss.fire && pipe_fus.map(_.io.iss.ready).orR
    pipe_valids.head := pipe_iss
    when (pipe_iss) {
      pipe_bits.head      := io.iss.bits
      pipe_sels.head      := VecInit(pipe_fus.map(_.io.iss.ready)).asUInt
    }

    for (i <- 1 until maxPipeDepth) {
      val fire = pipe_valids(i-1)
      pipe_valids(i) := fire
      when (fire) {
        pipe_bits(i)      := pipe_bits(i-1)
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

    val write_sel = pipe_fus.map(_.io.pipe.last.valid)
    assert(PopCount(write_sel) <= 1.U)
    when (write_sel.orR) {
      val acc = Mux1H(write_sel, pipe_fus.map(_.io.pipe.last.bits.acc))
      val tail = Mux1H(write_sel, pipe_fus.map(_.io.pipe.last.bits.tail))
      io.pipe_write.valid := Mux1H(write_sel, pipe_fus.map(_.io.write.valid)) && (!acc || tail)
      io.pipe_write.bits := Mux1H(write_sel, pipe_fus.map(_.io.write.bits))
      io.acc_write.valid := acc && !tail
      io.acc_write.bits := Mux1H(write_sel, pipe_fus.map(_.io.write.bits))
    }

    when (pipe_valids.orR) { io.busy := true.B }
    for (i <- 0 until maxPipeDepth) {
      io.pipe_hazards(i).valid       := pipe_valids(i)
      io.pipe_hazards(i).bits.eg     := pipe_bits(i).wvd_eg
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
