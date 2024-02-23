package saturn.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import saturn.common._

class ExecutionUnit(fus: Seq[FunctionalUnit])(implicit val p: Parameters) extends HasCoreParameters with HasVectorParams {
  val supported_insns = fus.map(_.supported_insns).flatten
  val pipe_fus: Seq[PipelinedFunctionalUnit] = fus.collect { case p: PipelinedFunctionalUnit => p }
  val iter_fus: Seq[IterativeFunctionalUnit] = fus.collect { case i: IterativeFunctionalUnit => i }
  // TODO for multi-VXS, differentiate between interleavable and non-interleavable FUs

  val fu_arbs = Seq.fill(fus.length)(Module(new RRArbiter(new ExecuteMicroOp, 2)))

  val pipe_depth = (pipe_fus.map(_.depth) :+ 0).max
  val nHazards = pipe_depth + iter_fus.size

  val iss = Wire(Vec(2, Decoupled(new ExecuteMicroOp)))

  val write = Wire(Valid(new VectorWrite(dLen)))
  val acc_write = Wire(Valid(new VectorWrite(dLen)))
  val vat_release = Wire(Valid(UInt(vParams.vatSz.W)))
  val hazards = Wire(Vec(nHazards, Valid(new PipeHazard)))
  val busy = Wire(Bool())

  val set_vxsat = Wire(Bool())
  val set_fflags = Wire(Valid(UInt(5.W)))
  val scalar_write = Wire(Decoupled(new ScalarWrite))
  val pipe_stall = WireInit(Vec(2, Bool(false.B))

  val pipe_write_hazard = WireInit(false.B)
  val readies = fus.map(_.io.iss.ready)
  //iss.ready := readies.orR && !pipe_write_hazard && !pipe_stall
  when (iss(0).valid || iss(1).valid) { assert(PopCount(readies) <= 1.U) }

  val vxs_0_readies = fu_arbs.map(_.io.in(0).ready)
  val vxs_1_readies = fu_arbs.map(_.io.in(1).ready)

  fus.zip(fu_arbs).foreach { case(fu, arb) =>
    arb.io.in(0) <> iss(0)
    arb.io.in(1) <> iss(1)
    arb.io.out.ready := readies.orR && !pipe_write_hazard && !pipe_stall 
    fu.io.iss.op := arb.io.out.bits
    fu.io.iss.valid := arb.io.out.valid && !pipe_stall
  }

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

  // TODO fix the pipeline stall logic for multi-VXS for potential hazards
  // Give priority to older instruction based on vat
  // Stall only if there is a write bank conflict

  if (pipe_fus.size > 0) {    

    val write_bank_conflict = iss(0).op.eg(vrfBankBits-1, 0) === iss(1).op.eg(vrfBankBits-1, 0) 
    // need to know which FU each VXS is issuing to
    // have a separate set of bits for each iss to track which FU is matched to which VXS

    //val pipe_iss_depth = Mux1H(pipe_fus.map(_.io.iss.ready), pipe_fus.map(_.depth.U))
    val pipe_ready_idx_0 = PriorityEncoder(pipe_fus.map(_.io.iss.ready).asUInt)
    val pipe_ready_idx_1 = PriorityEncoder(Reverse(pipe_fus.map(_.io.iss.ready).asUInt))

    val pipe_iss_depth_0 = Mux1H(UIntToOH(pipe_ready_idx_0, pipe_fus.map(_.depth.U)))
    val pipe_iss_depth_1 = Mux1H(UIntToOH((pipe_fus.length-1).U - pipe_ready_idx_1), pipe_fus.mapt(_.depth.U))

    val pipe_depth_conflict = (pipe_iss_depth_0 === pipe_iss_depth_1) && (pipe_ready_idx_0 =/= (pipe_fus.length-1).U - pipe_ready_idx_1)
    val vxs_issue_conflict = write_bank_conflict && pipe_depth_conflict

    val pipe_valids    = Seq.fill(pipe_depth)(RegInit(false.B))

    val pipe_sels      = Seq.fill(pipe_depth)(Reg(UInt(pipe_fus.size.W)))
    val pipe_bits      = Seq.fill(pipe_depth)(Reg(new ExecuteMicroOp))
    val pipe_latencies = Seq.fill(pipe_depth)(Reg(UInt(log2Ceil(pipe_depth).W)))

    pipe_stall := Mux1H(pipe_sels.head, pipe_fus.map(_.io.pipe0_stall))

    pipe_stall(0) := 

    pipe_write_hazard := (0 until pipe_depth).map { i =>
      pipe_valids(i) && (pipe_latencies(i) === pipe_iss_depth_0 || pipe_latencies(i) === pipe_iss_depth_1)
    }.orR

    val pipe_iss = (iss(0).fire || iss(1).fire) && pipe_fus.map(_.io.iss.ready).orR
    when (!pipe_stall) {
      pipe_valids.head := pipe_iss
      when (pipe_iss) {
        pipe_bits.head      := Mux(iss(0).fire(), iss(0).bits, iss(1).bits)
        //pipe_bits.head      := iss.bits
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
          pipe_bits(i), 0.U.asTypeOf(new ExecuteMicroOp))
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
      when (pipe_latencies(i) === 0.U) { // hack to deal with compress unit
        hazards(i).bits.eg   := Mux1H(pipe_sels(i), pipe_fus.map(_.io.write.bits.eg))
      }
    }
  }

  if (iter_fus.size > 0) {
    val iter_write_arb = Module(new Arbiter(new VectorWrite(dLen), iter_fus.size))
    iter_write_arb.io.in.zip(iter_fus.map(_.io.write)).foreach { case (l,r) => l <> r }
    iter_write_arb.io.out.ready := !pipe_write

    when (!pipe_write) {
      val acc = Mux1H(iter_write_arb.io.in.map(_.fire()), iter_fus.map(_.io.acc))
      val tail = Mux1H(iter_write_arb.io.in.map(_.fire()), iter_fus.map(_.io.tail))
      write.valid     := iter_write_arb.io.out.valid && (!acc || tail)
      write.bits.eg   := iter_write_arb.io.out.bits.eg
      write.bits.mask := iter_write_arb.io.out.bits.mask
      write.bits.data := iter_write_arb.io.out.bits.data
      acc_write.valid := iter_write_arb.io.out.valid && acc
      acc_write.bits.eg   := Mux1H(iter_write_arb.io.in.map(_.fire()), iter_fus.map(_.io.write.bits.eg))
      acc_write.bits.data := Mux1H(iter_write_arb.io.in.map(_.fire()), iter_fus.map(_.io.write.bits.data))
      acc_write.bits.mask := Mux1H(iter_write_arb.io.in.map(_.fire()), iter_fus.map(_.io.write.bits.mask))
      vat_release.valid := iter_write_arb.io.out.fire() && Mux1H(iter_write_arb.io.in.map(_.ready), iter_fus.map(_.io.vat.valid))
      vat_release.bits  := Mux1H(iter_write_arb.io.in.map(_.fire()), iter_fus.map(_.io.vat.bits))
    }
    when (iter_fus.map(_.io.busy).orR) { busy := true.B }
    for (i <- 0 until iter_fus.size) {
      hazards(i+pipe_depth) := iter_fus(i).io.hazard
    }
  }
}
