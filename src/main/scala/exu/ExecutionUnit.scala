package saturn.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import saturn.common._

class ExecutionUnit(num_vxs: Int, vxs_supported_insns: Seq[VectorInstruction], fus: Seq[FunctionalUnit])(implicit val p: Parameters) extends HasCoreParameters with HasVectorParams {
  val supported_insns = fus.map(_.supported_insns).flatten
  val pipe_fus: Seq[PipelinedFunctionalUnit] = fus.collect { case p: PipelinedFunctionalUnit => p }
  val iter_fus: Seq[IterativeFunctionalUnit] = fus.collect { case i: IterativeFunctionalUnit => i }
  // TODO for multi-VXS, differentiate between interleavable and non-interleavable FUs
  // Account for this by having only one VXS that supports the mask ops and permute ops

  // TODO do we need to account for a hazard in the backend issq hazard logic for when an op is issued to one the VXS, and it is stalled 
  // and not yet issued to the pipelines? (ie. is the hazard still captured if it's issued to the VXS but not yet issued to the FUs)
  // It should already be accounted for, but this should be checked

  val pipe_depth = (pipe_fus.map(_.depth) :+ 0).max
  val nHazards = pipe_depth + iter_fus.size

  val iss = Wire(Vec(num_vxs, Decoupled(new ExecuteMicroOp)))

  val writes = Wire(Vec(num_vxs, Valid(new VectorWrite(dLen))))
  val acc_write = Wire(Vec(num_vxs, Valid(new VectorWrite(dLen))))
  val vat_release = Wire(Vec(num_vxs, Valid(UInt(vParams.vatSz.W))))
  val hazards = Wire(Vec(nHazards, Valid(new PipeHazard)))
  val busy = Wire(Bool())

  val set_vxsat = Wire(Bool())
  val set_fflags = Wire(Valid(UInt(5.W)))
  val scalar_write = Wire(Decoupled(new ScalarWrite))  // We'll only have one VXS be capable of using this port

  val pipe_inflight_hazard = WireInit(Vec(num_vxs, Bool(false.B)))  // Conflict with an inflight op in a pipelined FU 
  val pipe_issue_hazard = WireInit(Vec(num_vxs, Bool(false.B)))     // Conflict with another issuing VXS
  val pipe_backpressure = WireInit(Vec(num_vxs, Bool(false.B)))     // Backpressure from an FU to stall issue

  val vxs_readies = Seq.fill(num_vxs)(WireInit(Vec(fus.length, Bool(false.B))))
  val fu_arbs = Seq.fill(fus.length)(Module(new RRArbiter(new ExecuteMicroOp, num_vxs)))

  iss.zipWithIndex.foreach{ case(issue, i) => 
    issue.ready := vxs_readies(i).orR && !pipe_inflight_hazard(i) && !pipe_issue_hazard(i)
    when (issue.valid) { assert(PopCount(vxs_readies(i)) === 1.U) })
  }

  val vxs_fu_indexes = vxs_readies.map(PriorityEncoder(_.toSeq))

  // New code for general number of VXS units
  fus.zip(fu_arbs).zipWithIndex.foreach { case((fu, arb), i) =>
    // Match the FU to the VXS units that support it
    val matching_vxs_units = vxs_supported_insns.map(fu.supported_insns.forall(_.contains)) 
    require(matching_vxs_units.length > 0)

    matching_vxs_units.zipWithIndex.foreach { case(supports, j) =>
      if (supports) {
        arb.io.in(j).valid := iss(j).valid
        arb.io.in(j).bits  := iss(j).bits
        vxs_readies(j)(i)  := iss(j).ready 
      } else {
        arb.io.in(j).valid := false.B
        arb.io.in(j).bits  := DontCare
        vxs_readies(j)(i)  := false.B
      }
    }

    arb.io.out.ready := fu.io.iss.ready && !fu_hazard && !fu_backpressure

    fu.io.iss.op    := arb.io.out.bits
    fu.io.iss.valid := arb.io.out.valid && !fu_hazard && !fu_backpressure
  }

  val pipe_writes = WireInit(Vec(num_vxs, Bool(false.B)))

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
    val pipe_valids     = Seq.fill(num_vxs)(Seq.fill(pipe_depth)(RegInit(false.B)))
    val pipe_sels       = Seq.fill(num_vxs)(Seq.fill(pipe_depth)(Reg(UInt(pipe_fus.size.W))))
    val pipe_bits       = Seq.fill(num_vxs)(Seq.fill(pipe_depth)(Reg(new ExecuteMicroOp)))
    val pipe_latencies  = Seq.fill(num_vxs)(Seq.fill(pipe_depth)(Reg(UInt(log2Ceil(pipe_depth).W))))

    // Find the pipe depth for each of the current VXS issues
    val pipe_fu_depths = vxu_fu_indexes.map(Mux1H(UIntToOH(_), pipe_fus.map(fus(_).depth.U)) 
    val vxs_vrf_banks  = iss.map(_.op.eg(vrfBankBits-1,0))

    for ((exec_seq, i) <- iss.zipWithIndex) {
      // Find if either conflicts with an in-flight instruction
      pipe_inflight_hazard(i) := (0 until num_vxs).map{ j => 
        (0 until pipe_depth).map { k =>
          pipe_valids(j)(k) && (pipe_latencies(j)(k) === pipe_fu_depths(i)) && (vxs_vrf_banks(i) === pipe_bits(j)(k).eg(vrfBankBits-1,0))  
        }.reduce(_ || _)
      }.reduce(_ || _)

      // Now check against the other iss
      // If an iss is blocked by an in-flight instruction, it should not block another iss
      val other_vxs = (0 until num_vxs).filter(_ != i)
      // both are valid, not the same fu, same pipe latency, same vrf write bank, older vat
      pipe_issue_hazard(i) := !pipe_inflight_hazard(i) && other_vxs.map{ other_iss_idx =>
        (iss(i).valid && other_iss.valid) && vxs_readies(other_iss_idx).orR && (pipe_fu_depths(i) === pipe_fu_depths(other_iss_idx)) && 
        (vxs_vrf_banks(i) === vxs_vrf_banks(other_iss_idx)) && vatOlder(iss(other_iss_idx).op.vat, iss(i).op.vat) 
      }.reduce(_ || _)
    }

    //pipe_stall(0) := Mux1H(pipe_ready_idx_0, pipe_fus.map(_.io.pipe0_stall)) 

    //val pipe_iss = (iss(0).fire || iss(1).fire) && pipe_fus.map(_.io.iss.ready).orR

    val pipe_iss = iss.map(_.fire)
    
    for (i <- (0 until num_vxs)) {
      pipe_valids(i).head := pipe_iss(i)
      when(pipe_iss(i)) {
        pipe_bits(i).head := iss(i).bits
        pipe_latencies(i).head := pipe_fu_depths(i) - 1.U
        pipe_sels(i).head := vxs_readies(i)
      }

      for (j <- 1 until pipe_depth) {
        val fire = pipe_valids(i)(j-1) && pipe_latencies(i)(j-1) =/= 0.U //&& !((i == 1).B && pipe_stall)
        pipe_valids(i)(j) := fire
        when (fire) {
          pipe_bits(i)(j)       := pipe_bits(i)(j-1)
          pipe_latencies(i)(j)  := pipe_latencies(i)(j-1) - 1.U
          pipe_sels(i)(j)       := pipe_sels(i)(j-1)
        }
      }
    }

    //when (!pipe_stall) {

    for ((fu, j) <- pipe_fus.zipWithIndex) {
      for (i <- 0 until fu.depth) {
        val vxs_selects = pipe_valids.zip(pipe_sels).map(case(v, s) => v(i) && s(i)(j))  // This FU is selected only if one of the VXS pipes is valid and the FU is selected
        val vxs_selected = vxs_selects.reduce(_ || _)
        fu.io.pipe(i).valid := vxs_selected
        fu.io.pipe(i).bits  := Mux(vxs_selected, pipe_bits(PriorityEncoder(vxs_selects)(i)), 0.U.asTypeOf(new ExecuteMicroOp))
      }
    }

    writes.zipWithIndex.foreach { case(write, i) =>
      val write_sel = pipe_valids(i).zip(pipe_latencies(i)).map { case(v, l) => v && l === 0.U }
      val fu_sel = Mux1H(write_sel, pipe_sels(i))
      pipe_writes(i) := write_sel.orR
      when (write_sel.orR) {
        val acc = Mux1H(write_sel, pipe_bits(i).map(_.acc))
        val tail = Mux1H(write_sel, pipe_bits(i).map(_.tail))
        write(i).valid := Mux1H(fu_sel, pipe_fus.map(_.io.write.valid)) && (!acc || tail)
        write(i).bits := Mux1H(fu_sel, pipe_fus.map(_.io.write.bits))
        acc_write(i).valid := acc && !tail
        acc_write(i).bits := Mux1H(fu_sel, pipe_fus.map(_.io.write.bits))
        vat_release(i).valid := Mux1H(write_sel, pipe_bits(i).map(_.tail))
        vat_release(i).bits := Mux1H(write_sel, pipe_bits(i).map(_.vat))
      }
    }


    //val write_sel = pipe_valids.zip(pipe_latencies).map { case (v,l) => v && l === 0.U }
    //val fu_sel = Mux1H(write_sel, pipe_sels)
    //pipe_write := write_sel.orR
    //when (write_sel.orR) {
    //  val acc = Mux1H(write_sel, pipe_bits.map(_.acc))
    //  val tail = Mux1H(write_sel, pipe_bits.map(_.tail))
    //  write.valid := Mux1H(fu_sel, pipe_fus.map(_.io.write.valid)) && (!acc || tail)
    //  write.bits := Mux1H(fu_sel, pipe_fus.map(_.io.write.bits))
    //  acc_write.valid := acc && !tail
    //  acc_write.bits := Mux1H(fu_sel, pipe_fus.map(_.io.write.bits))
    //  vat_release.valid := Mux1H(write_sel, pipe_bits.map(_.tail))
    //  vat_release.bits := Mux1H(write_sel, pipe_bits.map(_.vat))
    //}

    when (pipe_valids.map(_.orR).reduce(_ || _)) { busy := true.B }
    for (i <- 0 until pipe_depth) {
      hazards(i).valid       := pipe_valids.map(_(i)).reduce(_ || _)
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
