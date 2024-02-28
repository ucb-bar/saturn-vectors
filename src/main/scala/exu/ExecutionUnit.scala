package saturn.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import saturn.common._
import saturn.insns._
import saturn.backend._

class ExecutionUnit(num_vxs: Int, vxs_supported_insns: Seq[Seq[VectorInstruction]], fus: Seq[FunctionalUnit])(implicit val p: Parameters) extends HasCoreParameters with HasVectorParams {
  val supported_insns = fus.map(_.supported_insns).flatten
  val pipe_fus: Seq[PipelinedFunctionalUnit] = fus.collect { case p: PipelinedFunctionalUnit => p }
  val iter_fus: Seq[IterativeFunctionalUnit] = fus.collect { case i: IterativeFunctionalUnit => i }
  // TODO for multi-VXS, differentiate between interleavable and non-interleavable FUs
  // Account for this by having only one VXS that supports the mask ops and permute ops

  // TODO do we need to account for a hazard in the backend issq hazard logic for when an op is issued to one the VXS, and it is stalled 
  // and not yet issued to the pipelines? (ie. is the hazard still captured if it's issued to the VXS but not yet issued to the FUs)
  // It should already be accounted for, but this should be checked

  val pipe_depth = (pipe_fus.map(_.depth) :+ 0).max
  val nHazards = (num_vxs * pipe_depth) + iter_fus.size

  val vat_tail_vxu = Wire(UInt(vParams.vatSz.W))
  def vatOlder(i0: UInt, i1: UInt) = cqOlder(i0, i1, vat_tail_vxu)

  val iss = Wire(Vec(num_vxs, Decoupled(new ExecuteMicroOp)))

  val writes = Wire(Vec(num_vxs, Valid(new VectorWrite(dLen))))
  val acc_writes = Wire(Vec(num_vxs, Valid(new VectorWrite(dLen))))
  val vat_releases = Wire(Vec(num_vxs, Valid(UInt(vParams.vatSz.W))))
  val hazards = Wire(Vec(nHazards, Valid(new PipeHazard)))
  val busy = Wire(Bool())

  val set_vxsat = Wire(Bool())
  val set_fflags = Wire(Valid(UInt(5.W)))
  val scalar_write = Wire(Decoupled(new ScalarWrite))  // We'll only have one VXS be capable of using this port


  val pipe_inflight_hazard = Seq.fill(num_vxs)(WireInit(false.B))  // Conflict with an inflight op in a pipelined FU 
  val pipe_issue_hazard = Seq.fill(num_vxs)(WireInit(false.B))     // Conflict with another issuing VXS
  val pipe_backpressure = Seq.fill(num_vxs)(WireInit(false.B))     // Backpressure from an FU to stall issue
  //val pipe_inflight_hazard = WireInit(Vec(num_vxs, Bool(false.B)))  // Conflict with an inflight op in a pipelined FU 
  //val pipe_issue_hazard = WireInit(Vec(num_vxs, Bool(false.B)))     // Conflict with another issuing VXS
  //val pipe_backpressure = WireInit(Vec(num_vxs, Bool(false.B)))     // Backpressure from an FU to stall issue

  val vxs_readies = Seq.fill(num_vxs)(Seq.fill(fus.length)(WireInit(false.B)))

  iss.zipWithIndex.foreach{ case(issue, i) => 
    issue.ready := vxs_readies(i).orR && !pipe_inflight_hazard(i) && !pipe_issue_hazard(i) && !pipe_backpressure(i)
    when (issue.valid) { assert(PopCount(vxs_readies(i)) === 1.U) }
  }

  fus.zipWithIndex.foreach { case(fu, i) =>
    // Match the FU to the VXS units that support it
    val matching_vxs_seq = vxs_supported_insns.zipWithIndex.filter { case(vxs_insns, j) => fu.supported_insns.forall(vxs_insns.contains) }.map(_._2)
    require(matching_vxs_seq.length == 1) // FUs are exclusive for now
    val matching_vxs = matching_vxs_seq(0)

    vxs_readies(matching_vxs)(i) := fu.io.iss.ready
    fu.io.iss.valid := iss(matching_vxs).valid && !pipe_inflight_hazard(matching_vxs) && !pipe_issue_hazard(matching_vxs) && !pipe_backpressure(matching_vxs)
    fu.io.iss.op := iss(matching_vxs).bits
  }

  val pipe_writes = Seq.fill(num_vxs)(WireInit(false.B))

  vat_releases.foreach { rel => 
    rel.valid := false.B 
    rel.bits  := DontCare
  }

  writes.foreach { write =>
    write.valid := false.B
    write.bits  := DontCare
  }

  acc_writes.foreach { acc_write =>
    acc_write.valid := false.B
    acc_write.bits  := DontCare
  }

  busy := false.B
  set_vxsat := fus.map(_.io.set_vxsat).orR
  set_fflags.valid := fus.map(_.io.set_fflags.valid).orR
  set_fflags.bits := fus.map(f => Mux(f.io.set_fflags.valid, f.io.set_fflags.bits, 0.U)).reduce(_|_)

  val scalar_write_arb = Module(new Arbiter(new ScalarWrite, fus.size))
  scalar_write_arb.io.in.zip(fus.map(_.io.scalar_write)).foreach { case (l, r) => l <> r }
  scalar_write <> scalar_write_arb.io.out

  if (pipe_fus.size > 0) { 
    val pipe_iss_depths = vxs_readies.map(Mux1H(_, pipe_fus.map(_.depth.U)))
    val vxs_vrf_banks   = iss.map(_.bits.wvd_eg(vrfBankBits-1,0))

    val pipe_valids     = Seq.fill(num_vxs)(Seq.fill(pipe_depth)(RegInit(false.B)))
    val pipe_sels       = Seq.fill(num_vxs)(Seq.fill(pipe_depth)(Reg(UInt(pipe_fus.size.W))))
    val pipe_bits       = Seq.fill(num_vxs)(Seq.fill(pipe_depth)(Reg(new ExecuteMicroOp)))
    val pipe_latencies  = Seq.fill(num_vxs)(Seq.fill(pipe_depth)(Reg(UInt(log2Ceil(pipe_depth).W))))

    for (i <- 0 until num_vxs) {
      pipe_backpressure(i) := Mux1H(pipe_sels(i).head, pipe_fus.map(_.io.pipe0_stall))
    }

    for ((exec_seq, i) <- iss.zipWithIndex) {
      // Find if either conflicts with an in-flight instruction
      pipe_inflight_hazard(i) := (0 until num_vxs).map{ j => 
        (0 until pipe_depth).map { k =>
          pipe_valids(j)(k) && (pipe_latencies(j)(k) === pipe_iss_depths(i)) && (vxs_vrf_banks(i) === pipe_bits(j)(k).wvd_eg(vrfBankBits-1,0))  
        }.reduce(_ || _)
      }.reduce(_ || _)

      // Now check against the other iss
      // If an iss is blocked by an in-flight instruction, it should not block another iss
      val other_vxs = (0 until num_vxs).filter(_ != i)
      // both are valid, not the same fu, same pipe latency, same vrf write bank, older vat
      pipe_issue_hazard(i) := !pipe_inflight_hazard(i) && other_vxs.map{ other_iss_idx =>
        (iss(i).valid && iss(other_iss_idx).valid) && vxs_readies(other_iss_idx).orR && (pipe_iss_depths(i) === pipe_iss_depths(other_iss_idx)) && 
        (vxs_vrf_banks(i) === vxs_vrf_banks(other_iss_idx)) && vatOlder(iss(other_iss_idx).bits.vat, iss(i).bits.vat) 
      }.reduce(_ || _)
    }


    //val pipe_iss = (iss(0).fire || iss(1).fire) && pipe_fus.map(_.io.iss.ready).orR
    // TODO check that I don't need to do the pipe_fus. ready.orR check here
    // it should be taken care of in the issue logic above, so iss.fire should be sufficient
    val pipe_iss = iss.map(_.fire)
    
    for (i <- (0 until num_vxs)) {
      when (!pipe_backpressure(i)) {
        pipe_valids(i).head := pipe_iss(i)
        when(pipe_iss(i)) {
          pipe_bits(i).head       := iss(i).bits
          pipe_latencies(i).head  := pipe_iss_depths(i) - 1.U 
          pipe_sels(i).head       := vxs_readies(i).asUInt
        }
      }
      
      for (j <- 1 until pipe_depth) {
        val fire = pipe_valids(i)(j-1) && pipe_latencies(i)(j-1) =/= 0.U && !((j == 1).B && pipe_backpressure(i))
        pipe_valids(i)(j) := fire
        when (fire) {
          pipe_bits(i)(j)       := pipe_bits(i)(j-1)
          pipe_latencies(i)(j)  := pipe_latencies(i)(j-1) - 1.U
          pipe_sels(i)(j)       := pipe_sels(i)(j-1)
        }
      }
    }

    for ((fu, i) <- pipe_fus.zipWithIndex) {
      val matching_vxs_seq = vxs_supported_insns.zipWithIndex.filter { case(vxs_insns, j) => fu.supported_insns.forall(vxs_insns.contains) }.map(_._2)
      require(matching_vxs_seq.length == 1) // FUs are exclusive for now
      val matching_vxs = matching_vxs_seq(0)

      for (j <- 0 until fu.depth) {
        fu.io.pipe(j).valid := pipe_valids(matching_vxs)(j) && pipe_sels(matching_vxs)(i)(j)
        fu.io.pipe(j).bits  := Mux(pipe_valids(matching_vxs)(j) && pipe_sels(matching_vxs)(i)(j),
                                   pipe_bits(matching_vxs)(j), 0.U.asTypeOf(new ExecuteMicroOp))
      }
    }

    writes.zipWithIndex.foreach { case(write, i) =>
      val write_sel = pipe_valids(i).zip(pipe_latencies(i)).map { case(v, l) => v && l === 0.U }
      val fu_sel = Mux1H(write_sel, pipe_sels(i))
      pipe_writes(i) := write_sel.orR
      when (write_sel.orR) {
        val acc = Mux1H(write_sel, pipe_bits(i).map(_.acc))
        val tail = Mux1H(write_sel, pipe_bits(i).map(_.tail))
        writes(i).valid := Mux1H(fu_sel, pipe_fus.map(_.io.write.valid)) && (!acc || tail)
        writes(i).bits := Mux1H(fu_sel, pipe_fus.map(_.io.write.bits))
        acc_writes(i).valid := acc && !tail
        acc_writes(i).bits := Mux1H(fu_sel, pipe_fus.map(_.io.write.bits))
        vat_releases(i).valid := Mux1H(write_sel, pipe_bits(i).map(_.tail))
        vat_releases(i).bits := Mux1H(write_sel, pipe_bits(i).map(_.vat))
      }
    }

    when (pipe_valids.map(_.orR).reduce(_ || _)) { busy := true.B }
    for (i <- 0 until num_vxs) {
      for (j <- 0 until pipe_depth) {
        hazards((i*pipe_depth)+j)     := pipe_valids(i)(j)
        hazards((i*pipe_depth)+j)     := pipe_bits(i)(j).vat
        hazards((i*pipe_depth)+j)     := pipe_bits(i)(j).wvd_eg
        when(pipe_latencies(i)(j) === 0.U) { // hack to deal with compress unit
          hazards((i*pipe_depth)+j).bits.eg   := Mux1H(pipe_sels(i)(j), pipe_fus.map(_.io.write.bits.eg))
        }
      }
    }
  }

  if (iter_fus.size > 0) {
    val iter_write_arb = Module(new Arbiter(new VectorWrite(dLen), iter_fus.size))
    iter_write_arb.io.in.zip(iter_fus.map(_.io.write)).foreach { case (l,r) => l <> r }
    iter_write_arb.io.out.ready := !pipe_writes.reduce(_ || _)

    when (!pipe_writes.reduce(_ || _)) { // just use the 0th write port for now
      val acc = Mux1H(iter_write_arb.io.in.map(_.fire()), iter_fus.map(_.io.acc))
      val tail = Mux1H(iter_write_arb.io.in.map(_.fire()), iter_fus.map(_.io.tail))
      writes(0).valid     := iter_write_arb.io.out.valid && (!acc || tail)
      writes(0).bits.eg   := iter_write_arb.io.out.bits.eg
      writes(0).bits.mask := iter_write_arb.io.out.bits.mask
      writes(0).bits.data := iter_write_arb.io.out.bits.data
      acc_writes(0).valid := iter_write_arb.io.out.valid && acc
      acc_writes(0).bits.eg   := Mux1H(iter_write_arb.io.in.map(_.fire()), iter_fus.map(_.io.write.bits.eg))
      acc_writes(0).bits.data := Mux1H(iter_write_arb.io.in.map(_.fire()), iter_fus.map(_.io.write.bits.data))
      acc_writes(0).bits.mask := Mux1H(iter_write_arb.io.in.map(_.fire()), iter_fus.map(_.io.write.bits.mask))
      vat_releases(0).valid := iter_write_arb.io.out.fire() && Mux1H(iter_write_arb.io.in.map(_.ready), iter_fus.map(_.io.vat.valid))
      vat_releases(0).bits  := Mux1H(iter_write_arb.io.in.map(_.fire()), iter_fus.map(_.io.vat.bits))
    }
    when (iter_fus.map(_.io.busy).orR) { busy := true.B }
    for (i <- 0 until iter_fus.size) {
      hazards(i+(num_vxs * pipe_depth)) := iter_fus(i).io.hazard
    }
  }
}
