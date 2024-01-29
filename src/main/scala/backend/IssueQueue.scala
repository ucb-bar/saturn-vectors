package saturn.backend

import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import saturn.common._

class IssueQueue(depth: Int)(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {

  val io = IO(new Bundle {
    val enq = Flipped(Decoupled(new BackendIssueInst))
    val deq = Decoupled(new BackendIssueInst)
    val hazards = Output(Vec(depth, Valid(new InstructionHazard)))
  })

  if (depth > 0) {
    val q = Module(new DCEQueue(new BackendIssueInst, depth, pipe=true))
    q.io.enq <> io.enq
    io.deq <> q.io.deq

    q.io.peek.zip(io.hazards).foreach { case (e,h) =>
      h.valid    := e.valid
      h.bits.vat := e.bits.vat
      val rs2 = Mux(e.bits.rs1_is_rs2, e.bits.rs1, e.bits.rs2)
      val only_writes_vd0 = e.bits.scalar_to_vd0 || e.bits.reduction
      val vd_lmul  = Mux(only_writes_vd0      , 0.U, e.bits.emul +& e.bits.wide_vd +& e.bits.nf_log2)
      val vs1_lmul = Mux(e.bits.reads_vs1_mask, 0.U, e.bits.emul)
      val vs2_lmul = Mux(e.bits.reads_vs2_mask, 0.U, e.bits.emul +& e.bits.wide_vs2 +& e.bits.nf_log2)
      val vd_arch_mask  = get_arch_mask(e.bits.rd , vd_lmul )
      val vs1_arch_mask = get_arch_mask(e.bits.rs1, vs1_lmul)
      val vs2_arch_mask = get_arch_mask(rs2       , vs2_lmul)
      h.bits.rintent := Seq(
        (e.bits.renv1, vs1_arch_mask),
        (e.bits.renv2, vs2_arch_mask),
        (e.bits.renv2, vd_arch_mask),
        (e.bits.renvm, 1.U)
      ).map(t => Mux(t._1, t._2, 0.U)).reduce(_|_)
      h.bits.wintent := Mux(e.bits.wvd, vd_arch_mask, 0.U)
    }
  } else {
    io.deq <> io.enq
  }
}
