package saturn.backend

import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import saturn.common._

class IssueQueue(depth: Int, nSeqs: Int)(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {

  val io = IO(new Bundle {
    val enq = Flipped(Decoupled(new IssueQueueInst(nSeqs)))
    val deq = Decoupled(new IssueQueueInst(nSeqs))
    val hazards = Output(Vec(depth, Valid(new InstructionHazard)))
  })

  private val all_vreg_arch_mask = VecInit((0 until 32).map{i => (1.U(32.W) << i.U(5.W))}) // @@@@ 모든 vreg의 마스크를 미리 만든다.
  private val ARCH_W = all_vreg_arch_mask.head.getWidth.W // @@@@ 마스크 비트폭을 getwidth로 추정
  
  if (depth > 0) {
    val q = Module(new DCEQueue(new IssueQueueInst(nSeqs), depth, pipe=true))
    q.io.enq <> io.enq
    io.deq <> q.io.deq

    q.io.peek.zip(io.hazards).foreach { case (e,h) =>
      h.valid    := e.valid
      h.bits.vat := e.bits.vat
      val rs2 = Mux(e.bits.rs1_is_rs2, e.bits.rs1, e.bits.rs2)
      val lo = Mux(rs2 <= e.bits.rd, rs2, e.bits.rd) // @@@@ min(rs2, rd)
      val hi = Mux(rs2 <= e.bits.rd, e.bits.rd, rs2) // @@@@ max(rs2, rd)
      /*@@@@ Custom*/
      var span_mask = 0.U(ARCH_W)
      for (i <- 0 until 32) {
        val inRange = (i.U >= lo) && (i.U <= hi)
        span_mask = span_mask | (Mux(inRange, all_vreg_arch_mask(i), 0.U))
      }
      val only_writes_vd0 = e.bits.scalar_to_vd0 || e.bits.reduction
      val vd_lmul  = Mux(only_writes_vd0      , 0.U, e.bits.emul +& e.bits.wide_vd +& e.bits.nf_log2)
      val vs1_lmul = Mux(e.bits.reads_vs1_mask, 0.U, e.bits.emul)
      val vs2_lmul = Mux(e.bits.reads_vs2_mask, 0.U, e.bits.emul +& e.bits.wide_vs2 +& e.bits.nf_log2)
      val vd_arch_mask  = get_arch_mask(e.bits.rd , vd_lmul )
      val vs1_arch_mask = get_arch_mask(e.bits.rs1, vs1_lmul)
      val vs2_arch_mask = get_arch_mask(rs2       , vs2_lmul)
      val pid_flag = e.bits.pid_flag // @@@@ pid 플래그 on되었을 때만 intent에 pid 내용 반영
      val rintent_base = Seq( // h.bits.rintent가 그냥 Seq으로 정의되어 있었는데 내가 base를 만들어서 Mux시킴
        (e.bits.renv1, vs1_arch_mask),
        (e.bits.renv2, vs2_arch_mask),
        (e.bits.renvd, vd_arch_mask),
        (e.bits.renvm, 1.U)
      ).map(t => Mux(t._1, t._2, 0.U)).reduce(_|_)
      h.bits.rintent := Mux(pid_flag, rintent_base | span_mask, rintent_base) // @@@@ pid 플래그가 켜져있으면 span_mask를 intent에 포함
      val wintent_base = Mux(e.bits.wvd, vd_arch_mask, 0.U) // 원래 h.bits.wintent임
      h.bits.wintent := Mux(pid_flag, span_mask, wintent_base) // @@@@ pid 플래그 켜져있으면 span_mask를 wintent로
    }
  } else {
    io.deq <> io.enq
  }
}
