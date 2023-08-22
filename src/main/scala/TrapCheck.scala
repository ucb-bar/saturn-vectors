package vref

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

class VREFFrontendTrapCheck(val params: VREFVectorParams)(implicit p: Parameters) extends CoreModule()(p) with VectorConsts {
  val io = IO(new Bundle {
    val core = new VectorCoreIO
    val tlb = Flipped(new DCacheTLBPort)

    val issue = Valid(new VectorIssueInst(params))
    val issue_credits = Input(UInt())
    val inflight_mem = Input(Bool())
    val resetting = Input(Bool())

    val vm = Input(UInt(maxVLMax.W))
    val vm_ready = Input(Bool())
  })

  // X stage
  val x_replay = RegInit(false.B)
  val x_replay_inst = Reg(new VectorIssueInst(params))
  val x_replay_eidx = Reg(UInt(log2Ceil(maxVLMax).W))
  val x_replay_addr = Reg(UInt(vaddrBitsExtended.W))
  val x_replay_pc = Reg(UInt(vaddrBitsExtended.W))
  val x_replay_stride = Reg(UInt(vaddrBitsExtended.W))

  val x_core_inst = Wire(new VectorIssueInst(params))
  x_core_inst.bits := io.core.ex.inst
  x_core_inst.vconfig := io.core.ex.vconfig
  x_core_inst.vstart := io.core.ex.vstart
  x_core_inst.rs1_data := io.core.ex.rs1
  x_core_inst.rs2_data := io.core.ex.rs2
  x_core_inst.vat := DontCare

  val x_inst = Mux(x_replay, x_replay_inst, x_core_inst)
  val x_addr = Mux(x_replay, x_replay_addr, io.core.ex.rs1)
  val x_stride = Mux(x_replay, x_replay_stride, Mux1H(Seq(
    (io.core.ex.inst(27,26) === mopUnit, 1.U << io.core.ex.inst(13,12)),
    (io.core.ex.inst(27,26) === mopStrided, io.core.ex.rs2))))
  val x_eidx = Mux(x_replay, x_replay_eidx, 0.U)
  val x_vl = x_inst.vconfig.vl
  val x_pc = Mux(x_replay, x_replay_pc, io.core.ex.pc)
  val x_iterative = true.B || x_inst.vstart =/= 0.U || x_inst.vm
  val x_masked = (io.vm >> x_eidx)(0)
  val x_tlb_valid = (x_replay || (io.core.ex.valid && io.core.ex.ready && !x_iterative)) && x_eidx < x_vl && x_inst.vmu && x_eidx >= x_inst.vstart && !x_masked

  io.core.ex.ready := !x_replay && (io.tlb.req.ready || !x_inst.vmu) && io.issue_credits > 2.U && !io.resetting && !(x_inst.vm && !io.vm_ready)
  io.tlb.req.valid := x_tlb_valid
  io.tlb.req.bits.vaddr := x_addr
  io.tlb.req.bits.passthrough := false.B
  io.tlb.req.bits.size := x_inst.mem_size
  io.tlb.req.bits.cmd := Mux(x_inst.opcode(5), M_XWR, M_XRD)
  io.tlb.req.bits.prv := io.core.status.prv
  io.tlb.req.bits.v := io.core.status.v

  when (x_replay && x_replay_eidx < x_replay_inst.vconfig.vl) {
    val next_x_replay_eidx = x_replay_eidx + 1.U
    x_replay_eidx := next_x_replay_eidx
  }

  val x_may_be_valid = io.core.ex.valid || x_replay

  // M stage
  val m_valid = RegNext(x_replay || (io.core.ex.valid && io.core.ex.ready), false.B)
  val m_inst = RegEnable(x_inst, x_may_be_valid)
  val m_replay = RegEnable(x_replay, x_may_be_valid)
  val m_addr = RegEnable(x_addr, x_may_be_valid)
  val m_stride = RegEnable(x_stride, x_may_be_valid)
  val m_eidx = RegEnable(x_eidx, x_may_be_valid)
  val m_pc = RegEnable(x_pc, x_may_be_valid)
  val m_vl = m_inst.vconfig.vl
  val m_masked = RegNext(x_masked, x_may_be_valid)
  val m_tlb_resp_valid = RegNext(io.tlb.req.fire, x_may_be_valid)
  val m_iterative = RegEnable(x_iterative, x_may_be_valid)
  val m_tlb_resp = WireInit(io.tlb.s1_resp)
  m_tlb_resp.miss := io.tlb.s1_resp.miss || (!m_tlb_resp_valid && !m_masked)

  // W stage
  val w_valid = RegNext(m_valid && !(io.core.killm && !m_replay), false.B)
  val w_replay = RegEnable(m_replay, m_valid)
  val w_inst = RegEnable(m_inst, m_valid)
  val w_addr = RegEnable(m_addr, m_valid)
  val w_stride = RegEnable(m_stride, m_valid)
  val w_iterative = RegEnable(m_iterative, m_valid)
  val w_eidx = RegEnable(m_eidx, m_valid)
  val w_masked = RegEnable(m_masked, m_valid)
  val w_vl = w_inst.vconfig.vl
  val w_pc = RegEnable(m_pc, m_valid)
  val w_tlb_resp = RegEnable(m_tlb_resp, m_valid)

  val w_xcpts = Seq(
    (w_tlb_resp.pf.st, Causes.store_page_fault.U),
    (w_tlb_resp.pf.ld, Causes.load_page_fault.U),
    (w_tlb_resp.gf.st, Causes.store_guest_page_fault.U),
    (w_tlb_resp.gf.ld, Causes.load_guest_page_fault.U),
    (w_tlb_resp.ae.st, Causes.store_access.U),
    (w_tlb_resp.ae.ld, Causes.load_access.U),
    (w_tlb_resp.ma.st, Causes.misaligned_store.U),
    (w_tlb_resp.ma.ld, Causes.misaligned_load.U)
  )
  val w_xcpt = w_xcpts.map(_._1).orR && w_eidx >= w_inst.vstart && !w_masked
  val w_cause = PriorityMux(w_xcpts)

  io.core.wb.retire := false.B
  io.core.wb.pc := w_pc
  io.core.wb.xcpt := false.B
  io.core.wb.cause := DontCare
  io.core.wb.replay := false.B
  io.core.set_vstart.valid := false.B
  io.core.set_vstart.bits := DontCare

  io.issue.valid := false.B
  io.issue.bits := w_inst

  when (w_valid && !w_replay) {
    when (w_inst.vstart >= w_vl) {
      io.core.wb.retire := true.B
    } .elsewhen (w_iterative) {
      x_replay := true.B
      x_replay_inst := w_inst
      x_replay_eidx := 0.U
      x_replay_addr := w_addr
      x_replay_pc := w_pc
      x_replay_stride := w_stride
    } .otherwise {
      when (w_tlb_resp.miss) { io.core.wb.replay := true.B }
      io.core.wb.retire := !w_xcpt
      io.core.wb.xcpt := w_xcpt
      io.core.wb.cause := w_cause
      io.core.set_vstart.valid := !w_tlb_resp.miss
      io.core.set_vstart.bits := 0.U
      io.issue.valid := true.B
    }
  }
  when (w_valid && w_replay) {
    when (w_tlb_resp.miss) {
      x_replay_eidx := w_eidx
      x_replay_addr := w_addr
    } .elsewhen (w_xcpt) {
      x_replay := false.B
      io.core.wb.retire := false.B
      io.core.wb.xcpt := true.B
      io.core.set_vstart.valid := true.B
      io.core.set_vstart.bits := w_eidx
    } .elsewhen (!w_xcpt && (w_eidx +& 1.U) === w_vl) {
      x_replay := false.B
      io.core.wb.retire := true.B
      io.core.set_vstart.valid := true.B
      io.core.set_vstart.bits := 0.U
      io.issue.valid := true.B
    }
  }

  io.core.mem.block_all := x_replay || (m_valid && m_replay) || (w_valid && (w_iterative || w_replay))
  io.core.mem.block_mem := (w_valid && w_inst.vmu) || io.inflight_mem
  io.tlb.s2_kill := false.B
}
