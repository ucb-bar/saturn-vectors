package saturn.rocket

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._

import saturn.common._
import saturn.backend.{VectorBackend}
import saturn.mem.{ScalarMemOrderCheckIO, TLSplitInterface}
import saturn.frontend.{EarlyTrapCheck, IterativeTrapCheck}

class SaturnRocketFrontend(edge: TLEdge)(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val core = new VectorCoreIO
    val tlb = Flipped(new DCacheTLBPort)

    val issue = Decoupled(new VectorIssueInst)

    val index_access = Flipped(new VectorIndexAccessIO)
    val mask_access = Flipped(new VectorMaskAccessIO)

    val scalar_check = Flipped(new ScalarMemOrderCheckIO)
  })

  val ptc = Module(new EarlyTrapCheck(edge, None))
  val itc = Module(new IterativeTrapCheck)

  ptc.io.sg_base            := DontCare
  ptc.io.s0.in.valid        := io.core.ex.valid && !itc.io.busy
  ptc.io.s0.in.bits.inst    := io.core.ex.inst
  ptc.io.s0.in.bits.pc      := io.core.ex.pc
  ptc.io.s0.in.bits.status  := io.core.status
  ptc.io.s0.in.bits.vconfig := io.core.ex.vconfig
  ptc.io.s0.in.bits.vstart  := io.core.ex.vstart
  ptc.io.s0.in.bits.rs1     := io.core.ex.rs1
  ptc.io.s0.in.bits.rs2     := io.core.ex.rs2
  ptc.io.s0.in.bits.phys    := false.B
  io.core.ex.ready          := !itc.io.busy

  ptc.io.s1.rs1.valid := ptc.io.s1.inst.isOpf && !ptc.io.s1.inst.vmu
  ptc.io.s1.rs1.bits  := io.core.mem.frs1
  ptc.io.s1.kill      := io.core.killm
  io.core.mem.block_all    := itc.io.busy || ptc.io.s2.internal_replay.valid
  io.core.mem.block_mem    := (ptc.io.s2.inst.valid && ptc.io.s2.inst.bits.vmu) || io.scalar_check.conflict

  io.tlb.req.valid := Mux(itc.io.busy, itc.io.s0_tlb_req.valid, ptc.io.s0.tlb_req.valid)
  io.tlb.req.bits  := Mux(itc.io.busy, itc.io.s0_tlb_req.bits , ptc.io.s0.tlb_req.bits)
  ptc.io.s1.tlb_resp := io.tlb.s1_resp
  when (RegEnable(itc.io.busy || !io.tlb.req.ready, ptc.io.s0.tlb_req.valid)) { ptc.io.s1.tlb_resp.miss := true.B }
  itc.io.tlb_resp := io.tlb.s1_resp
  when (RegEnable(!io.tlb.req.ready, itc.io.s0_tlb_req.valid)) { itc.io.tlb_resp.miss := true.B }
  io.tlb.s2_kill := false.B

  ptc.io.s2.scalar_store_pending := io.core.wb.store_pending

  io.core.wb.replay := ptc.io.s2.replay
  io.core.wb.xcpt   := Mux(itc.io.busy, itc.io.xcpt.valid     , ptc.io.s2.xcpt.valid)
  io.core.wb.cause  := Mux(itc.io.busy, itc.io.xcpt.bits.cause, ptc.io.s2.xcpt.bits.cause)
  io.core.wb.pc     := Mux(itc.io.busy, itc.io.pc             , ptc.io.s2.pc)
  io.core.wb.retire := Mux(itc.io.busy, itc.io.retire         , ptc.io.s2.retire)
  io.core.wb.inst   := Mux(itc.io.busy, itc.io.inst.bits      , ptc.io.s2.inst.bits.bits)
  io.core.wb.tval   := Mux(itc.io.busy, itc.io.xcpt.bits.tval , ptc.io.s2.xcpt.bits.tval)
  io.core.wb.rob_should_wb    := Mux(itc.io.busy, itc.io.inst.writes_xrf, ptc.io.s2.inst.bits.writes_xrf)
  io.core.wb.rob_should_wb_fp := Mux(itc.io.busy, itc.io.inst.writes_frf, ptc.io.s2.inst.bits.writes_frf)
  io.core.set_vstart       := Mux(itc.io.busy, itc.io.vstart         , ptc.io.s2.vstart)
  io.core.set_vconfig      := itc.io.vconfig
  ptc.io.s2.vxrm := io.core.wb.vxrm
  ptc.io.s2.frm  := io.core.wb.frm
  itc.io.in      := ptc.io.s2.internal_replay

  io.issue.valid := Mux(itc.io.busy, itc.io.issue.valid, ptc.io.s2.issue.valid)
  io.issue.bits  := Mux(itc.io.busy, itc.io.issue.bits , ptc.io.s2.issue.bits)
  itc.io.issue.ready    := io.issue.ready
  ptc.io.s2.issue.ready := !itc.io.busy && io.issue.ready

  io.core.trap_check_busy := ptc.io.busy || itc.io.busy

  itc.io.status := io.core.status
  itc.io.index_access <> io.index_access
  itc.io.mask_access <> io.mask_access
  io.scalar_check.addr := io.tlb.s1_resp.paddr
  io.scalar_check.size := io.tlb.s1_resp.size
  io.scalar_check.store := isWrite(io.tlb.s1_resp.cmd)

  io.core.backend_busy := false.B // set externally
  io.core.set_vxsat := false.B // set externally
  io.core.set_fflags := DontCare // set externally
  io.core.resp := DontCare
}

