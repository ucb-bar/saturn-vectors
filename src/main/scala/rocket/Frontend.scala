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
import saturn.frontend.{PipelinedFaultCheck, IterativeFaultCheck}

class SaturnRocketFrontend(edge: TLEdge)(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val core = new VectorCoreIO
    val tlb = Flipped(new DCacheTLBPort)

    val issue = Decoupled(new VectorIssueInst)

    val index_access = Flipped(new VectorIndexAccessIO)
    val mask_access = Flipped(new VectorMaskAccessIO)

    val scalar_check = Flipped(new ScalarMemOrderCheckIO)
  })

  val pfc = Module(new PipelinedFaultCheck(edge, None))
  val ifc = Module(new IterativeFaultCheck)

  pfc.io.sg_base            := DontCare
  pfc.io.s0.in.valid        := io.core.ex.valid && !ifc.io.busy
  pfc.io.s0.in.bits.inst    := io.core.ex.inst
  pfc.io.s0.in.bits.pc      := io.core.ex.pc
  pfc.io.s0.in.bits.status  := io.core.status
  pfc.io.s0.in.bits.vconfig := io.core.ex.vconfig
  pfc.io.s0.in.bits.vstart  := io.core.ex.vstart
  pfc.io.s0.in.bits.rs1     := io.core.ex.rs1
  pfc.io.s0.in.bits.rs2     := io.core.ex.rs2
  pfc.io.s0.in.bits.phys    := false.B
  io.core.ex.ready          := !ifc.io.busy

  pfc.io.s1.rs1.valid := pfc.io.s1.inst.isOpf && !pfc.io.s1.inst.vmu
  pfc.io.s1.rs1.bits  := io.core.mem.frs1
  pfc.io.s1.kill      := io.core.killm
  io.core.mem.block_all    := ifc.io.busy || pfc.io.s2.internal_replay.valid
  io.core.mem.block_mem    := (pfc.io.s2.inst.valid && pfc.io.s2.inst.bits.vmu) || io.scalar_check.conflict

  io.tlb.req.valid := Mux(ifc.io.busy, ifc.io.s0_tlb_req.valid, pfc.io.s0.tlb_req.valid)
  io.tlb.req.bits  := Mux(ifc.io.busy, ifc.io.s0_tlb_req.bits , pfc.io.s0.tlb_req.bits)
  pfc.io.s1.tlb_resp := io.tlb.s1_resp
  when (RegEnable(ifc.io.busy || !io.tlb.req.ready, pfc.io.s0.tlb_req.valid)) { pfc.io.s1.tlb_resp.miss := true.B }
  ifc.io.tlb_resp := io.tlb.s1_resp
  when (RegEnable(!io.tlb.req.ready, ifc.io.s0_tlb_req.valid)) { ifc.io.tlb_resp.miss := true.B }
  io.tlb.s2_kill := false.B

  pfc.io.s2.scalar_store_pending := io.core.wb.store_pending

  io.core.wb.replay := pfc.io.s2.replay
  io.core.wb.xcpt   := Mux(ifc.io.busy, ifc.io.xcpt.valid     , pfc.io.s2.xcpt.valid)
  io.core.wb.cause  := Mux(ifc.io.busy, ifc.io.xcpt.bits.cause, pfc.io.s2.xcpt.bits.cause)
  io.core.wb.pc     := Mux(ifc.io.busy, ifc.io.pc             , pfc.io.s2.pc)
  io.core.wb.retire := Mux(ifc.io.busy, ifc.io.retire         , pfc.io.s2.retire)
  io.core.wb.inst   := Mux(ifc.io.busy, ifc.io.inst.bits      , pfc.io.s2.inst.bits.bits)
  io.core.wb.tval   := Mux(ifc.io.busy, ifc.io.xcpt.bits.tval , pfc.io.s2.xcpt.bits.tval)
  io.core.wb.rob_should_wb    := Mux(ifc.io.busy, ifc.io.inst.writes_xrf, pfc.io.s2.inst.bits.writes_xrf)
  io.core.wb.rob_should_wb_fp := Mux(ifc.io.busy, ifc.io.inst.writes_frf, pfc.io.s2.inst.bits.writes_frf)
  io.core.set_vstart       := Mux(ifc.io.busy, ifc.io.vstart         , pfc.io.s2.vstart)
  io.core.set_vconfig      := ifc.io.vconfig
  pfc.io.s2.vxrm := io.core.wb.vxrm
  pfc.io.s2.frm  := io.core.wb.frm
  ifc.io.in      := pfc.io.s2.internal_replay

  io.issue.valid := Mux(ifc.io.busy, ifc.io.issue.valid, pfc.io.s2.issue.valid)
  io.issue.bits  := Mux(ifc.io.busy, ifc.io.issue.bits , pfc.io.s2.issue.bits)
  ifc.io.issue.ready    := io.issue.ready
  pfc.io.s2.issue.ready := !ifc.io.busy && io.issue.ready

  io.core.trap_check_busy := pfc.io.busy || ifc.io.busy

  ifc.io.status := io.core.status
  ifc.io.index_access <> io.index_access
  ifc.io.mask_access <> io.mask_access
  io.scalar_check.addr := io.tlb.s1_resp.paddr
  io.scalar_check.size := io.tlb.s1_resp.size
  io.scalar_check.store := isWrite(io.tlb.s1_resp.cmd)

  io.core.backend_busy := false.B // set externally
  io.core.set_vxsat := false.B // set externally
  io.core.set_fflags := DontCare // set externally
  io.core.resp := DontCare
}

