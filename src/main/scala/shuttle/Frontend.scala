package saturn.shuttle

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
import saturn.mem.{ScalarMemOrderCheckIO, MemRequest, TLSplitInterface, SGTLInterface}
import saturn.frontend.{EarlyTrapCheck, IterativeTrapCheck}
import shuttle.common._


class SaturnShuttleFrontend(sgSize: Option[BigInt], edge: TLEdge)(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val sg_base = Input(UInt(coreMaxAddrBits.W))
    val core = new ShuttleVectorCoreIO

    val issue = Decoupled(new VectorIssueInst)

    val index_access = Flipped(new VectorIndexAccessIO)
    val mask_access = Flipped(new VectorMaskAccessIO)

    val scalar_check = Flipped(new ScalarMemOrderCheckIO)
  })

  val ptc = Module(new EarlyTrapCheck(edge, sgSize))
  val itc = Module(new IterativeTrapCheck)

  val replayed = RegInit(false.B)

  ptc.io.sg_base            := io.sg_base
  ptc.io.s0.in.valid        := io.core.ex.valid && !itc.io.busy && !(replayed && !io.issue.ready)
  ptc.io.s0.in.bits.inst    := io.core.ex.uop.inst
  ptc.io.s0.in.bits.pc      := io.core.ex.uop.pc
  ptc.io.s0.in.bits.status  := io.core.status
  ptc.io.s0.in.bits.vconfig := io.core.ex.vconfig
  ptc.io.s0.in.bits.vstart  := io.core.ex.vstart
  ptc.io.s0.in.bits.rs1     := io.core.ex.uop.rs1_data
  ptc.io.s0.in.bits.rs2     := io.core.ex.uop.rs2_data
  ptc.io.s0.in.bits.phys    := !(io.core.status.dprv <= PRV.S.U && io.core.satp.mode(io.core.satp.mode.getWidth-1))
  io.core.ex.ready          := !itc.io.busy && !(replayed && !io.issue.ready)

  ptc.io.s1.rs1.valid := ptc.io.s1.inst.isOpf && !ptc.io.s1.inst.vmu
  ptc.io.s1.rs1.bits := io.core.mem.frs1
  ptc.io.s1.kill := io.core.mem.kill || !RegEnable(io.core.ex.fire, io.core.ex.valid)

  io.core.mem.tlb_req.valid := Mux(itc.io.busy, itc.io.s1_tlb_req.valid, ptc.io.s1.tlb_req.valid)
  io.core.mem.tlb_req.bits  := Mux(itc.io.busy, itc.io.s1_tlb_req.bits,  ptc.io.s1.tlb_req.bits)
  val mem_tlb_resp = Wire(new TLBResp)
  mem_tlb_resp.miss := io.core.mem.tlb_resp.miss || !io.core.mem.tlb_req.ready
  mem_tlb_resp.paddr := io.core.mem.tlb_resp.paddr
  mem_tlb_resp.pf    := io.core.mem.tlb_resp.pf
  mem_tlb_resp.ae    := io.core.mem.tlb_resp.ae
  mem_tlb_resp.ma    := io.core.mem.tlb_resp.ma
  mem_tlb_resp.gpa        := DontCare
  mem_tlb_resp.gpa_is_pte := DontCare
  mem_tlb_resp.gf         := 0.U.asTypeOf(new TLBExceptions)
  mem_tlb_resp.cacheable  := DontCare
  mem_tlb_resp.must_alloc := DontCare
  mem_tlb_resp.prefetchable := DontCare
  mem_tlb_resp.size         := DontCare
  mem_tlb_resp.cmd          := DontCare
  ptc.io.s1.tlb_resp := mem_tlb_resp
  itc.io.tlb_resp    := mem_tlb_resp

  ptc.io.s2.scalar_store_pending := io.core.wb.store_pending

  io.core.wb.retire_late  := itc.io.retire
  io.core.wb.inst         := Mux(itc.io.busy, itc.io.inst.bits      , ptc.io.s2.inst.bits.bits)
  io.core.wb.pc           := Mux(itc.io.busy, itc.io.pc             , ptc.io.s2.pc)
  io.core.wb.xcpt         := Mux(itc.io.busy, itc.io.xcpt.valid     , ptc.io.s2.xcpt.valid)
  io.core.wb.cause        := Mux(itc.io.busy, itc.io.xcpt.bits.cause, ptc.io.s2.xcpt.bits.cause)
  io.core.wb.tval         := Mux(itc.io.busy, itc.io.xcpt.bits.tval , ptc.io.s2.xcpt.bits.tval)
  io.core.wb.internal_replay  := ptc.io.s2.internal_replay.valid
  io.core.wb.block_all        := itc.io.busy || (ptc.io.s2.inst.valid && !ptc.io.s2.retire && !ptc.io.s2.internal_replay.valid)
  io.core.wb.rob_should_wb    := Mux(itc.io.busy, itc.io.inst.writes_xrf, ptc.io.s2.inst.bits.writes_xrf)
  io.core.wb.rob_should_wb_fp := Mux(itc.io.busy, itc.io.inst.writes_frf, ptc.io.s2.inst.bits.writes_frf)
  io.core.set_vstart  := Mux(itc.io.busy, itc.io.vstart, ptc.io.s2.vstart)
  io.core.set_vconfig := itc.io.vconfig
  ptc.io.s2.vxrm := io.core.wb.vxrm
  ptc.io.s2.frm := io.core.wb.frm
  itc.io.in := ptc.io.s2.internal_replay

  when (!io.issue.ready && ptc.io.s2.inst.valid) { replayed := true.B }
  when (io.issue.ready) { replayed := false.B }

  io.issue.valid := Mux(itc.io.busy, itc.io.issue.valid, ptc.io.s2.issue.valid)
  io.issue.bits  := Mux(itc.io.busy, itc.io.issue.bits , ptc.io.s2.issue.bits)
  itc.io.issue.ready    := io.issue.ready
  ptc.io.s2.issue.ready := !itc.io.busy && io.issue.ready

  io.core.trap_check_busy := ptc.io.busy || itc.io.busy

  itc.io.status := io.core.status
  itc.io.index_access <> io.index_access
  itc.io.mask_access <> io.mask_access
  io.scalar_check.addr := io.core.wb.scalar_check.bits.addr
  io.scalar_check.size := io.core.wb.scalar_check.bits.size
  io.scalar_check.store := io.core.wb.scalar_check.bits.store
  io.core.wb.scalar_check.ready := !io.scalar_check.conflict && !(ptc.io.s2.inst.valid && ptc.io.s2.inst.bits.vmu)

  io.core.backend_busy   := false.B // set externally
  io.core.set_vxsat      := false.B // set externally
  io.core.set_fflags     := DontCare // set externally
  io.core.resp           := DontCare // set externally
}
