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
import saturn.frontend.{PipelinedFaultCheck, IterativeFaultCheck}
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

  val pfc = Module(new PipelinedFaultCheck(edge, sgSize))
  val ifc = Module(new IterativeFaultCheck)

  val replayed = RegInit(false.B)

  pfc.io.sg_base            := io.sg_base
  pfc.io.s0.in.valid        := io.core.ex.valid && !ifc.io.busy && !(replayed && !io.issue.ready)
  pfc.io.s0.in.bits.inst    := io.core.ex.uop.inst
  pfc.io.s0.in.bits.pc      := io.core.ex.uop.pc
  pfc.io.s0.in.bits.status  := io.core.status
  pfc.io.s0.in.bits.vconfig := io.core.ex.vconfig
  pfc.io.s0.in.bits.vstart  := io.core.ex.vstart
  pfc.io.s0.in.bits.rs1     := io.core.ex.uop.rs1_data
  pfc.io.s0.in.bits.rs2     := io.core.ex.uop.rs2_data
  pfc.io.s0.in.bits.phys    := !(io.core.status.dprv <= PRV.S.U && io.core.satp.mode(io.core.satp.mode.getWidth-1))
  io.core.ex.ready          := !ifc.io.busy && !(replayed && !io.issue.ready)

  pfc.io.s1.rs1.valid := pfc.io.s1.inst.isOpf && !pfc.io.s1.inst.vmu
  pfc.io.s1.rs1.bits := io.core.mem.frs1
  pfc.io.s1.kill := io.core.mem.kill || !RegEnable(io.core.ex.fire, io.core.ex.valid)

  io.core.mem.tlb_req.valid := Mux(ifc.io.busy, ifc.io.s1_tlb_req.valid, pfc.io.s1.tlb_req.valid)
  io.core.mem.tlb_req.bits  := Mux(ifc.io.busy, ifc.io.s1_tlb_req.bits,  pfc.io.s1.tlb_req.bits)
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
  pfc.io.s1.tlb_resp := mem_tlb_resp
  ifc.io.tlb_resp    := mem_tlb_resp

  pfc.io.s2.scalar_store_pending := io.core.wb.store_pending

  io.core.wb.retire_late  := ifc.io.retire
  io.core.wb.inst         := Mux(ifc.io.busy, ifc.io.inst.bits      , pfc.io.s2.inst.bits.bits)
  io.core.wb.pc           := Mux(ifc.io.busy, ifc.io.pc             , pfc.io.s2.pc)
  io.core.wb.xcpt         := Mux(ifc.io.busy, ifc.io.xcpt.valid     , pfc.io.s2.xcpt.valid)
  io.core.wb.cause        := Mux(ifc.io.busy, ifc.io.xcpt.bits.cause, pfc.io.s2.xcpt.bits.cause)
  io.core.wb.tval         := Mux(ifc.io.busy, ifc.io.xcpt.bits.tval , pfc.io.s2.xcpt.bits.tval)
  io.core.wb.internal_replay  := pfc.io.s2.internal_replay.valid
  io.core.wb.block_all        := ifc.io.busy || (pfc.io.s2.inst.valid && !pfc.io.s2.retire && !pfc.io.s2.internal_replay.valid)
  io.core.wb.rob_should_wb    := Mux(ifc.io.busy, ifc.io.inst.writes_xrf, pfc.io.s2.inst.bits.writes_xrf)
  io.core.wb.rob_should_wb_fp := Mux(ifc.io.busy, ifc.io.inst.writes_frf, pfc.io.s2.inst.bits.writes_frf)
  io.core.set_vstart  := Mux(ifc.io.busy, ifc.io.vstart, pfc.io.s2.vstart)
  io.core.set_vconfig := ifc.io.vconfig
  pfc.io.s2.vxrm := io.core.wb.vxrm
  pfc.io.s2.frm := io.core.wb.frm
  ifc.io.in := pfc.io.s2.internal_replay

  when (!io.issue.ready && pfc.io.s2.inst.valid) { replayed := true.B }
  when (io.issue.ready) { replayed := false.B }

  io.issue.valid := Mux(ifc.io.busy, ifc.io.issue.valid, pfc.io.s2.issue.valid)
  io.issue.bits  := Mux(ifc.io.busy, ifc.io.issue.bits , pfc.io.s2.issue.bits)
  ifc.io.issue.ready    := io.issue.ready
  pfc.io.s2.issue.ready := !ifc.io.busy && io.issue.ready

  io.core.trap_check_busy := pfc.io.busy || ifc.io.busy

  ifc.io.status := io.core.status
  ifc.io.index_access <> io.index_access
  ifc.io.mask_access <> io.mask_access
  io.scalar_check.addr := io.core.wb.scalar_check.bits.addr
  io.scalar_check.size := io.core.wb.scalar_check.bits.size
  io.scalar_check.store := io.core.wb.scalar_check.bits.store
  io.core.wb.scalar_check.ready := !io.scalar_check.conflict && !(pfc.io.s2.inst.valid && pfc.io.s2.inst.bits.vmu)

  io.core.backend_busy   := false.B // set externally
  io.core.set_vxsat      := false.B // set externally
  io.core.set_fflags     := DontCare // set externally
  io.core.resp           := DontCare // set externally
}
