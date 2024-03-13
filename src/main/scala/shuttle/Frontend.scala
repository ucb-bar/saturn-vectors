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
import saturn.mem.{ScalarMemOrderCheckIO, MemRequest, TLInterface}
import saturn.frontend.{EarlyTrapCheck, IterativeTrapCheck}
import shuttle.common._


class SaturnShuttleUnit(implicit p: Parameters) extends ShuttleVectorUnit()(p) with HasVectorParams with HasCoreParameters {
  assert(!vParams.useScalarFPFMA && !vParams.useScalarFPMisc)
  if (vParams.useScalarFPFMA) {
    require(coreParams.fpu.get.dfmaLatency == vParams.fmaPipeDepth - 1)
  }

  val tl_if = LazyModule(new TLInterface)
  atlNode := TLBuffer(vParams.tlBuffer) := TLWidthWidget(dLenB) := tl_if.node

  override lazy val module = new SaturnShuttleImpl
  class SaturnShuttleImpl extends ShuttleVectorUnitModuleImp(this) with HasVectorParams with HasCoreParameters {

    val ecu = Module(new EarlyTrapCheck(tl_if.node.edges.out(0)))
    val icu = Module(new IterativeTrapCheck)
    val vu = Module(new VectorBackend)

    val replayed = RegInit(false.B)
    val edge = tl_if.node.edges.out(0)

    ecu.io.s0.in.valid        := io.ex.valid && !icu.io.busy && !(replayed && !vu.io.issue.ready)
    ecu.io.s0.in.bits.inst    := io.ex.uop.inst
    ecu.io.s0.in.bits.pc      := io.ex.uop.pc
    ecu.io.s0.in.bits.status  := io.status
    ecu.io.s0.in.bits.vconfig := io.ex.vconfig
    ecu.io.s0.in.bits.vstart  := io.ex.vstart
    ecu.io.s0.in.bits.rs1     := io.ex.uop.rs1_data
    ecu.io.s0.in.bits.rs2     := io.ex.uop.rs2_data
    io.ex.ready               := !icu.io.busy && !(replayed && !vu.io.issue.ready)

    ecu.io.s1.rs1.valid := ecu.io.s1.inst.isOpf && !ecu.io.s1.inst.vmu
    ecu.io.s1.rs1.bits := io.mem.frs1
    ecu.io.s1.kill := io.mem.kill || !RegEnable(io.ex.fire, io.ex.valid)

    io.mem.tlb_req.valid := Mux(icu.io.busy, icu.io.s1_tlb_req.valid, ecu.io.s1.tlb_req.valid)
    io.mem.tlb_req.bits  := Mux(icu.io.busy, icu.io.s1_tlb_req.bits,  ecu.io.s1.tlb_req.bits)
    val mem_tlb_resp = Wire(new TLBResp)
    mem_tlb_resp.miss := io.mem.tlb_resp.miss || !io.mem.tlb_req.ready
    mem_tlb_resp.paddr := io.mem.tlb_resp.paddr
    mem_tlb_resp.pf    := io.mem.tlb_resp.pf
    mem_tlb_resp.ae    := io.mem.tlb_resp.ae
    mem_tlb_resp.ma    := io.mem.tlb_resp.ma
    mem_tlb_resp.gpa        := DontCare
    mem_tlb_resp.gpa_is_pte := DontCare
    mem_tlb_resp.gf         := 0.U.asTypeOf(new TLBExceptions)
    mem_tlb_resp.cacheable  := DontCare
    mem_tlb_resp.must_alloc := DontCare
    mem_tlb_resp.prefetchable := DontCare
    mem_tlb_resp.size         := DontCare
    mem_tlb_resp.cmd          := DontCare
    ecu.io.s1.tlb_resp := mem_tlb_resp
    icu.io.tlb_resp    := mem_tlb_resp

    io.wb.retire_late  := icu.io.retire
    io.wb.inst         := Mux(icu.io.busy, icu.io.inst.bits      , ecu.io.s2.inst.bits.bits)
    io.wb.pc           := Mux(icu.io.busy, icu.io.pc             , ecu.io.s2.pc)
    io.wb.xcpt         := Mux(icu.io.busy, icu.io.xcpt.valid     , ecu.io.s2.xcpt.valid)
    io.wb.cause        := Mux(icu.io.busy, icu.io.xcpt.bits.cause, ecu.io.s2.xcpt.bits.cause)
    io.wb.tval         := Mux(icu.io.busy, icu.io.xcpt.bits.tval , ecu.io.s2.xcpt.bits.tval)
    io.wb.internal_replay  := ecu.io.s2.internal_replay.valid
    io.wb.block_all        := icu.io.busy || (ecu.io.s2.inst.valid && !ecu.io.s2.retire && !ecu.io.s2.internal_replay.valid)
    io.wb.rob_should_wb    := Mux(icu.io.busy, icu.io.inst.writes_xrf, ecu.io.s2.inst.bits.writes_xrf)
    io.wb.rob_should_wb_fp := Mux(icu.io.busy, icu.io.inst.writes_frf, ecu.io.s2.inst.bits.writes_frf)
    io.set_vstart  := Mux(icu.io.busy, icu.io.vstart, ecu.io.s2.vstart)
    io.set_vconfig := icu.io.vconfig
    ecu.io.s2.vxrm := io.wb.vxrm
    ecu.io.s2.frm := io.wb.frm
    icu.io.in := ecu.io.s2.internal_replay

    when (!vu.io.issue.ready && ecu.io.s2.inst.valid) { replayed := true.B }
    when (vu.io.issue.ready) { replayed := false.B }

    vu.io.issue.valid := Mux(icu.io.busy, icu.io.issue.valid, ecu.io.s2.issue.valid)
    vu.io.issue.bits  := Mux(icu.io.busy, icu.io.issue.bits , ecu.io.s2.issue.bits)
    icu.io.issue.ready    := vu.io.issue.ready
    ecu.io.s2.issue.ready := !icu.io.busy && vu.io.issue.ready

    io.trap_check_busy := ecu.io.busy || icu.io.busy

    icu.io.status := io.status
    icu.io.index_access <> vu.io.index_access
    icu.io.mask_access <> vu.io.mask_access
    vu.io.scalar_check.addr := io.wb.scalar_check.bits.addr
    vu.io.scalar_check.size := io.wb.scalar_check.bits.size
    vu.io.scalar_check.store := io.wb.scalar_check.bits.store
    io.wb.scalar_check.ready := !vu.io.scalar_check.conflict && !(ecu.io.s2.inst.valid && ecu.io.s2.inst.bits.vmu)

    io.backend_busy   := vu.io.backend_busy || tl_if.module.io.mem_busy
    io.set_vxsat      := vu.io.set_vxsat
    io.set_fflags     := vu.io.set_fflags
    io.resp           <> vu.io.scalar_resp

    tl_if.module.io.vec <> vu.io.dmem
    tl_if.module.io.vec_busy := vu.io.backend_busy

    vu.io.fp_req.ready := false.B
    vu.io.fp_resp.valid := false.B
    vu.io.fp_resp.bits := DontCare
  }
}
