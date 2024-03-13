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
import saturn.mem.{ScalarMemOrderCheckIO, MemRequest, TLInterface}
import saturn.frontend.{EarlyTrapCheck, IterativeTrapCheck}

class SaturnRocketUnit(implicit p: Parameters) extends RocketVectorUnit()(p) with HasVectorParams with HasCoreParameters {

  if (vParams.useScalarFPFMA || vParams.useScalarFPMisc) {
    require(coreParams.fpu.isDefined)
    if (vParams.useScalarFPFMA) {
      require(coreParams.fpu.get.sfmaLatency == vParams.fmaPipeDepth - 1)
      require(coreParams.fpu.get.dfmaLatency == vParams.fmaPipeDepth - 1)
    }
  }

  val tl_if = LazyModule(new TLInterface)
  atlNode := TLBuffer(vParams.tlBuffer) := TLWidthWidget(dLen/8) := tl_if.node

  override lazy val module = new SaturnRocketImpl
  class SaturnRocketImpl extends RocketVectorUnitModuleImp(this) with HasVectorParams with HasCoreParameters {

    val useL1DCache = dLen == vMemDataBits

    val ecu = Module(new EarlyTrapCheck(tl_if.node.edges.out(0)))
    val icu = Module(new IterativeTrapCheck)
    val vu = Module(new VectorBackend)

    ecu.io.s0.in.valid        := io.core.ex.valid && !icu.io.busy
    ecu.io.s0.in.bits.inst    := io.core.ex.inst
    ecu.io.s0.in.bits.pc      := io.core.ex.pc
    ecu.io.s0.in.bits.status  := io.core.status
    ecu.io.s0.in.bits.vconfig := io.core.ex.vconfig
    ecu.io.s0.in.bits.vstart  := io.core.ex.vstart
    ecu.io.s0.in.bits.rs1     := io.core.ex.rs1
    ecu.io.s0.in.bits.rs2     := io.core.ex.rs2
    io.core.ex.ready          := !icu.io.busy

    ecu.io.s1.rs1.valid := ecu.io.s1.inst.isOpf && !ecu.io.s1.inst.vmu
    ecu.io.s1.rs1.bits  := io.core.mem.frs1
    ecu.io.s1.kill      := io.core.killm
    io.core.mem.block_all    := icu.io.busy || ecu.io.s2.internal_replay.valid
    io.core.mem.block_mem    := (ecu.io.s2.inst.valid && ecu.io.s2.inst.bits.vmu && ecu.io.s2.inst.bits.store) || vu.io.scalar_check.conflict

    io.tlb.req.valid := Mux(icu.io.busy, icu.io.s0_tlb_req.valid, ecu.io.s0.tlb_req.valid)
    io.tlb.req.bits  := Mux(icu.io.busy, icu.io.s0_tlb_req.bits , ecu.io.s0.tlb_req.bits)
    ecu.io.s1.tlb_resp := io.tlb.s1_resp
    when (RegEnable(icu.io.busy || !io.tlb.req.ready, ecu.io.s0.tlb_req.valid)) { ecu.io.s1.tlb_resp.miss := true.B }
    icu.io.tlb_resp := io.tlb.s1_resp
    when (RegEnable(!io.tlb.req.ready, icu.io.s0_tlb_req.valid)) { icu.io.tlb_resp.miss := true.B }
    io.tlb.s2_kill := false.B

    io.core.wb.replay := ecu.io.s2.replay
    io.core.wb.xcpt   := Mux(icu.io.busy, icu.io.xcpt.valid     , ecu.io.s2.xcpt.valid)
    io.core.wb.cause  := Mux(icu.io.busy, icu.io.xcpt.bits.cause, ecu.io.s2.xcpt.bits.cause)
    io.core.wb.pc     := Mux(icu.io.busy, icu.io.pc             , ecu.io.s2.pc)
    io.core.wb.retire := Mux(icu.io.busy, icu.io.retire         , ecu.io.s2.retire)
    io.core.wb.inst   := Mux(icu.io.busy, icu.io.inst.bits      , ecu.io.s2.inst.bits.bits)
    io.core.wb.tval   := Mux(icu.io.busy, icu.io.xcpt.bits.tval , ecu.io.s2.xcpt.bits.tval)
    io.core.wb.rob_should_wb    := Mux(icu.io.busy, icu.io.inst.writes_xrf, ecu.io.s2.inst.bits.writes_xrf)
    io.core.wb.rob_should_wb_fp := Mux(icu.io.busy, icu.io.inst.writes_frf, ecu.io.s2.inst.bits.writes_frf)
    io.core.set_vstart       := Mux(icu.io.busy, icu.io.vstart         , ecu.io.s2.vstart)
    io.core.set_vconfig      := icu.io.vconfig
    ecu.io.s2.vxrm := io.core.wb.vxrm
    ecu.io.s2.frm  := io.core.wb.frm
    icu.io.in      := ecu.io.s2.internal_replay

    vu.io.issue.valid := Mux(icu.io.busy, icu.io.issue.valid, ecu.io.s2.issue.valid)
    vu.io.issue.bits  := Mux(icu.io.busy, icu.io.issue.bits , ecu.io.s2.issue.bits)
    icu.io.issue.ready    := vu.io.issue.ready
    ecu.io.s2.issue.ready := !icu.io.busy && vu.io.issue.ready

    io.core.trap_check_busy := ecu.io.busy || icu.io.busy

    icu.io.status := io.core.status
    icu.io.index_access <> vu.io.index_access
    icu.io.mask_access <> vu.io.mask_access
    vu.io.scalar_check.addr := io.tlb.s1_resp.paddr
    vu.io.scalar_check.size := io.tlb.s1_resp.size
    vu.io.scalar_check.store := isWrite(io.tlb.s1_resp.cmd)

    val hella_if = Module(new HellaCacheInterface)

    io.core.backend_busy   := vu.io.backend_busy || tl_if.module.io.mem_busy || hella_if.io.mem_busy
    io.core.set_vxsat      := vu.io.set_vxsat
    io.core.set_fflags     := vu.io.set_fflags
    io.core.resp           <> vu.io.scalar_resp
    io.fp_req <> vu.io.fp_req
    vu.io.fp_resp <> io.fp_resp

    io.dmem <> hella_if.io.dmem
    hella_if.io.vec_busy := vu.io.backend_busy
    tl_if.module.io.vec_busy := vu.io.backend_busy
    hella_if.io.status := io.core.status

    def block[T <: Data](in: DecoupledIO[T], block: Bool): DecoupledIO[T] = {
      val out = Wire(Decoupled(in.bits.cloneType))
      out.bits := in.bits
      out.valid := in.valid && !block
      in.ready := out.ready && !block
      out
    }

    val load_use_tl_reg = RegInit(true.B)
    val store_use_tl_reg = RegInit(true.B)

    // virtually-addressed requests must go through L1
    val load_use_tl = load_use_tl_reg || !useL1DCache.B
    val store_use_tl = store_use_tl_reg || !useL1DCache.B

    vu.io.dmem.load_resp.valid := tl_if.module.io.vec.load_resp.valid || hella_if.io.vec.load_resp.valid
    vu.io.dmem.load_resp.bits := Mux1H(
      Seq(tl_if.module.io.vec.load_resp.valid, hella_if.io.vec.load_resp.valid),
      Seq(tl_if.module.io.vec.load_resp.bits , hella_if.io.vec.load_resp.bits))
    vu.io.dmem.store_ack.valid := tl_if.module.io.vec.store_ack.valid || hella_if.io.vec.store_ack.valid
    vu.io.dmem.store_ack.bits := Mux1H(
      Seq(tl_if.module.io.vec.store_ack.valid, hella_if.io.vec.store_ack.valid),
      Seq(tl_if.module.io.vec.store_ack.bits , hella_if.io.vec.store_ack.bits))

    when (load_use_tl) {
      tl_if.module.io.vec.load_req <> block(vu.io.dmem.load_req, hella_if.io.mem_busy)
      hella_if.io.vec.load_req.valid := false.B
      hella_if.io.vec.load_req.bits := DontCare
    } .otherwise {
      hella_if.io.vec.load_req <> block(vu.io.dmem.load_req, tl_if.module.io.mem_busy)
      tl_if.module.io.vec.load_req.valid := false.B
      tl_if.module.io.vec.load_req.bits := DontCare
    }
    when (store_use_tl) {
      tl_if.module.io.vec.store_req <> block(vu.io.dmem.store_req, hella_if.io.mem_busy)
      hella_if.io.vec.store_req.valid := false.B
      hella_if.io.vec.store_req.bits := DontCare
    } .otherwise {
      hella_if.io.vec.store_req <> block(vu.io.dmem.store_req, tl_if.module.io.mem_busy)
      tl_if.module.io.vec.store_req.valid := false.B
      tl_if.module.io.vec.store_req.bits := DontCare
    }
  }
}

