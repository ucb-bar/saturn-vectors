package vector.rocket

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

import vector.common._

class VectorUnit(implicit p: Parameters) extends RocketVectorUnit()(p) with HasVectorParams {
  require(dLen == vMemDataBits)

  val trap_check = Module(new FrontendTrapCheck)
  trap_check.io.core <> io.core
  trap_check.io.tlb <> io.tlb

  val vxu = Module(new VectorBackend)
  vxu.io.issue <> trap_check.io.issue

  trap_check.io.mem_busy := vxu.io.mem_busy
  trap_check.io.vm       := vxu.io.vm
  trap_check.io.vm_busy  := vxu.io.vm_busy
  io.core.backend_busy   := vxu.io.backend_busy

  val hella_simple = Module(new SimpleHellaCacheIF)
  val hella_arb = Module(new HellaCacheArbiter(2))
  hella_simple.io.requestor <> hella_arb.io.mem
  io.dmem <> hella_simple.io.cache
  val hella_load = hella_arb.io.requestor(1)
  val hella_store = hella_arb.io.requestor(0)

  vxu.io.mem.scalar_check := DontCare

  val load_tag_oh = RegInit(VecInit.fill(4)(false.B))
  val load_tag = PriorityEncoder(~(load_tag_oh.asUInt))
  val load_tag_available = !load_tag_oh(load_tag)
  when (hella_load.req.fire) { load_tag_oh(load_tag) := true.B }
  when (hella_load.resp.fire) { load_tag_oh(hella_load.resp.bits.tag) := false.B }

  vxu.io.mem.load_req.ready  := hella_load.req.ready && load_tag_available
  hella_load.req.valid       := vxu.io.mem.load_req.valid && load_tag_available
  hella_load.req.bits.addr   := vxu.io.mem.load_req.bits.addr
  hella_load.req.bits.size   := vxu.io.mem.load_req.bits.size
  hella_load.req.bits.tag    := load_tag
  hella_load.req.bits.cmd    := M_XRD
  hella_load.req.bits.signed := false.B
  hella_load.req.bits.dprv   := io.core.status.prv
  hella_load.req.bits.dv     := io.core.status.dv
  hella_load.req.bits.data   := DontCare
  hella_load.req.bits.mask   := DontCare
  hella_load.req.bits.phys   := DontCare
  hella_load.req.bits.no_alloc := false.B
  hella_load.req.bits.no_xcpt := true.B
  hella_load.s1_kill := false.B
  hella_load.s1_data := DontCare
  hella_load.s2_kill := false.B
  hella_load.keep_clock_enabled := vxu.io.backend_busy

  vxu.io.mem.load_resp.valid := hella_load.resp.valid
  vxu.io.mem.load_resp.bits  := hella_load.resp.bits.data

  val store_tag_oh = RegInit(VecInit.fill(4)(false.B))
  val store_tag = PriorityEncoder(~(store_tag_oh.asUInt))
  val store_tag_available = !store_tag_oh(store_tag)
  when (hella_store.req.fire) { store_tag_oh(store_tag) := true.B }
  when (hella_store.resp.fire) { store_tag_oh(hella_store.resp.bits.tag) := false.B }

  vxu.io.mem.store_req.ready  := hella_store.req.ready && store_tag_available
  hella_store.req.valid       := vxu.io.mem.store_req.valid && store_tag_available
  hella_store.req.bits.addr   := vxu.io.mem.store_req.bits.addr
  hella_store.req.bits.tag    := store_tag
  hella_store.req.bits.cmd    := M_XWR
  hella_store.req.bits.size   := vxu.io.mem.store_req.bits.size
  hella_store.req.bits.signed := false.B
  hella_store.req.bits.dprv   := io.core.status.prv
  hella_store.req.bits.dv     := io.core.status.dv
  hella_store.req.bits.data   := vxu.io.mem.store_req.bits.data
  hella_store.req.bits.mask   := vxu.io.mem.store_req.bits.mask
  hella_store.req.bits.phys   := DontCare
  hella_store.req.bits.no_alloc := false.B
  hella_store.req.bits.no_xcpt := true.B
  hella_store.s1_kill := false.B
  hella_store.s1_data := DontCare
  hella_store.s2_kill := false.B
  hella_store.keep_clock_enabled := vxu.io.backend_busy

  vxu.io.mem.store_ack := hella_store.resp.fire

  when (store_tag_oh.orR || load_tag_oh.orR) { trap_check.io.mem_busy := true.B }
}

class FrontendTrapCheck(implicit p: Parameters) extends CoreModule()(p) with VectorConsts {
  val io = IO(new Bundle {
    val core = new VectorCoreIO
    val tlb = Flipped(new DCacheTLBPort)

    val issue = Decoupled(new VectorIssueInst)
    val mem_busy = Input(Bool())

    val vm = Input(UInt(maxVLMax.W))
    val vm_busy = Input(Bool())
  })

  val replay_kill = WireInit(false.B)

  // X stage
  val x_tlb_backoff = RegInit(0.U(2.W))
  when (x_tlb_backoff =/= 0.U) { x_tlb_backoff := x_tlb_backoff - 1.U }
  val x_replay = RegInit(false.B)
  val x_replay_inst = Reg(new VectorIssueInst)
  val x_replay_eidx = Reg(UInt(log2Ceil(maxVLMax).W))
  val x_replay_addr = Reg(UInt(vaddrBitsExtended.W))
  val x_replay_pc = Reg(UInt(vaddrBitsExtended.W))
  val x_replay_stride = Reg(UInt(vaddrBitsExtended.W))

  val x_core_inst = Wire(new VectorIssueInst)
  x_core_inst.bits := io.core.ex.inst
  x_core_inst.vconfig := io.core.ex.vconfig
  x_core_inst.vconfig.vl := Mux(io.core.ex.inst(27,26) === mopUnit && io.core.ex.inst(24,20) === lumopMask,
    (io.core.ex.vconfig.vl >> 3) + Mux(io.core.ex.vconfig.vl(2,0) === 0.U, 0.U, 1.U),
    io.core.ex.vconfig.vl)
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
  val x_masked = !(io.vm >> x_eidx)(0) && !x_inst.vm
  val x_mem_size = Mux(x_inst.mop(0), x_inst.vconfig.vtype.vsew, x_inst.mem_size)
  val x_unit_bound = x_inst.vconfig.vl << x_inst.mem_size
  val x_single_page = x_inst.mop === mopUnit && ((x_addr + x_unit_bound)(pgIdxBits) === x_addr(pgIdxBits))
  val x_iterative = !x_single_page || x_inst.vstart =/= 0.U || !x_inst.vm
  val x_tlb_valid = (x_replay || (io.core.ex.valid && io.core.ex.ready && !x_iterative)) && x_eidx < x_vl && x_inst.vmu && x_eidx >= x_inst.vstart && !x_masked

  io.core.ex.ready := !x_replay && (io.tlb.req.ready || !x_inst.vmu) && !(!x_inst.vm && io.vm_busy)
  io.tlb.req.valid := x_tlb_valid && x_tlb_backoff === 0.U
  io.tlb.req.bits.vaddr := x_addr
  io.tlb.req.bits.passthrough := false.B
  io.tlb.req.bits.size := x_mem_size
  io.tlb.req.bits.cmd := Mux(x_inst.opcode(5), M_XWR, M_XRD)
  io.tlb.req.bits.prv := io.core.status.prv
  io.tlb.req.bits.v := io.core.status.v

  when (x_replay && x_replay_eidx < x_replay_inst.vconfig.vl) {
    val next_x_replay_eidx = x_replay_eidx + 1.U
    x_replay_eidx := next_x_replay_eidx
    x_replay_addr := x_replay_addr + x_replay_stride
  }

  val x_may_be_valid = io.core.ex.valid || x_replay

  // M stage
  val m_valid = RegNext((x_replay && !replay_kill) || (io.core.ex.valid && io.core.ex.ready), false.B)
  val m_inst = RegEnable(x_inst, x_may_be_valid)
  val m_replay = RegEnable(x_replay, x_may_be_valid)
  val m_addr = RegEnable(x_addr, x_may_be_valid)
  val m_stride = RegEnable(x_stride, x_may_be_valid)
  val m_eidx = RegEnable(x_eidx, x_may_be_valid)
  val m_pc = RegEnable(x_pc, x_may_be_valid)
  val m_masked = RegNext(x_masked, x_may_be_valid)
  val m_tlb_req_valid = RegNext(x_tlb_valid, x_may_be_valid)
  val m_tlb_resp_valid = RegNext(io.tlb.req.fire, x_may_be_valid)
  val m_iterative = RegEnable(x_iterative, x_may_be_valid)
  val m_tlb_resp = WireInit(io.tlb.s1_resp)
  m_tlb_resp.miss := io.tlb.s1_resp.miss || (!m_tlb_resp_valid && m_tlb_req_valid)

  when (io.tlb.s1_resp.miss && m_tlb_req_valid) { x_tlb_backoff := 3.U }

  // W stage
  val w_valid = RegNext(m_valid && !Mux(m_replay, replay_kill, io.core.killm), false.B)
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
  io.core.wb.tval := w_addr
  io.core.set_vstart.valid := false.B
  io.core.set_vstart.bits := DontCare
  io.core.set_vxsat := DontCare
  io.core.backend_busy := DontCare

  io.issue.valid := false.B
  io.issue.bits := w_inst

  val x_set_replay = WireInit(false.B)
  when (x_set_replay) {
    x_replay := true.B
    x_replay_inst := w_inst
    x_replay_eidx := 0.U
    x_replay_addr := w_addr
    x_replay_pc := w_pc
    x_replay_stride := w_stride
  }

  when (w_valid && !w_replay) {
    when (w_inst.vstart >= w_vl) {
      io.core.wb.retire := true.B
    } .elsewhen (!io.issue.ready) {
      io.core.wb.replay := true.B
    } .elsewhen (w_iterative || (!w_tlb_resp.cacheable && !w_tlb_resp.miss)) {
      x_set_replay := true.B
    } .elsewhen (w_tlb_resp.miss) {
      io.core.wb.replay := true.B
    } .otherwise {
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
      replay_kill := true.B
      x_replay_eidx := w_eidx
      x_replay_addr := w_addr
    } .elsewhen (w_xcpt) {
      x_replay := false.B
      io.core.wb.retire := false.B
      io.core.wb.xcpt := true.B
      io.core.set_vstart.valid := true.B
      io.core.set_vstart.bits := w_eidx
      replay_kill := true.B
      when (w_eidx =/= 0.U) {
        io.issue.valid := true.B
        io.issue.bits.vconfig.vl := w_eidx
        assert(io.issue.ready)
      }
    } .elsewhen (!w_xcpt && (w_eidx +& 1.U) === w_vl) {
      x_replay := false.B
      io.core.wb.retire := true.B
      io.core.set_vstart.valid := true.B
      io.core.set_vstart.bits := 0.U
      io.issue.valid := true.B
      assert(io.issue.ready)
    }
  }

  io.core.mem.block_all := x_replay || (m_valid && m_replay) || (w_valid && (w_iterative || w_replay || x_set_replay))
  io.core.mem.block_mem := (w_valid && w_inst.vmu) || io.mem_busy
  io.core.trap_check_busy := x_replay || m_valid || w_valid
  io.tlb.s2_kill := false.B
}
