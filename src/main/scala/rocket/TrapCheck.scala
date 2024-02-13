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
import saturn.mem.{ScalarMemOrderCheckIO, MemRequest}

class FrontendTrapCheck(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val core = new VectorCoreIO
    val tlb = Flipped(new DCacheTLBPort)

    val issue = Decoupled(new VectorIssueInst)

    val backend_busy = Input(Bool())

    val vm_busy = Input(Bool())

    val scalar_check = Flipped(new ScalarMemOrderCheckIO)

    val index_access = Flipped(new VectorIndexAccessIO)
    val mask_access = Flipped(new VectorMaskAccessIO)
  })

  val replay_kill = WireInit(false.B)

  val m_valid = RegInit(false.B)
  val w_valid = RegInit(false.B)

  // X stage
  val x_tlb_backoff = RegInit(0.U(2.W))
  when (x_tlb_backoff =/= 0.U) { x_tlb_backoff := x_tlb_backoff - 1.U }
  val x_replay = RegInit(false.B)
  val x_replay_seg_hi = Reg(Bool())
  val x_replay_inst = Reg(new VectorIssueInst)
  val x_replay_eidx = Reg(UInt(log2Ceil(maxVLMax).W))
  val x_replay_addr = Reg(UInt(vaddrBitsExtended.W))
  val x_replay_pc = Reg(UInt(vaddrBitsExtended.W))
  val x_replay_stride = Reg(UInt(vaddrBitsExtended.W))

  val x_core_inst = Wire(new VectorIssueInst)
  x_core_inst.bits := io.core.ex.inst
  x_core_inst.vconfig := io.core.ex.vconfig
  x_core_inst.rm := DontCare // set at wb
  when (x_core_inst.mop === mopUnit && x_core_inst.vmu) {
    when (x_core_inst.umop === lumopMask) {
      x_core_inst.vconfig.vl := (io.core.ex.vconfig.vl >> 3) + Mux(io.core.ex.vconfig.vl(2,0) === 0.U, 0.U, 1.U)
    }
    when (x_core_inst.umop === lumopWhole) {
      x_core_inst.vconfig.vl := (vLen.U >> (x_core_inst.mem_elem_size +& 3.U)) * (x_core_inst.nf +& 1.U)
    }
  }
  x_core_inst.vstart := Mux(m_valid || w_valid, 0.U, io.core.ex.vstart)
  x_core_inst.rs1_data := io.core.ex.rs1
  x_core_inst.rs2_data := io.core.ex.rs2
  x_core_inst.emul := Mux(io.core.ex.vconfig.vtype.vlmul_sign, 0.U, io.core.ex.vconfig.vtype.vlmul_mag)
  x_core_inst.vat := DontCare
  x_core_inst.phys := DontCare

  def nextPage(addr: UInt) = ((addr + (1 << pgIdxBits).U) >> pgIdxBits) << pgIdxBits

  val x_inst = Mux(x_replay, x_replay_inst, x_core_inst)
  val x_stride = Mux(x_replay, x_replay_stride, Mux1H(Seq(
    (io.core.ex.inst(27,26) === mopUnit, (io.core.ex.inst(31,29) +& 1.U) << io.core.ex.inst(13,12)),
    (io.core.ex.inst(27,26) === mopStrided, io.core.ex.rs2),
    (io.core.ex.inst(27,26).isOneOf(mopOrdered, mopUnordered), 0.U))))
  val x_eidx = Mux(x_replay, x_replay_eidx, x_core_inst.vstart)
  val x_vl = x_inst.vconfig.vl
  val x_pc = Mux(x_replay, x_replay_pc, io.core.ex.pc)
  val x_mem_size = x_inst.mem_elem_size
  val x_unit_bound = ((x_inst.nf +& 1.U) * x_inst.vconfig.vl) << x_mem_size
  val x_indexed = x_inst.vmu && x_inst.mop.isOneOf(mopOrdered, mopUnordered)
  val x_index_ready = !x_indexed || io.index_access.ready
  val x_mask_ready = x_inst.vm || io.mask_access.ready
  val x_index = Mux(x_indexed,
    io.index_access.idx & eewBitMask(x_inst.mem_idx_size),
    0.U)
  val x_baseaddr = Mux(x_replay,
    Mux(x_inst.mop(0), x_inst.rs1_data, x_replay_addr),
    io.core.ex.rs1)(vaddrBitsExtended-1,0)
  val x_indexaddr = (x_baseaddr + x_index)(vaddrBitsExtended-1,0)
  val x_addr = Mux(x_replay && x_replay_seg_hi, nextPage(x_indexaddr),
    Mux(x_replay, x_indexaddr, x_baseaddr + (x_inst.vstart << x_mem_size)))
  def samePage(page1: UInt, page2: UInt) = page1(pgIdxBits) === page2(pgIdxBits)
  val x_single_page = samePage(x_baseaddr + (x_inst.vstart << x_mem_size), x_baseaddr + x_unit_bound - 1.U)
  val x_replay_seg_single_page = samePage(x_indexaddr, x_indexaddr + ((x_inst.nf +& 1.U) << x_mem_size) - 1.U)
  val x_replay_next_page = x_inst.vmu && x_inst.mop === mopUnit && x_inst.nf === 0.U && !x_single_page && !x_replay
  val x_iterative = x_inst.vmu && (!x_single_page || (x_inst.mop =/= mopUnit)) && !x_replay_next_page
  val x_masked = !io.mask_access.mask && !x_inst.vm
  val x_tlb_valid = Mux(x_replay,
    x_eidx < x_vl && x_eidx >= x_inst.vstart && !x_masked,
    io.core.ex.valid && !x_iterative && x_inst.vmu)

  io.index_access.valid := x_replay && x_indexed && x_tlb_backoff === 0.U
  io.index_access.vrs := x_inst.rs2
  io.index_access.eidx := x_eidx
  io.index_access.eew := x_inst.mem_idx_size

  io.mask_access.valid := x_replay && !x_inst.vm && x_tlb_backoff === 0.U
  io.mask_access.eidx := x_eidx

  when ((io.index_access.valid && !io.index_access.ready) || (io.mask_access.valid && !io.mask_access.ready)) {
    x_tlb_backoff := 3.U
  }

  io.core.ex.ready := !x_replay && (io.tlb.req.ready || !x_inst.vmu) && !(!x_inst.vm && io.vm_busy) && !(x_indexed && io.backend_busy)
  io.tlb.req.valid := x_tlb_valid && x_tlb_backoff === 0.U && ((x_mask_ready && x_index_ready) || !x_replay)
  io.tlb.req.bits.vaddr := x_addr
  io.tlb.req.bits.passthrough := false.B
  io.tlb.req.bits.size := x_mem_size
  io.tlb.req.bits.cmd := Mux(x_inst.opcode(5), M_XWR, M_XRD)
  io.tlb.req.bits.prv := io.core.status.prv
  io.tlb.req.bits.v := io.core.status.v

  when (x_replay && x_replay_eidx < x_replay_inst.vconfig.vl && x_tlb_backoff === 0.U && x_index_ready && x_mask_ready) {
    val next_x_replay_eidx = x_replay_eidx + 1.U
    when (x_replay_seg_hi || x_replay_seg_single_page || x_inst.seg_nf === 0.U) {
      x_replay_eidx := next_x_replay_eidx
      x_replay_addr := x_replay_addr + x_replay_stride
      x_replay_seg_hi := false.B
    } .otherwise {
      x_replay_seg_hi := true.B
    }
  }

  val x_may_be_valid = io.core.ex.valid || x_replay

  // M stage
  m_valid := (x_replay && !replay_kill && x_tlb_backoff === 0.U) || (io.core.ex.valid && io.core.ex.ready)
  val m_inst = RegEnable(x_inst, x_may_be_valid)
  val m_replay = RegEnable(x_replay, x_may_be_valid)
  val m_baseaddr = RegEnable(x_baseaddr, x_may_be_valid)
  val m_addr = RegEnable(x_addr, x_may_be_valid)
  val m_stride = RegEnable(x_stride, x_may_be_valid)
  val m_eidx = RegEnable(x_eidx, x_may_be_valid)
  val m_pc = RegEnable(x_pc, x_may_be_valid)
  val m_masked = RegEnable(x_masked, x_may_be_valid)
  val m_seg_hi = RegEnable(x_replay_seg_hi, x_may_be_valid)
  val m_seg_single_page = RegEnable(x_replay_seg_single_page, x_may_be_valid)
  val m_replay_next_page = RegEnable(x_replay_next_page, x_may_be_valid)
  val m_tlb_req_valid = RegEnable(x_tlb_valid, x_may_be_valid)
  val m_tlb_resp_valid = RegEnable(io.tlb.req.fire, x_may_be_valid)
  val m_iterative = RegEnable(x_iterative, x_may_be_valid)
  val m_tlb_resp = WireInit(io.tlb.s1_resp)
  m_tlb_resp.miss := io.tlb.s1_resp.miss || (!m_tlb_resp_valid && m_tlb_req_valid)

  when (io.tlb.s1_resp.miss && m_tlb_req_valid && x_tlb_backoff === 0.U) { x_tlb_backoff := 3.U }

  io.scalar_check.addr := io.tlb.s1_resp.paddr
  io.scalar_check.size := io.tlb.s1_resp.size
  io.scalar_check.store := isWrite(io.tlb.s1_resp.cmd)
  io.core.mem.block_mem := io.scalar_check.conflict

  // W stage
  val killm = WireInit(io.core.killm)
  w_valid := m_valid && !Mux(m_replay, replay_kill, killm)
  val w_replay = RegEnable(m_replay, m_valid)
  val w_inst = Reg(new VectorIssueInst)
  val w_baseaddr = RegEnable(m_baseaddr, m_valid)
  val w_addr = RegEnable(m_addr, m_valid)
  val w_stride = RegEnable(m_stride, m_valid)
  val w_iterative = RegEnable(m_iterative, m_valid)
  val w_eidx = RegEnable(m_eidx, m_valid)
  val w_masked = RegEnable(m_masked, m_valid)
  val w_vl = w_inst.vconfig.vl
  val w_pc = RegEnable(m_pc, m_valid)
  val w_tlb_req_valid = RegEnable(m_tlb_req_valid, m_valid)
  val w_tlb_resp = RegEnable(m_tlb_resp, m_valid)
  val w_seg_hi = RegEnable(m_seg_hi, m_valid)
  val w_seg_single_page = RegEnable(m_seg_single_page, m_valid)
  val w_replay_next_page = RegEnable(m_replay_next_page, m_valid)

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
  val w_xcpt = w_xcpts.map(_._1).orR && w_eidx >= w_inst.vstart && !w_masked && w_tlb_req_valid
  val w_cause = PriorityMux(w_xcpts)
  val w_ff = w_inst.umop === lumopFF && w_inst.mop === mopUnit && w_eidx =/= 0.U

  when (m_valid) {
    w_inst := m_inst
    w_inst.rs1_data := Mux(m_inst.isOpf && !m_inst.vmu, io.core.mem.frs1, m_inst.rs1_data)
  }

  io.core.wb.retire := false.B
  io.core.wb.inst := w_inst.bits
  io.core.wb.pc := w_pc
  io.core.wb.xcpt := false.B
  io.core.wb.cause := DontCare
  io.core.wb.replay := false.B
  io.core.wb.tval := w_addr
  io.core.wb.rob_should_wb := (w_inst.funct3 === OPMVV && w_inst.opmf6 === OPMFunct6.wrxunary0) || (w_inst.funct3 === OPFVV && w_inst.opff6 === OPFFunct6.wrfunary0)
  io.core.wb.rob_should_wb_fp := w_inst.funct3 === OPFVV
  io.core.set_vstart.valid := false.B
  io.core.set_vstart.bits := DontCare
  io.core.set_vxsat := DontCare // set outside
  io.core.set_fflags := DontCare // set outside
  io.core.set_vconfig.valid := false.B
  io.core.set_vconfig.bits := w_inst.vconfig
  io.core.set_vconfig.bits.vl := w_eidx
  io.core.backend_busy := DontCare
  io.core.resp := DontCare // set outside

  io.issue.valid := false.B
  io.issue.bits := w_inst
  io.issue.bits.rm := Mux(w_inst.isOpf, io.core.wb.frm, io.core.wb.vxrm)
  io.issue.bits.phys := false.B
  val consumed = ((1 << pgIdxBits).U - w_addr(pgIdxBits-1,0) >> w_inst.mem_elem_size)
  when (w_inst.vmu) {
    val phys = w_inst.seg_nf === 0.U && w_inst.mop.isOneOf(mopUnit, mopStrided)
    io.issue.bits.phys := phys
    io.issue.bits.rs1_data := Mux(phys, w_tlb_resp.paddr, w_baseaddr)
    when (w_replay_next_page) {
      io.issue.bits.vconfig.vl := w_inst.vstart +& consumed
    }
  }


  val x_set_replay = WireInit(false.B)
  when (x_set_replay) {
    x_replay := true.B
    x_replay_inst := w_inst
    x_replay_eidx := 0.U
    x_replay_seg_hi := false.B
    x_replay_addr := w_baseaddr
    x_replay_pc := w_pc
    x_replay_stride := w_stride
  }

  when (w_valid && !w_replay) {
    when (!io.issue.ready) {
      io.core.wb.replay := true.B
    } .elsewhen (w_inst.vstart =/= 0.U && !w_inst.vmu) {
      io.core.wb.xcpt := true.B
      io.core.wb.cause := Causes.illegal_instruction.U
    } .elsewhen (w_inst.vstart >= w_vl) {
      io.core.wb.retire := true.B
      io.issue.valid := true.B
    } .elsewhen (w_inst.vmu && (w_iterative || (!w_tlb_resp.cacheable && !w_tlb_resp.miss))) {
      x_set_replay := true.B
      killm := true.B
    } .elsewhen (w_tlb_resp.miss) {
      io.core.wb.replay := true.B
    } .elsewhen (w_xcpt) {
      io.core.wb.xcpt := true.B
      io.core.wb.cause := w_cause
      io.core.set_vstart.valid := true.B
      io.core.set_vstart.bits := w_inst.vstart
    } .elsewhen (w_replay_next_page) {
      io.core.set_vstart.valid := true.B
      io.core.set_vstart.bits := consumed + w_inst.vstart
      io.core.wb.replay := true.B
      io.issue.valid := true.B
    } .otherwise {
      io.core.wb.retire := true.B
      io.core.set_vstart.valid := true.B
      io.core.set_vstart.bits := 0.U
      io.issue.valid := true.B
    }
  }
  when (w_valid && w_replay) {
    io.issue.valid := (!w_tlb_resp.miss &&
      !w_xcpt &&
      w_inst.vstart <= w_eidx &&
      !w_masked &&
      (w_seg_hi || w_inst.seg_nf === 0.U || w_seg_single_page)
    )
    io.issue.bits.vstart := w_eidx
    io.issue.bits.vconfig.vl := w_eidx +& 1.U

    when (w_tlb_resp.miss || !io.issue.ready) {
      x_tlb_backoff := 3.U
      replay_kill := true.B
      x_replay_eidx := w_eidx
      x_replay_addr := w_baseaddr
      x_replay_seg_hi := w_seg_hi
    } .elsewhen (w_xcpt) {
      x_replay := false.B
      io.core.wb.retire := w_ff
      io.core.wb.xcpt := !w_ff
      io.core.set_vstart.valid := !w_ff
      io.core.set_vstart.bits := w_eidx
      io.core.set_vconfig.valid := w_ff
      io.core.set_vconfig.bits.vl := w_eidx
      replay_kill := true.B
    } .elsewhen (!w_xcpt && (w_eidx +& 1.U) === w_vl && (w_seg_hi || w_seg_single_page || w_inst.seg_nf === 0.U)) {
      x_replay := false.B
      io.core.wb.retire := true.B
      io.core.set_vstart.valid := true.B
      io.core.set_vstart.bits := 0.U
      replay_kill := true.B
    }
  }

  // this instruction hasn't writen into the VMIQs yet, so have to block a younger scalar op
  when (w_valid && w_inst.vmu) {
    io.core.mem.block_mem := true.B
  }

  io.core.mem.block_all := x_replay || (m_valid && m_replay) || (w_valid && (w_iterative || w_replay || x_set_replay))
  io.core.trap_check_busy := x_replay || m_valid || w_valid
  io.tlb.s2_kill := false.B
}
