package saturn.frontend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._

import saturn.common._

class IndexMaskAccess(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val in = Input(Bool())
    val inst = Input(new VectorIssueInst)

    val index_access = Flipped(new VectorIndexAccessIO)
    val mask_access = Flipped(new VectorMaskAccessIO)

    val access = new Bundle {
      val ready = Output(Bool())
      val eidx = Input(UInt(log2Ceil(maxVLMax).W))
      val index = Output(UInt(64.W))
      val mask = Output(Bool())
    }

    val pop = Input(Valid(UInt(log2Ceil(maxVLMax).W)))
    val flush = Input(Bool())
  })

  val valid = RegInit(false.B)
  val eidx = Reg(UInt(log2Ceil(maxVLMax).W))
  // This all works only with pow2 buffers and eidx starting at 0
  val valids = Reg(Vec(4, Bool()))
  val indices = Reg(Vec(4, UInt(64.W)))
  val masks = Reg(Vec(4, Bool()))
  when (io.in) {
    assert(!valid)
    valid := true.B
    eidx := 0.U
    valids.foreach(_ := false.B)
  }

  val needs_index = io.inst.mop.isOneOf(mopOrdered, mopUnordered)
  val needs_mask = !io.inst.vm

  val index_ready = io.index_access.ready || !needs_index
  val mask_ready = io.mask_access.ready || !needs_mask

  io.index_access.valid := valid && needs_index && !valids(eidx(1,0))
  io.mask_access.valid  := valid && needs_mask && !valids(eidx(1,0))

  io.index_access.vrs  := io.inst.rs2
  io.index_access.eidx := eidx
  io.index_access.eew  := io.inst.mem_idx_size
  io.mask_access.eidx := eidx

  when (valid && index_ready && mask_ready && !valids(eidx(1,0))) {
    val next_eidx = eidx +& 1.U
    eidx := eidx + 1.U
    when (next_eidx === io.inst.vconfig.vl) {
      valid := false.B
    }
    valids(eidx(1,0)) := true.B
    indices(eidx(1,0)) := io.index_access.idx
    masks(eidx(1,0)) := io.mask_access.mask
  }

  io.access.ready := valids(io.access.eidx(1,0))
  io.access.index := indices(io.access.eidx(1,0))
  io.access.mask  := masks(io.access.eidx(1,0))

  when (io.pop.fire) {
    valids(io.pop.bits(1,0)) := false.B
  }
  when (io.flush) {
    valid := false.B
  }
}

class IterativeFaultCheck(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val status = Input(new MStatus)
    val in         = Input(Valid(new VectorIssueInst))
    val busy       = Output(Bool())
    val s0_tlb_req = Valid(new TLBReq(3))
    val s1_tlb_req = Valid(new TLBReq(3))
    val tlb_resp   = Input(new TLBResp)
    val retire     = Output(Bool())
    val pc         = Output(UInt(vaddrBitsExtended.W))
    val vstart     = Valid(UInt(log2Ceil(maxVLMax).W))
    val vconfig    = Valid(new VConfig)
    val xcpt       = Valid(new Bundle {
      val cause = UInt(xLen.W)
      val tval = UInt(coreMaxAddrBits.W)
    })
    val inst       = Output(new VectorIssueInst)
    val issue      = Decoupled(new VectorIssueInst)

    val index_access = Flipped(new VectorIndexAccessIO)
    val mask_access = Flipped(new VectorMaskAccessIO)
  })

  val replay_kill = WireInit(false.B)

  def nextPage(addr: UInt) = ((addr + (1 << pgIdxBits).U) >> pgIdxBits) << pgIdxBits

  val valid  = RegInit(false.B)
  val seg_hi = Reg(Bool())
  val inst   = Reg(new VectorIssueInst)
  val eidx   = Reg(UInt(log2Ceil(maxVLMax).W))
  val addr   = Reg(UInt(vaddrBitsExtended.W))
  val tlb_backoff = RegInit(0.U(2.W))
  when (tlb_backoff =/= 0.U) { tlb_backoff := tlb_backoff - 1.U }

  val im_access = Module(new IndexMaskAccess)
  im_access.io.in := io.in.valid
  im_access.io.inst := inst
  im_access.io.index_access <> io.index_access
  im_access.io.mask_access <> io.mask_access

  when (io.in.valid) {
    assert(!valid)
    valid := true.B
    seg_hi := false.B
    inst := io.in.bits
    eidx := 0.U
    addr := io.in.bits.rs1_data
  }


  val stride = MuxLookup(inst.mop, 0.U)(Seq(
    (mopUnit    -> ((inst.seg_nf +& 1.U) << inst.mem_elem_size)),
    (mopStrided -> inst.rs2_data)
  ))

  val indexed = inst.mop.isOneOf(mopOrdered, mopUnordered)
  val index_ready = !indexed || im_access.io.access.ready
  val mask_ready = inst.vm || im_access.io.access.ready
  val index = Mux(indexed, im_access.io.access.index & eewBitMask(inst.mem_idx_size), 0.U)
  val base = Mux(indexed, inst.rs1_data, addr)
  val indexaddr = base + index
  val tlb_addr = Mux(seg_hi, nextPage(indexaddr), indexaddr)
  val seg_nf_consumed = ((1 << pgIdxBits).U - Mux(seg_hi, indexaddr, tlb_addr)(pgIdxBits-1,0)) >> inst.mem_elem_size
  val seg_single_page = seg_nf_consumed >= (inst.seg_nf +& 1.U)
  val masked = !im_access.io.access.mask && !inst.vm
  val tlb_valid = eidx < inst.vconfig.vl && eidx >= inst.vstart && !masked
  val ff = inst.umop === lumopFF && inst.mop === mopUnit

  io.busy := valid
  io.inst := inst

  im_access.io.access.eidx := eidx

  io.s0_tlb_req.valid            := tlb_valid && tlb_backoff === 0.U && index_ready && mask_ready
  io.s0_tlb_req.bits.vaddr       := tlb_addr
  io.s0_tlb_req.bits.passthrough := false.B
  io.s0_tlb_req.bits.size        := inst.mem_elem_size
  io.s0_tlb_req.bits.cmd         := Mux(inst.opcode(5), M_XWR, M_XRD)
  io.s0_tlb_req.bits.prv         := io.status.prv
  io.s0_tlb_req.bits.v           := io.status.v

  io.s1_tlb_req.valid := RegEnable(io.s0_tlb_req.valid, false.B, valid)
  io.s1_tlb_req.bits  := RegEnable(io.s0_tlb_req.bits, valid)

  val replay_fire = valid && eidx < inst.vconfig.vl && tlb_backoff === 0.U && index_ready && mask_ready
  when (replay_fire) {
    when (seg_hi || seg_single_page || inst.seg_nf === 0.U) {
      eidx := eidx + 1.U
      addr := addr + stride
      seg_hi := false.B
    } .otherwise {
      seg_hi := true.B
    }
  }

  val s1_kill        = WireInit(false.B)
  val s1_valid       = RegNext(replay_fire && !replay_kill, false.B)
  val s1_eidx        = RegEnable(eidx, valid)
  val s1_masked      = RegEnable(masked, valid)
  val s1_seg_hi      = RegEnable(seg_hi, valid)
  val s1_base        = RegEnable(base, valid)
  val s1_tlb_valid   = RegEnable(tlb_valid, valid)
  val s1_tlb_addr    = RegEnable(tlb_addr, valid)
  val s1_seg_nf_consumed = RegEnable(seg_nf_consumed, valid)
  val s1_seg_single_page = RegEnable(seg_single_page, valid)

  when (io.tlb_resp.miss && s1_valid && tlb_backoff === 0.U) { tlb_backoff := 3.U }

  val tlb_resp = WireInit(io.tlb_resp)
  when (!s1_tlb_valid) {
    tlb_resp.miss := false.B
  }

  val xcpts = Seq(
    (tlb_resp.ma.st, Causes.misaligned_store.U),
    (tlb_resp.ma.ld, Causes.misaligned_load.U),
    (tlb_resp.pf.st, Causes.store_page_fault.U),
    (tlb_resp.pf.ld, Causes.load_page_fault.U),
    (tlb_resp.gf.st, Causes.store_guest_page_fault.U),
    (tlb_resp.gf.ld, Causes.load_guest_page_fault.U),
    (tlb_resp.ae.st, Causes.store_access.U),
    (tlb_resp.ae.ld, Causes.load_access.U),
  )
  val xcpt = xcpts.map(_._1).orR && s1_eidx >= inst.vstart && !s1_masked
  val cause = PriorityMux(xcpts)

  val s2_valid = RegNext(s1_valid && !s1_kill, false.B)
  val s2_eidx = RegEnable(s1_eidx, s1_valid)
  val s2_base = RegEnable(s1_base, s1_valid)
  val s2_tlb_resp = RegEnable(tlb_resp, s1_valid)
  val s2_tlb_addr = RegEnable(s1_tlb_addr, s1_valid)
  val s2_xcpt = RegEnable(xcpt, s1_valid)
  val s2_masked = RegEnable(s1_masked, s1_valid)
  val s2_seg_single_page = RegEnable(s1_seg_single_page, s1_valid)
  val s2_seg_hi = RegEnable(s1_seg_hi, s1_valid)
  val s2_seg_nf_consumed = RegEnable(s1_seg_nf_consumed, s1_valid)
  val s2_cause = RegEnable(cause, s1_valid)


  io.issue.valid := false.B
  io.issue.bits := inst
  io.issue.bits.vstart := s2_eidx
  io.issue.bits.vconfig.vl := s2_eidx +& 1.U
  io.issue.bits.segend := inst.seg_nf
  io.issue.bits.segstart := 0.U
  io.issue.bits.page := s2_tlb_resp.paddr >> pgIdxBits
  io.xcpt.valid := false.B
  io.pc := inst.pc
  io.xcpt.bits.cause := s2_cause
  io.xcpt.bits.tval := s2_tlb_addr
  io.vstart.valid := false.B
  io.vstart.bits := s2_eidx
  io.retire := false.B
  io.vconfig.valid := false.B
  io.vconfig.bits := inst.vconfig
  io.vconfig.bits.vl := s2_eidx
  im_access.io.pop.valid := false.B
  im_access.io.pop.bits := s2_eidx
  im_access.io.flush := false.B

  when (s2_valid) {
    io.issue.valid := !s2_tlb_resp.miss && !s2_xcpt && s2_eidx >= inst.vstart && !s2_masked
    when (inst.seg_nf =/= 0.U && !s2_seg_single_page) {
      when (!s2_seg_hi) {
        io.issue.bits.segend := s2_seg_nf_consumed - 1.U
      } .otherwise {
        io.issue.bits.segstart := s2_seg_nf_consumed
      }
    }

    when (s2_seg_hi || s2_seg_single_page || inst.seg_nf === 0.U) {
      im_access.io.pop.valid := true.B
    }

    when (s2_tlb_resp.miss || !io.issue.ready) {
      tlb_backoff := 3.U
      replay_kill := true.B
      eidx := s2_eidx
      addr := s2_base
      seg_hi := s2_seg_hi
      s1_kill := true.B
      im_access.io.pop.valid := false.B
    } .elsewhen (s2_xcpt) {
      val ff_nofault = ff && s2_eidx =/= 0.U
      valid := false.B
      replay_kill := true.B
      io.retire := ff_nofault
      io.xcpt.valid := !ff_nofault
      io.vstart.valid := !ff_nofault
      io.vconfig.valid := ff_nofault
      s1_kill := true.B
      im_access.io.flush := true.B
    } .elsewhen ((s2_eidx +& 1.U) === inst.vconfig.vl && (s2_seg_hi || s2_seg_single_page || inst.seg_nf === 0.U)) {
      valid := false.B
      replay_kill := true.B
      io.retire := true.B
      io.vstart.valid := true.B
      io.vstart.bits := 0.U
      im_access.io.flush := true.B
      s1_kill := true.B
    }
  }
}
