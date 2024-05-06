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

class IterativeTrapCheck(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
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
  val index_access_kill = RegInit(false.B)

  def nextPage(addr: UInt) = ((addr + (1 << pgIdxBits).U) >> pgIdxBits) << pgIdxBits

  val valid  = RegInit(false.B)
  val seg_hi = Reg(Bool())
  val inst   = Reg(new VectorIssueInst)
  val eidx   = Reg(UInt(log2Ceil(maxVLMax).W))
  val addr   = Reg(UInt(vaddrBitsExtended.W))
  val tlb_backoff = RegInit(0.U(2.W))
  when (tlb_backoff =/= 0.U) { tlb_backoff := tlb_backoff - 1.U }

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
  val index_ready = !indexed || io.index_access.ready
  val mask_ready = inst.vm || io.mask_access.ready
  val index = Mux(indexed, io.index_access.idx & eewBitMask(inst.mem_idx_size), 0.U)
  val base = Mux(indexed, inst.rs1_data, addr)
  val indexaddr = base + index
  val tlb_addr = Mux(seg_hi, nextPage(indexaddr), indexaddr)
  val seg_nf_consumed = ((1 << pgIdxBits).U - Mux(seg_hi, indexaddr, tlb_addr)(pgIdxBits-1,0)) >> inst.mem_elem_size
  val seg_single_page = seg_nf_consumed >= (inst.seg_nf +& 1.U)
  val masked = !io.mask_access.mask && !inst.vm
  val tlb_valid = eidx < inst.vconfig.vl && eidx >= inst.vstart && !masked
  val ff = inst.umop === lumopFF && inst.mop === mopUnit

  io.busy := valid
  io.inst := inst
  io.index_access.valid := valid && indexed && tlb_backoff === 0.U
  io.index_access.vrs   := inst.rs2
  io.index_access.eidx  := eidx
  io.index_access.eew   := inst.mem_idx_size

  io.mask_access.valid := valid && !inst.vm && tlb_backoff === 0.U
  io.mask_access.eidx  := eidx

  io.s0_tlb_req.valid            := tlb_valid && tlb_backoff === 0.U && mask_ready && index_ready
  io.s0_tlb_req.bits.vaddr       := tlb_addr
  io.s0_tlb_req.bits.passthrough := false.B
  io.s0_tlb_req.bits.size        := inst.mem_elem_size
  io.s0_tlb_req.bits.cmd         := Mux(inst.opcode(5), M_XWR, M_XRD)
  io.s0_tlb_req.bits.prv         := io.status.prv
  io.s0_tlb_req.bits.v           := io.status.v

  io.s1_tlb_req.valid := RegEnable(io.s0_tlb_req.valid, false.B, valid)
  io.s1_tlb_req.bits  := RegEnable(io.s0_tlb_req.bits, valid)

  val replay_fire = valid && eidx < inst.vconfig.vl && tlb_backoff === 0.U && index_ready && mask_ready && !index_access_kill
  when (replay_fire) {
    index_access_kill := true.B
    when (seg_hi || seg_single_page || inst.seg_nf === 0.U) {
      eidx := eidx + 1.U
      addr := addr + stride
      seg_hi := false.B
    } .otherwise {
      seg_hi := true.B
    }
  } .otherwise {
    index_access_kill := false.B
  }

  val s1_valid       = RegNext(replay_fire && !replay_kill && !index_access_kill, false.B)
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
    (tlb_resp.pf.st, Causes.store_page_fault.U),
    (tlb_resp.pf.ld, Causes.load_page_fault.U),
    (tlb_resp.gf.st, Causes.store_guest_page_fault.U),
    (tlb_resp.gf.ld, Causes.load_guest_page_fault.U),
    (tlb_resp.ae.st, Causes.store_access.U),
    (tlb_resp.ae.ld, Causes.load_access.U),
    (tlb_resp.ma.st, Causes.misaligned_store.U),
    (tlb_resp.ma.ld, Causes.misaligned_load.U)
  )
  val xcpt = xcpts.map(_._1).orR && s1_eidx >= inst.vstart && !s1_masked
  val cause = PriorityMux(xcpts)


  io.issue.valid := false.B
  io.issue.bits := inst
  io.issue.bits.vstart := s1_eidx
  io.issue.bits.vconfig.vl := s1_eidx +& 1.U
  io.issue.bits.segend := inst.seg_nf
  io.issue.bits.segstart := 0.U
  io.issue.bits.page := tlb_resp.paddr >> pgIdxBits
  io.xcpt.valid := false.B
  io.pc := inst.pc
  io.xcpt.bits.cause := cause
  io.xcpt.bits.tval := s1_tlb_addr
  io.vstart.valid := false.B
  io.vstart.bits := s1_eidx
  io.retire := false.B
  io.vconfig.valid := false.B
  io.vconfig.bits := inst.vconfig
  io.vconfig.bits.vl := s1_eidx

  when (s1_valid) {
    io.issue.valid := !tlb_resp.miss && !xcpt && s1_eidx >= inst.vstart && !s1_masked
    when (inst.seg_nf =/= 0.U && !s1_seg_single_page) {
      when (!s1_seg_hi) {
        io.issue.bits.segend := s1_seg_nf_consumed - 1.U
      } .otherwise {
        io.issue.bits.segstart := s1_seg_nf_consumed
      }
    }

    when (tlb_resp.miss || !io.issue.ready) {
      tlb_backoff := 3.U
      replay_kill := true.B
      eidx := s1_eidx
      addr := s1_base
      seg_hi := s1_seg_hi
    } .elsewhen (xcpt) {
      val ff_nofault = ff && s1_eidx =/= 0.U
      valid := false.B
      replay_kill := true.B
      io.retire := ff_nofault
      io.xcpt.valid := !ff_nofault
      io.vstart.valid := !ff_nofault
      io.vconfig.valid := ff_nofault
    } .elsewhen ((s1_eidx +& 1.U) === inst.vconfig.vl && (s1_seg_hi || s1_seg_single_page || inst.seg_nf === 0.U)) {
      valid := false.B
      replay_kill := true.B
      io.retire := true.B
      io.vstart.valid := true.B
      io.vstart.bits := 0.U
    }
  }
}
