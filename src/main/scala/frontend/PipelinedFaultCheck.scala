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
import saturn.backend.{VectorBackend}

class PipelinedFaultCheck(edge: TLEdge, sgSize: Option[BigInt])(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {

  val unified_addresses = AddressSet.unify(edge.manager.managers.map(_.address).flatten)
  require(unified_addresses.forall(_.alignment >= (1 << pgIdxBits)),
    "Memory devices on this system must be at least page-aligned")

  val io = IO(new Bundle {
    val sg_base = Input(UInt(coreMaxAddrBits.W))

    val busy = Output(Bool())
    val s0 = new Bundle {
      val in = Input(Valid(new Bundle {
        val inst = UInt(32.W)
        val pc = UInt(vaddrBitsExtended.W)
        val status = new MStatus
        val vconfig = new VConfig
        val vstart = UInt(log2Ceil(maxVLMax).W)
        val rs1 = UInt(xLen.W)
        val rs2 = UInt(xLen.W)
        val phys = Bool()
      }))
      val tlb_req = Valid(new TLBReq(3))
    }

    val s1 = new Bundle {
      val inst = Output(new VectorIssueInst)
      val rs1 = Input(Valid(UInt(xLen.W)))
      val kill = Input(Bool())
      val tlb_req = Valid(new TLBReq(3))
      val tlb_resp = Input(new TLBResp)
    }

    val s2 = new Bundle {
      val scalar_store_pending = Input(Bool())
      val inst   = Valid(new VectorIssueInst)
      val replay = Output(Bool())
      val vstart = Valid(UInt(log2Ceil(maxVLMax).W))
      val retire = Output(Bool())
      val xcpt   = Valid(new Bundle {
        val cause = UInt(xLen.W)
        val tval = UInt(coreMaxAddrBits.W)
      })
      val pc     = Output(UInt(vaddrBitsExtended.W))
      val internal_replay = Valid(new VectorIssueInst)
      val issue           = Decoupled(new VectorIssueInst)
      val vxrm            = Input(UInt(2.W))
      val frm             = Input(UInt(3.W))
    }
  })

  val s1_valid = RegInit(false.B)
  val s2_valid = RegInit(false.B)
  io.busy := s1_valid || s2_valid

  val s0_inst = Wire(new VectorIssueInst)
  s0_inst.pc      := io.s0.in.bits.pc
  s0_inst.bits    := io.s0.in.bits.inst
  s0_inst.vconfig := io.s0.in.bits.vconfig
  s0_inst.vstart   := Mux(s1_valid || s2_valid, 0.U, io.s0.in.bits.vstart)
  s0_inst.segstart := 0.U
  s0_inst.segend   := s0_inst.seg_nf
  s0_inst.rs1_data := io.s0.in.bits.rs1
  s0_inst.rs2_data := io.s0.in.bits.rs2
  val base_mul = Mux(io.s0.in.bits.vconfig.vtype.vlmul_sign, 0.U, io.s0.in.bits.vconfig.vtype.vlmul_mag)
  val adj_mul = base_mul +& Mux(s0_inst.vmu && s0_inst.mem_elem_size > io.s0.in.bits.vconfig.vtype.vsew,
    s0_inst.mem_elem_size - io.s0.in.bits.vconfig.vtype.vsew,
    0.U
  )
  s0_inst.emul     := Mux(adj_mul > 3.U, 3.U, adj_mul)
  s0_inst.page     := DontCare
  s0_inst.vat      := DontCare
  s0_inst.debug_id := DontCare
  s0_inst.rm       := DontCare
  s0_inst.fast_sg  := false.B
  s0_inst.mop      := s0_inst.orig_mop
  s0_inst.fission_vl.valid := false.B // set in s2
  s0_inst.fission_vl.bits := DontCare
  when (s0_inst.vmu && s0_inst.mop === mopUnit) {
    val mask_vl = (io.s0.in.bits.vconfig.vl >> 3) + Mux(io.s0.in.bits.vconfig.vl(2,0) === 0.U, 0.U, 1.U)
    val whole_vl = (vLen.U >> (s0_inst.mem_elem_size +& 3.U)) << MuxLookup(s0_inst.nf, 0.U)(Seq(
      0.U -> 0.U,
      1.U -> 1.U,
      3.U -> 2.U,
      7.U -> 3.U
    ))
    s0_inst.vconfig.vl := MuxLookup(s0_inst.umop, io.s0.in.bits.vconfig.vl)(Seq(
      (lumopWhole -> whole_vl), (lumopMask -> mask_vl)
    ))
    when (s0_inst.umop === lumopWhole) {
      s0_inst.emul := VecInit.tabulate(8)(nf => log2Ceil(nf+1).U)(s0_inst.nf)
    }
  }
  when (!s0_inst.vmu && s0_inst.funct3 === OPIVI && s0_inst.funct6 === OPIFunct6.mvnrr.litValue.U) {
    s0_inst.emul := log2_up(s0_inst.imm5, 8)
  }

  val s0_unit = s0_inst.mop === mopUnit || (s0_inst.mop === mopStrided && io.s0.in.bits.rs2 === ((s0_inst.nf +& 1.U) << s0_inst.mem_elem_size))
  val s0_indexed = s0_inst.mop.isOneOf(mopOrdered, mopUnordered)
  val s0_base  = io.s0.in.bits.rs1 + (((s0_inst.seg_nf +& 1.U) * s0_inst.vstart    ) << s0_inst.mem_elem_size)
  val s0_bound = io.s0.in.bits.rs1 + (((s0_inst.seg_nf +& 1.U) * s0_inst.vconfig.vl) << s0_inst.mem_elem_size) - 1.U
  val s0_single_page = (s0_base >> pgIdxBits) === (s0_bound >> pgIdxBits)
  val s0_replay_next_page = s0_inst.vmu && s0_unit && s0_inst.nf === 0.U && !s0_single_page
  val s0_iterative = (!s0_single_page || !s0_unit || s0_inst.umop === lumopFF) && !s0_replay_next_page
  val s0_fast_sg = s0_iterative && io.s0.in.bits.phys && s0_inst.mop === mopUnordered && s0_inst.seg_nf === 0.U && sgSize.map { size =>
    s0_base >= io.sg_base && s0_base < (io.sg_base + size.U)
  }.getOrElse(false.B)

  val s0_tlb_valid = !s0_iterative && s0_inst.vmu && s0_inst.vstart < s0_inst.vconfig.vl

  io.s0.tlb_req.valid            := s0_tlb_valid && io.s0.in.valid
  io.s0.tlb_req.bits.vaddr       := s0_base
  io.s0.tlb_req.bits.passthrough := false.B
  io.s0.tlb_req.bits.size        := s0_inst.mem_elem_size
  io.s0.tlb_req.bits.cmd         := Mux(s0_inst.opcode(5), M_XWR, M_XRD)
  io.s0.tlb_req.bits.prv         := io.s0.in.bits.status.prv
  io.s0.tlb_req.bits.v           := io.s0.in.bits.status.v

  // s1_stage
  s1_valid := io.s0.in.fire
  val s1_inst             = RegEnable(s0_inst            , io.s0.in.valid)
  val s1_iterative        = RegEnable(s0_iterative       , io.s0.in.valid)
  val s1_replay_next_page = RegEnable(s0_replay_next_page, io.s0.in.valid)
  val s1_base             = RegEnable(s0_base            , io.s0.in.valid)
  val s1_tlb_valid        = RegEnable(s0_tlb_valid       , io.s0.in.valid)
  val s1_fast_sg          = RegEnable(s0_fast_sg         , io.s0.in.valid)
  val s1_tlb_resp         = WireInit(io.s1.tlb_resp)

  when (!s1_tlb_valid) {
    s1_tlb_resp := 0.U.asTypeOf(new TLBResp)
    when (s1_fast_sg) {
      s1_tlb_resp.paddr := s1_base
    }
  }

  io.s1.inst := s1_inst
  io.s1.tlb_req.valid := RegNext(io.s0.tlb_req.valid, false.B)
  io.s1.tlb_req.bits  := RegEnable(io.s0.tlb_req.bits, s0_tlb_valid)

  // s2 stage
  s2_valid := s1_valid && !io.s1.kill
  val s2_inst = Reg(new VectorIssueInst)
  val s2_base = RegEnable(s1_base, s1_valid)
  val s2_iterative        = RegEnable(s1_iterative       , s1_valid)
  val s2_fast_sg          = RegEnable(s1_fast_sg         , s1_valid)
  val s2_replay_next_page = RegEnable(s1_replay_next_page, s1_valid)
  when (s1_valid) {
    s2_inst := s1_inst
    when (io.s1.rs1.valid) { s2_inst.rs1_data := io.s1.rs1.bits }
  }
  val s2_tlb_resp = RegEnable(s1_tlb_resp, s1_valid)

  val s2_xcpts = Seq(
    (s2_tlb_resp.pf.st, Causes.store_page_fault.U),
    (s2_tlb_resp.pf.ld, Causes.load_page_fault.U),
    (s2_tlb_resp.gf.st, Causes.store_guest_page_fault.U),
    (s2_tlb_resp.gf.ld, Causes.load_guest_page_fault.U),
    (s2_tlb_resp.ae.st, Causes.store_access.U),
    (s2_tlb_resp.ae.ld, Causes.load_access.U),
    (s2_tlb_resp.ma.st, Causes.misaligned_store.U),
    (s2_tlb_resp.ma.ld, Causes.misaligned_load.U)
  )
  val s2_xcpt  = s2_xcpts.map(_._1).orR
  val s2_cause = PriorityMux(s2_xcpts)
  val s2_go_to_itc = WireInit(s2_inst.vmu && s2_iterative)
  val s2_generate_xcpt = WireInit(s2_xcpt)

  // masked checks, even in the fast case, need to
  // to to ITC to get the precise element+address of the fault
  when (s2_inst.vmu && s2_xcpt && !s2_inst.vm) {
    s2_go_to_itc := true.B
    s2_generate_xcpt := false.B
  }

  io.s2.inst.valid   := s2_valid
  io.s2.inst.bits    := s2_inst
  io.s2.replay       := false.B
  io.s2.vstart.valid := false.B
  io.s2.vstart.bits  := 0.U
  io.s2.retire       := false.B
  io.s2.internal_replay.valid   := false.B
  io.s2.internal_replay.bits    := s2_inst
  io.s2.internal_replay.bits.rm := Mux(s2_inst.isOpf, io.s2.frm, io.s2.vxrm)
  io.s2.xcpt.valid          := false.B
  io.s2.xcpt.bits.cause     := s2_cause
  io.s2.xcpt.bits.tval      := s2_base
  io.s2.pc                  := s2_inst.pc
  io.s2.issue.valid         := false.B
  io.s2.issue.bits          := s2_inst
  io.s2.issue.bits.segstart := 0.U
  io.s2.issue.bits.segend   := s2_inst.seg_nf
  io.s2.issue.bits.rm       := Mux(s2_inst.isOpf, io.s2.frm, io.s2.vxrm)
  io.s2.issue.bits.page     := s2_tlb_resp.paddr >> pgIdxBits

  val consumed = ((1 << pgIdxBits).U - s2_tlb_resp.paddr(pgIdxBits-1,0)) >> s2_inst.mem_elem_size
  io.s2.issue.bits.fission_vl.valid := s2_inst.vmu && s2_replay_next_page
  io.s2.issue.bits.fission_vl.bits := s2_inst.vstart +& consumed

  when (s2_valid) {
    when (!io.s2.issue.ready || (io.s2.scalar_store_pending && s2_inst.vmu)) {
      io.s2.replay := true.B
    } .elsewhen (s2_inst.vstart =/= 0.U && !s2_inst.vmu) {
      io.s2.xcpt.valid := true.B
      io.s2.xcpt.bits.cause := Causes.illegal_instruction.U
      io.s2.xcpt.bits.tval := s2_inst.pc
    } .elsewhen (s2_inst.vstart >= s2_inst.vconfig.vl) {
      io.s2.retire := true.B
      io.s2.issue.valid := true.B
      io.s2.vstart.valid := true.B
    } .elsewhen (s2_tlb_resp.miss) {
      io.s2.replay := true.B
    } .elsewhen (s2_generate_xcpt) {
      io.s2.xcpt.valid := true.B
    } .elsewhen (s2_inst.vmu && s2_fast_sg) {
      io.s2.retire := true.B
      io.s2.issue.valid := true.B
      io.s2.issue.bits.fast_sg := true.B
      io.s2.vstart.valid := true.B
    } .elsewhen (s2_go_to_itc) {
      io.s2.internal_replay.valid := true.B
    } .elsewhen (s2_replay_next_page) {
      io.s2.replay := true.B
      io.s2.issue.valid := true.B
      io.s2.vstart.valid := true.B
      io.s2.vstart.bits := s2_inst.vstart +& consumed
    } .otherwise {
      io.s2.retire := true.B
      io.s2.vstart.valid := true.B
      io.s2.issue.valid  := true.B
    }
  }

}
