package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

class LSAQEntry(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val inst = new VectorIssueInst
  val addr = UInt(vaddrBitsExtended.W)
  val eidx = UInt(log2Ceil(maxVLMax).W)
  val iterative = Bool()
  val head = Bool()
  val tail = Bool()
  val masked = Bool()
  val maq_idx = UInt(vmaqSz.W)
}

class StoreData(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val data = UInt(dLen.W)
  val mask = UInt(dLenB.W)
}

class MemRequest(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val addr = UInt(coreMaxAddrBits.W)
  val size = UInt(log2Ceil(1+log2Ceil(dLenB)).W)
  val data = UInt(dLen.W)
  val mask = UInt(dLenB.W)
}

class VectorMemInterface(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val load_req = Decoupled(new MemRequest)
  val load_resp = Input(Valid(UInt(dLen.W)))
  val store_req = Decoupled(new MemRequest)
  val store_ack = Input(Bool())

  val scalar_check = new Bundle {
    val addr = Input(UInt(coreMaxAddrBits.W))
    val size = Input(UInt(2.W))
    val store = Input(Bool())
    val conflict = Output(Bool())
  }
}

class VectorMemUnit(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val enq = Flipped(Decoupled(new VectorIssueInst))

    val dmem = new VectorMemInterface

    val load = Decoupled(UInt(dLen.W))
    val vstdata = Flipped(Decoupled(new StoreData))
    val vm = Input(UInt(maxVLMax.W))
    val vm_hazard = new Bundle {
      val valid = Output(Bool())
      val vat = Output(UInt(vParams.vatSz.W))
      val hazard = Input(Bool())
    }
    val busy = Output(Bool())
  })

  class MAQEntry extends Bundle {
    val agen = Bool()
    val inst = new VectorIssueInst
    def base = inst.rs1_data
    val bound = UInt(vaddrBitsExtended.W)
    def store = inst.opcode(5)
    def all = inst.mop =/= mopUnit
  }

  val maq = Reg(Vec(vParams.vmaqEntries, new MAQEntry))
  val maq_valids = RegInit(VecInit.fill(vParams.vmaqEntries)(false.B))
  val maq_enq_ptr   = RegInit(0.U(vmaqSz.W))
  val maq_agen_ptr  = RegInit(0.U(vmaqSz.W))
  val maq_available = !maq_valids(maq_enq_ptr)
  def maqOlder(i0: UInt, i1: UInt) = cqOlder(i0, i1, maq_enq_ptr)

  io.enq.ready := maq_available
  when (io.enq.fire) {
    maq(maq_enq_ptr).inst := io.enq.bits
    val enq_bound = ((io.enq.bits.vconfig.vl - io.enq.bits.vstart) << io.enq.bits.mem_size) + io.enq.bits.rs1_data
    maq(maq_enq_ptr).bound := enq_bound
    maq(maq_enq_ptr).agen := false.B
    maq_valids(maq_enq_ptr) := true.B
    maq_enq_ptr := Mux(maq_enq_ptr === (vParams.vmaqEntries-1).U, 0.U, maq_enq_ptr + 1.U)
  }

  val scalar_bound = io.dmem.scalar_check.addr + (1.U << io.dmem.scalar_check.size)
  io.dmem.scalar_check.conflict := (0 until vParams.vmaqEntries).map { i =>
    val addr_conflict = maq(maq_agen_ptr).all || (maq(i).base < scalar_bound && maq(i).bound > io.dmem.scalar_check.addr)
    val conflict = addr_conflict && (io.dmem.scalar_check.store || maq(i).store)
    maq_valids(i) && conflict
  }.orR

  val laq = Module(new DCEQueue(new LSAQEntry, vParams.vlaqEntries))
  val saq = Module(new DCEQueue(new LSAQEntry, vParams.vsaqEntries))

  val agen_maq = maq(maq_agen_ptr)
  val r_addr = Reg(UInt(paddrBits.W))
  val r_eidx = Reg(UInt((1+log2Ceil(maxVLMax)).W))
  val agen_head   = RegInit(true.B)
  val agen_addr = Mux(agen_head, agen_maq.inst.rs1_data, r_addr)
  val agen_eidx = Mux(agen_head, agen_maq.inst.vstart, r_eidx)
  val agen_aligned_addr = (agen_addr >> dLenOffBits) << dLenOffBits
  val agen_alignment = agen_addr(dLenOffBits-1,0)
  val agen_inst = agen_maq.inst
  val agen_load = !agen_inst.opcode(5)
  val agen_mem_size = Mux(agen_inst.mop(0), agen_inst.vconfig.vtype.vsew, agen_inst.mem_size)
  val agen_eg_elems = dLenB.U >> agen_inst.mem_size
  val agen_iterative = agen_inst.mop =/= mopUnit || agen_inst.vstart =/= 0.U || !agen_inst.vm
  val agen_next_eidx = agen_eidx +& Mux(agen_iterative, 1.U, agen_eg_elems)
  val agen_may_clear = agen_next_eidx >= Mux(agen_iterative || agen_alignment === 0.U,
    agen_inst.vconfig.vl,
    agen_inst.vconfig.vl + agen_eg_elems)
  val agen_masked = !agen_inst.vm && !(io.vm >> agen_eidx)(0)

  val agen_maq_conflict = (0 until vParams.vmaqEntries).map { i =>
    val addr_conflict = agen_maq.all || maq(i).all || (maq(i).base < agen_maq.bound && maq(i).bound > agen_maq.base)
    val conflict = addr_conflict && (agen_inst.opcode(5) || maq(i).store)
    maq_valids(i) && maqOlder(i.U, maq_agen_ptr) && conflict
  }.orR
  val agen_mask_hazard = !agen_inst.vm && io.vm_hazard.hazard
  val agen_valid = maq_valids(maq_agen_ptr) && !agen_maq.agen && !agen_maq_conflict && !agen_mask_hazard
  val agen_ready = Mux(agen_maq.store,
    saq.io.enq.ready,
    (io.dmem.load_req.ready && laq.io.enq.ready) || agen_masked)
  val agen_stride = Mux(agen_inst.mop === mopUnit, dLenB.U, agen_inst.rs2_data)

  when (agen_valid && agen_ready) {
    r_addr := agen_addr + agen_stride
    r_eidx := agen_next_eidx
    agen_head := false.B
    when (agen_may_clear) {
      maq(maq_agen_ptr).agen := true.B
      maq_agen_ptr := Mux(maq_agen_ptr === (vParams.vmaqEntries-1).U, 0.U, maq_agen_ptr + 1.U)
      agen_head := true.B
    }
  }

  io.vm_hazard.valid := agen_valid && !agen_inst.vm
  io.vm_hazard.vat := agen_inst.vat


  io.dmem.load_req.valid := agen_valid && agen_load && !agen_masked && laq.io.enq.ready
  io.dmem.load_req.bits.addr := Mux(agen_iterative, agen_addr, agen_aligned_addr)
  io.dmem.load_req.bits.size := Mux(agen_iterative, agen_mem_size, log2Ceil(dLenB).U)
  io.dmem.load_req.bits.data := DontCare
  io.dmem.load_req.bits.mask := DontCare

  laq.io.enq.bits.inst := agen_inst
  laq.io.enq.bits.eidx := agen_eidx
  laq.io.enq.bits.iterative := agen_iterative
  laq.io.enq.bits.head := agen_head
  laq.io.enq.bits.tail := agen_may_clear
  laq.io.enq.bits.addr := agen_addr
  laq.io.enq.bits.masked := agen_masked
  laq.io.enq.bits.maq_idx := maq_agen_ptr
  laq.io.enq.valid := agen_valid && agen_load && (agen_masked || io.dmem.load_req.ready)

  saq.io.enq.bits.inst := agen_inst
  saq.io.enq.bits.eidx := agen_eidx
  saq.io.enq.bits.iterative := agen_iterative
  saq.io.enq.bits.head := agen_head
  saq.io.enq.bits.tail := agen_may_clear
  saq.io.enq.bits.addr := agen_addr
  saq.io.enq.bits.masked := agen_masked
  saq.io.enq.bits.maq_idx := maq_agen_ptr
  saq.io.enq.valid := agen_valid && !agen_load

  val lcoal = Module(new LoadCoalescer)
  val lrq = Module(new DCEQueue(UInt(dLen.W), vParams.vlaqEntries, flow=true))
  lrq.io.enq.valid := io.dmem.load_resp.valid
  lrq.io.enq.bits := io.dmem.load_resp.bits
  assert(!(lrq.io.enq.valid && !lrq.io.enq.ready))

  lcoal.io.lrq <> lrq.io.deq
  lcoal.io.laq <> laq.io.deq
  io.load <> lcoal.io.out

  val scoal = Module(new StoreCoalescer)
  scoal.io.saq <> saq.io.deq
  scoal.io.stdata <> io.vstdata
  io.dmem.store_req <> scoal.io.req
  scoal.io.ack <> io.dmem.store_ack

  when (lcoal.io.maq_clear.valid) {
    assert(maq_valids(lcoal.io.maq_clear.bits))
    maq_valids(lcoal.io.maq_clear.bits) := false.B
  }
  when (scoal.io.maq_clear.valid) {
    assert(maq_valids(scoal.io.maq_clear.bits))
    maq_valids(scoal.io.maq_clear.bits) := false.B
  }

  io.busy := maq_valids.orR
}
