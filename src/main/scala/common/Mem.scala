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
  val prestart = Bool()
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
    val addr = Input(UInt(vaddrBitsExtended.W))
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
    val enq_bound = (io.enq.bits.vconfig.vl << io.enq.bits.mem_size) + io.enq.bits.rs1_data
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

  val valid = RegInit(false.B)
  val inst = Reg(new VectorIssueInst)
  val addr = Reg(UInt(vaddrBitsExtended.W))
  val eidx = Reg(UInt((1+log2Ceil(maxVLMax)).W))
  val stride = Reg(UInt(vaddrBitsExtended.W))
  val iterative = Reg(Bool())
  val clear = WireInit(false.B)
  val maq_idx = Reg(UInt(vmaqSz.W))

  val agen_inst = maq(maq_agen_ptr).inst
  val agen_base = maq(maq_agen_ptr).base
  val agen_bound = maq(maq_agen_ptr).bound
  val agen_maq_conflict = (0 until vParams.vmaqEntries).map { i =>
    val addr_conflict = maq(maq_agen_ptr).all || maq(i).all || (maq(i).base < agen_bound && maq(i).bound > agen_base)
    val conflict = addr_conflict && (agen_inst.opcode(5) || maq(i).store)
    maq_valids(i) && maqOlder(i.U, maq_agen_ptr) && conflict
  }.orR
  val agen_valid = maq_valids(maq_agen_ptr) && !maq(maq_agen_ptr).agen
  val agen_ready = (!valid || clear) && !agen_maq_conflict

  when (agen_valid && agen_ready) {

    valid := true.B
    inst := agen_inst
    eidx := 0.U
    addr := agen_inst.rs1_data
    stride := Mux(agen_inst.mop === mopUnit, 1.U << agen_inst.mem_size, agen_inst.rs2_data)

    val enq_iterative = agen_inst.mop =/= mopUnit || agen_inst.vstart =/= 0.U || !agen_inst.vm
    when (!enq_iterative) {
      stride := dLenB.U
    }
    iterative := enq_iterative
    maq_idx := maq_agen_ptr

    maq(maq_agen_ptr).agen := true.B
    maq_agen_ptr := Mux(maq_agen_ptr === (vParams.vmaqEntries-1).U, 0.U, maq_agen_ptr + 1.U)
  } .elsewhen (clear) {
    valid := false.B
  }

  val aligned_addr = (addr >> dLenOffBits) << dLenOffBits
  val alignment = addr(dLenOffBits-1,0)
  val alignment_elems = alignment >> inst.mem_size
  val load = !inst.opcode(5)
  val eg_elems = dLenB.U >> inst.mem_size
  val next_eidx = eidx +& Mux(iterative, 1.U, eg_elems)
  val may_clear = next_eidx >= Mux(iterative || alignment === 0.U, inst.vconfig.vl, inst.vconfig.vl + eg_elems)
  val prestart = eidx < inst.vstart
  val masked = !inst.vm && !(io.vm >> eidx)(0)

  io.vm_hazard.valid := valid && !inst.vm
  io.vm_hazard.vat := inst.vat
  val mask_hazard = !inst.vm && io.vm_hazard.hazard

  val laq = Module(new DCEQueue(new LSAQEntry, vParams.vlaqEntries))
  val saq = Module(new DCEQueue(new LSAQEntry, vParams.vsaqEntries))

  io.dmem.load_req.valid := valid && load && !prestart && laq.io.enq.ready && !masked && !mask_hazard
  io.dmem.load_req.bits.addr := Mux(iterative, addr, aligned_addr)
  io.dmem.load_req.bits.size := Mux(iterative, inst.mem_size, log2Ceil(dLenB).U)
  io.dmem.load_req.bits.data := DontCare
  io.dmem.load_req.bits.mask := DontCare

  laq.io.enq.bits.inst := inst
  laq.io.enq.bits.eidx := eidx
  laq.io.enq.bits.iterative := iterative
  laq.io.enq.bits.head := eidx === 0.U
  laq.io.enq.bits.tail := may_clear
  laq.io.enq.bits.addr := addr
  laq.io.enq.bits.prestart := prestart
  laq.io.enq.bits.masked := masked
  laq.io.enq.bits.maq_idx := maq_idx
  laq.io.enq.valid := valid && load && (prestart || io.dmem.load_req.ready) && !mask_hazard

  saq.io.enq.bits.inst := inst
  saq.io.enq.bits.eidx := eidx
  saq.io.enq.bits.iterative := iterative
  saq.io.enq.bits.head := eidx === 0.U
  saq.io.enq.bits.tail := may_clear
  saq.io.enq.bits.addr := addr
  saq.io.enq.bits.prestart := prestart
  saq.io.enq.bits.masked := masked
  saq.io.enq.bits.maq_idx := maq_idx
  saq.io.enq.valid := valid && !load && !mask_hazard

  val fire = valid && Mux(load, laq.io.enq.fire, saq.io.enq.fire)

  when (fire) {
    clear := may_clear
    when (!may_clear) {
      addr := addr + stride
      eidx := next_eidx
    }
  }

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
