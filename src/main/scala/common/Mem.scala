package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

class LSIQEntry(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val inst = new VectorIssueInst
  def base = inst.rs1_data
  val bound = UInt(paddrBits.W)
  def bound_all = inst.mop =/= mopUnit
}

class IFQEntry(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val head   = UInt(log2Ceil(dLenB).W)
  val tail   = UInt(log2Ceil(dLenB).W)
  val masked = Bool()
  val last   = Bool()
}

class MemRequest(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val addr = UInt(coreMaxAddrBits.W)
  val phys = Bool()
  val data = UInt(dLen.W)
  val mask = UInt(dLenB.W)
}

class MaskIndex extends Bundle {
  val mask = Bool()
  val index = UInt(64.W)
  val load = Bool()
}

class ScalarMemOrderCheckIO(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val addr = Input(UInt(coreMaxAddrBits.W))
  val size = Input(UInt(2.W))
  val store = Input(Bool())
  val conflict = Output(Bool())
}

class VectorMemInterface(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val load_req = Decoupled(new MemRequest)
  val load_resp = Input(Valid(UInt(dLen.W)))
  val store_req = Decoupled(new MemRequest)
  val store_ack = Input(Bool())

  val scalar_check = new ScalarMemOrderCheckIO
}

class VectorMemUnit(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val enq = Flipped(Decoupled(new VectorIssueInst))

    val dmem = new VectorMemInterface

    val lresp = Decoupled(UInt(dLen.W))
    val sdata = Flipped(Decoupled(UInt(dLen.W)))

    val maskindex = Flipped(Decoupled(new MaskIndex))

    val busy = Output(Bool())

    val vat_tail = Input(UInt(vParams.vatSz.W))
    val vat_release = Output(Valid(UInt(vParams.vatSz.W)))
  })

  def vatOlder(i0: UInt, i1: UInt) = cqOlder(i0, i1, io.vat_tail)
  def ptrIncr(u: UInt, sz: Int): Unit = {
    val n = u +& 1.U
    u := Mux(n === sz.U, 0.U, n)
  }

  val liq = Reg(Vec(vParams.vliqEntries, new LSIQEntry))
  val liq_valids    = RegInit(VecInit.fill(vParams.vliqEntries)(false.B))
  val liq_las       = RegInit(VecInit.fill(vParams.vliqEntries)(false.B))
  val liq_enq_ptr   = RegInit(0.U(log2Ceil(vParams.vliqEntries).W))
  val liq_las_ptr   = RegInit(0.U(log2Ceil(vParams.vliqEntries).W))
  val liq_lss_ptr   = RegInit(0.U(log2Ceil(vParams.vliqEntries).W))

  val liq_enq_fire = Wire(Bool())
  val liq_las_fire = Wire(Bool())
  val liq_lss_fire = Wire(Bool())

  val liq_enq_ready = !liq_valids(liq_enq_ptr)
  val liq_las_valid = !liq_las(liq_las_ptr) && liq_valids(liq_las_ptr)
  val liq_lss_valid = liq_valids(liq_lss_ptr)

  when (liq_enq_fire) { ptrIncr(liq_enq_ptr, vParams.vliqEntries); liq_valids(liq_enq_ptr) := true.B }
  when (liq_las_fire) { ptrIncr(liq_las_ptr, vParams.vliqEntries); liq_las(liq_las_ptr) := true.B }
  when (liq_lss_fire) { ptrIncr(liq_lss_ptr, vParams.vliqEntries); liq_valids(liq_lss_ptr) := false.B }

  val siq = Reg(Vec(vParams.vsiqEntries, new LSIQEntry))
  val siq_valids    = RegInit(VecInit.fill(vParams.vsiqEntries)(false.B))
  val siq_sss       = RegInit(VecInit.fill(vParams.vsiqEntries)(false.B))
  val siq_sas       = RegInit(VecInit.fill(vParams.vsiqEntries)(false.B))
  val siq_enq_ptr   = RegInit(0.U(log2Ceil(vParams.vsiqEntries).W))
  val siq_sss_ptr   = RegInit(0.U(log2Ceil(vParams.vsiqEntries).W))
  val siq_sas_ptr   = RegInit(0.U(log2Ceil(vParams.vsiqEntries).W))
  val siq_deq_ptr   = RegInit(0.U(log2Ceil(vParams.vsiqEntries).W))

  val siq_enq_fire = Wire(Bool())
  val siq_sss_fire = Wire(Bool())
  val siq_sas_fire = Wire(Bool())
  val siq_deq_fire = Wire(Bool())

  val siq_enq_ready = !siq_valids(siq_enq_ptr)
  val siq_sss_valid = !siq_sss(siq_sss_ptr) && siq_valids(siq_sss_ptr)
  val siq_sas_valid = !siq_sas(siq_sas_ptr) && siq_valids(siq_sas_ptr)

  when (siq_enq_fire) { ptrIncr(siq_enq_ptr, vParams.vsiqEntries); siq_valids(siq_enq_ptr) := true.B }
  when (siq_sss_fire) { ptrIncr(siq_sss_ptr, vParams.vsiqEntries); siq_sss(siq_sss_ptr) := true.B }
  when (siq_sas_fire) { ptrIncr(siq_sas_ptr, vParams.vsiqEntries); siq_sas(siq_sas_ptr) := true.B }
  when (siq_deq_fire) { ptrIncr(siq_deq_ptr, vParams.vsiqEntries); siq_valids(siq_deq_ptr) := false.B }

  io.enq.ready := Mux(io.enq.bits.store, siq_enq_ready, liq_enq_ready)
  liq_enq_fire := io.enq.valid && liq_enq_ready && !io.enq.bits.store
  siq_enq_fire := io.enq.valid && siq_enq_ready &&  io.enq.bits.store

  val enq_bound = (((io.enq.bits.nf +& 1.U) * io.enq.bits.vconfig.vl) << io.enq.bits.mem_size) + io.enq.bits.rs1_data

  when (liq_enq_fire) {
    liq(liq_enq_ptr).inst := io.enq.bits
    liq(liq_enq_ptr).bound := enq_bound
    liq_las(liq_enq_ptr) := false.B
  }
  when (siq_enq_fire) {
    siq(siq_enq_ptr).inst := io.enq.bits
    siq(siq_enq_ptr).bound := enq_bound
    siq_sss(siq_enq_ptr) := false.B
    siq_sas(siq_enq_ptr) := false.B
  }

  val scalar_bound = io.dmem.scalar_check.addr + (1.U << io.dmem.scalar_check.size)
  val scalar_store_conflict = (0 until vParams.vsiqEntries).map { i =>
    val addr_conflict = siq(i).bound_all || (siq(i).base < scalar_bound && siq(i).bound > io.dmem.scalar_check.addr)
    siq_valids(i) && addr_conflict
  }.orR
  val scalar_load_conflict = (0 until vParams.vliqEntries).map { i =>
    val addr_conflict = liq(i).bound_all || (liq(i).base < scalar_bound && liq(i).bound > io.dmem.scalar_check.addr)
    liq_valids(i) && addr_conflict
  }.orR
  io.dmem.scalar_check.conflict := scalar_store_conflict || (scalar_load_conflict && io.dmem.scalar_check.store)

  // Load Addr Sequencing
  val las = Module(new AddrGen)
  val las_order_block = (0 until vParams.vsiqEntries).map { i =>
    val addr_conflict = (siq(i).bound_all || liq(liq_las_ptr).bound_all ||
      (siq(i).base < liq(liq_las_ptr).bound && siq(i).bound > liq(liq_las_ptr).base)
    )
    siq_valids(i) && addr_conflict && vatOlder(siq(i).inst.vat, liq(liq_las_ptr).inst.vat)
  }.orR
  las.io.valid := liq_las_valid && !las_order_block
  las.io.inst := liq(liq_las_ptr).inst

  las.io.maskindex.valid := io.maskindex.valid && io.maskindex.bits.load
  las.io.maskindex.bits := io.maskindex.bits

  io.dmem.load_req <> las.io.req
  liq_las_fire := las.io.done

  val lifq = Module(new DCEQueue(new IFQEntry, vParams.vlifqEntries))
  lifq.io.enq <> las.io.out

  val lrq = Module(new DCEQueue(UInt(dLen.W), vParams.vlifqEntries))
  lrq.io.enq.valid := io.dmem.load_resp.valid
  lrq.io.enq.bits := io.dmem.load_resp.bits

  // Load compacting
  val lcu = Module(new Compactor)
  lcu.io.push.valid := lifq.io.deq.valid && (lrq.io.deq.valid || lifq.io.deq.bits.masked)
  lcu.io.push.bits.head := lifq.io.deq.bits.head
  lcu.io.push.bits.tail := lifq.io.deq.bits.tail
  lcu.io.push_data := lrq.io.deq.bits
  lifq.io.deq.ready := lcu.io.push.ready && (lrq.io.deq.valid || lifq.io.deq.bits.masked)
  lrq.io.deq.ready := lcu.io.push.ready && !lifq.io.deq.bits.masked

  // Load segment sequencing
  val lss = Module(new SegmentGen)
  lss.io.valid := liq_lss_valid
  lss.io.inst := liq(liq_lss_ptr).inst
  liq_lss_fire := lss.io.done

  val lseg = Module(new LoadSegmenter)
  lcu.io.pop <> lseg.io.compactor
  lseg.io.compactor_data := lcu.io.pop_data
  lseg.io.seg <> lss.io.seg
  io.lresp <> lseg.io.resp

  // Store segment sequencing
  val scu = Module(new Compactor)
  val sss = Module(new SegmentGen)
  sss.io.valid := siq_sss_valid
  sss.io.inst := siq(siq_sss_ptr).inst
  siq_sss_fire := sss.io.done

  val sseg = Module(new StoreSegmenter)
  scu.io.push <> sseg.io.compactor
  scu.io.push_data := sseg.io.compactor_data
  sseg.io.seg <> sss.io.seg
  sseg.io.stdata <> io.sdata

  // Store address sequencing
  val sas = Module(new AddrGen)
  val sas_order_block = (0 until vParams.vliqEntries).map { i =>
    val addr_conflict = (liq(i).bound_all || siq(siq_sas_ptr).bound_all ||
      (liq(i).base < siq(siq_sas_ptr).bound && liq(i).bound > siq(siq_sas_ptr).base)
    )
    liq_valids(i) && addr_conflict && vatOlder(liq(i).inst.vat, siq(siq_sas_ptr).inst.vat)
  }.orR
  sas.io.valid := siq_sas_valid && !sas_order_block
  sas.io.inst := siq(siq_sas_ptr).inst
  sas.io.maskindex.valid := io.maskindex.valid && !io.maskindex.bits.load
  sas.io.maskindex.bits := io.maskindex.bits
  siq_sas_fire := sas.io.done

  io.dmem.store_req <> sas.io.req
  io.dmem.store_req.bits.data := scu.io.pop_data

  val sifq = Module(new DCEQueue(new IFQEntry, vParams.vsifqEntries))
  sas.io.out.ready := sifq.io.enq.ready && scu.io.pop.ready
  sifq.io.enq.valid := sas.io.out.valid && scu.io.pop.ready
  sifq.io.enq.bits := sas.io.out.bits
  scu.io.pop.valid := sas.io.out.valid && sifq.io.enq.ready

  val sifq_mask_hazard = sifq.io.peek.map(e => e.valid && e.bits.masked).orR
  when (sifq_mask_hazard) {
    io.dmem.store_req.valid := false.B
    sas.io.req.ready := false.B
  }

  scu.io.pop.bits.head := sas.io.out.bits.head
  scu.io.pop.bits.tail := sas.io.out.bits.tail

  sifq.io.deq.ready := sifq.io.deq.bits.masked || io.dmem.store_ack
  siq_deq_fire := sifq.io.deq.fire && sifq.io.deq.bits.last
  io.vat_release.valid := siq_deq_fire
  io.vat_release.bits := siq(siq_deq_ptr).inst.vat

  io.busy := liq_valids.orR || siq_valids.orR

  io.maskindex.ready := Mux(io.maskindex.bits.load, las.io.maskindex.ready, sas.io.maskindex.ready)

  // class MAQEntry extends Bundle {
  //   val agen  = Bool()
  //   val sdata = Bool()
  //   val lresp = Bool()
  //   val inst = new VectorIssueInst
  //   def base = inst.rs1_data
  //   val bound = UInt(vaddrBitsExtended.W)
  //   def store = inst.opcode(5)
  //   def all = inst.mop =/= mopUnit
  // }

  // val maq = Reg(Vec(vParams.vmaqEntries, new MAQEntry))
  // val maq_valids = RegInit(VecInit.fill(vParams.vmaqEntries)(false.B))
  // val maq_enq_ptr   = RegInit(0.U(vmaqSz.W))
  // val maq_lagen_ptr = RegInit(0.U(vmaqSz.W))
  // val maq_lresp_ptr = RegInit(0.U(vmaqSz.W))
  // val maq_sagen_ptr = RegInit(0.U(vmaqSz.W))
  // val maq_sdata_ptr = RegInit(0.U(vmaqSz.W))
  // val maq_available = !maq_valids(maq_enq_ptr)
  // def maqOlder(i0: UInt, i1: UInt) = cqOlder(i0, i1, maq_enq_ptr)

  // io.enq.ready := maq_available
  // when (io.enq.fire) {
  //   maq(maq_enq_ptr).inst := io.enq.bits
  //   val enq_bound = ((io.enq.bits.vconfig.vl - io.enq.bits.vstart) << io.enq.bits.mem_size) + io.enq.bits.rs1_data
  //   maq(maq_enq_ptr).bound := enq_bound
  //   maq(maq_enq_ptr).agen := false.B
  //   maq(maq_enq_ptr).sdata := false.B
  //   maq_valids(maq_enq_ptr) := true.B
  //   maq_enq_ptr := Mux(maq_enq_ptr === (vParams.vmaqEntries-1).U, 0.U, maq_enq_ptr + 1.U)
  // }

  // val scalar_bound = io.dmem.scalar_check.addr + (1.U << io.dmem.scalar_check.size)
  // io.dmem.scalar_check.conflict := (0 until vParams.vmaqEntries).map { i =>
  //   val addr_conflict = maq(maq_agen_ptr).all || (maq(i).base < scalar_bound && maq(i).bound > io.dmem.scalar_check.addr)
  //   val conflict = addr_conflict && (io.dmem.scalar_check.store || maq(i).store)
  //   maq_valids(i) && conflict
  // }.orR

  // // lagen
  // val laq = Module(new DCEQueue(new LAQEntry, vParams.vlaqEntries))
  // val lagen_maq    = maq(maq_lagen_ptr)
  // val r_lagen_addr = Reg(UInt(paddrBits.W))
  // val r_lagen_eidx = Reg(UInt((1+log2Ceil(maxVLMax)).W))
  // val r_lagen_sidx = Reg(UInt(3.W))
  // val lagen_head   = RegInit(true.B)
  // val lagen_inst = lagen_maq.inst
  // val lagen_addr = Mux(lagen_head, lagen_maq.inst.rs1_data, r_lagen_addr + (r_lagen_sidx << lagen_inst.mem_size))
  // val lagen_eidx = Mux(lagen_head, lagen_maq.inst.vstart  , r_lagen_eidx)
  // val lagen_sidx = Mux(lagen_head, 0.U                    , r_lagen_sidx)
  // val lagen_alignment = lagen_addr(dLenOffBits-1,0)
  // val lagen_load = !lagen_maq.store
  // val lagen_mem_size = Mux(lagen_inst.mop(0), lagen_inst.vconfig.vtype.vsew, lagen_inst.mem_size)
  // val lagen_eg_elems = dLenB.U >> lagen_inst.mem_size
  // val lagen_iterative = lagen_inst.mop =/= mopUnit || lagen_inst.vstart =/= 0.U || !lagen_inst.vm
  // val lagen_next_eidx = lagen_eidx +& Mux(lagen_iterative, 1.U, lagen_eg_elems)
  // val lagen_may_clear = lagen_sidx === lagen_inst.nf && lagen_next_eidx >= Mux(lagen_iterative || lagen_alignment === 0.U,
  //   lagen_inst.vconfig.vl,
  //   lagen_inst.vconfig.vl + lagen_eg_elems)
  // val lagen_masked = !lagen_inst.vm && !(io.vm >> lagen_eidx)(0)

  // val lagen_maq_conflict = (0 until vParams.vmaqEntries).map { i =>
  //   val addr_conflict = lagen_maq.all || maq(i).all || (maq(i).base < lagen_maq.bound && maq(i).bound > lagen_maq.base)
  //   val conflict = addr_conflict && maq(i).store
  //   maq_valids(i) && maqOlder(i.U, maq_lagen_ptr) && conflict
  // }.orR
  // val lagen_mask_hazard = !lagen_inst.vm && io.vm_hazard.hazard
  // val lagen_valid = maq_valids(maq_lagen_ptr) && !lagen_maq.lagen && !lagen_maq_conflict && !lagen_mask_hazard
  // val lagen_ready = (io.dmem.load_req.ready && laq.io.enq.ready) || lagen_masked || !lagen_req.load
  // val lagen_stride = Mux(lagen_inst.mop === mopUnit, dLenB.U, lagen_inst.rs2_data)

  // when (lagen_valid && lagen_ready) {
  //   when (lagen_req.load) {
  //     when (lagen_sidx === lagen_inst.nf) {
  //       r_lagen_addr := lagen_addr + lagen_stride
  //       r_lagen_eidx := lagen_next_eidx
  //       r_lagen_sidx := 0.U
  //     } .otherwise {
  //       r_lagen_sidx := r_lagen_sidx + 1.U
  //     }
  //     lagen_head := false.B
  //   }
  //   when (lagen_may_clear || !lagen_req.load) {
  //     maq(maq_lagen_ptr).lagen := true.B
  //     maq_lagen_ptr := Mux(maq_lagen_ptr === (vParams.vmaqEntries-1).U, 0.U, maq_lagen_ptr + 1.U)
  //     lagen_head := true.B
  //   }
  // }

  // io.vm_hazard.valid := lagen_valid && !lagen_inst.vm
  // io.vm_hazard.vat := lagen_inst.vat

  // io.dmem.load_req.valid := lagen_valid && lagen_load && !lagen_masked && laq.io.enq.ready
  // io.dmem.load_req.bits.addr := lagen_addr
  // io.dmem.load_req.bits.size := Mux(lagen_iterative, lagen_mem_size, log2Ceil(dLenB).U)
  // io.dmem.load_req.bits.data := DontCare
  // io.dmem.load_req.bits.mask := DontCare

  // laq.io.enq.bits.head := lagen_head
  // laq.io.enq.bits.tail := lagen_may_clear
  // laq.io.enq.bits.addr := lagen_addr
  // laq.io.enq.bits.masked := lagen_masked
  // laq.io.enq.valid := lagen_valid && lagen_load && (lagen_masked || io.dmem.load_req.ready)


  // // lresp
  // val lrq = Module(new DCEQueue(UInt(dLen.W), vParams.vlaqEntries, flow=true))
  // lrq.io.enq.valid := io.dmem.load_resp.valid
  // lrq.io.enq.bits := io.dmem.load_resp.bits
  // assert(!(lrq.io.enq.valid && !lrq.io.enq.ready))

  // val lpack = Module(new ShiftPacker(dLenB))
  // lpack.io.push.valid := (laq.io.deq.bits.masked || lrq.io.deq.valid) && laq.io.deq.valid
  // lrq.io.deq.ready    := lpack.io.push.ready && laq.io.deq.valid && !laq.io.deq.bits.masked
  // laq.io.deq.ready    := lpack.io.push.ready && (lrq.io.deq.valid || laq.io.deq.bits.masked)
  // lpack.io.push_data      := io.dmem.load_resp.bits.asTypeOf(Vec(dLenB, UInt(8.W)))
  // lpack.io.push.bits.head := Mux(laq.io.deq.bits.head, laq.io.deq.bits.addr(dLenOffBits-1,0), 0.U)
  // lpack.io.push.bits.tail := Mux(laq.io.deq.bits.tail, laq.io.deq.bits.addr(dLenOffBits-1,0), 0.U)

  // // lseg
  // val lresp_maq = maq(maq_lresp_ptr)
  // val lresp_inst = lresp_maq.inst
  // val lresp_valid = !maq(maq_lresp_ptr).store
  // val lseg = Module(new LoadSegmenter)

  // lpack.io.pop.valid := lseg.io.push.valid && lresp_valid
  // lseg.io.push.ready := lpack.io.pop.ready && lresp_valid
  // lpack.io.pop.bits  := lseg.io.push.bits
  // lseg.io.push_data  := lpack.io.pop_data

  // lseg.io.vl := lresp_inst.vconfig.vl
  // lseg.io.nf := lresp_inst.nf
  // lseg.io.eew := lresp_inst.mem_size

  // io.load.valid := lseg.io.pop.valid
  // io.load.bits := lseg.io.pop.bits
  // lseg.io.pop.ready := io.load.ready

  // when (lseg.io.done) {
  //   maq_valids(maq_lresp_ptr) := false.B
  // }
  // when (lseg.io.done || maq_valids(maq_lresp_ptr) && !lresp_valid) {
  //   maq_lresp_ptr := Mux(maq_lresp_ptr +& 1.U === vParams.vmaqEntries.U, 0.U, maq_lresp_ptr + 1.U)
  // }

  // // sagen


  // // stdata
  // val soq = Module(new DCEQueue(new Bundle {
  //   val last = Bool()
  //   val masked = Bool()
  //   val maq_idx = UInt(log2Ceil(vParams.vmaqEntries).W)
  // }, vParams.vsoqEntries))

  // val spack = Module(new ShiftPacker(dLenB))
  // spack.io.pop.valid := saq.io.deq.valid && (saq.io.deq.bits.masked || io.dmem.store_req.ready) && soq.io.enq.ready
  // spack.io.pop.bits.tail := Mux(saq.io.deq.bits.tail, saq.io.deq.bits.addr(dLenOffBits-1,0), dLenB.U)
  // spack.io.pop.bits.head := Mux(saq.io.deq.bits.head, saq.io.deq.bits.addr(dLenOffBits-1,0), 0.U)

  // saq.io.deq.ready := soq.io.enq.ready && spack.io.pop.ready && (saq.io.deq.bits.masked || io.dmem.store_req.ready)

  // io.dmem.store_req.valid := soq.io.enq.ready && spack.io.pop.ready && saq.io.deq.valid && !saq.io.deq.bits.masked
  // io.dmem.store_req.bits.addr := saq.io.deq.bits.addr
  // io.dmem.store_req.bits.size := saq.io.deq.bits.size
  // io.dmem.store_req.bits.data := spack.io.pop_data.asUInt
  // io.dmem.store_req.bits.mask := ~(0.U(dLenB.W)) // TODO

  // soq.io.enq.bits.last := saq.io.deq.bits.last
  // soq.io.enq.bits

  // // sdata
  // val sdata_maq = maq(maq_sdata_ptr)
  // val sdata_inst = sdata_maq.inst
  // val sdata_valid = maq(maq_sdata_ptr).store
  // val sseg = Module(new StoreSegmenter)

  // when (sseg.io.done || maq_valids(maq_sdata_ptr) && !sdata_valid) {
  //   maq_sdata_ptr := Mux(maq_sdata_ptr +& 1.U === vParams.vmaqEntries.U, 0.U, maq_sdata_ptr + 1.U)
  // }

  // sseg.io.push <> io.vstdata
  // sseg.io.nf := sdata_inst.nf
  // sseg.io.eew := sdata_inst.mem_size
  // sseg.io.vl := sdata_inst.vconfig.vl

  // spack.io.push.valid := sseg.io.pop.valid
  // spack.io.push.data := sseg.io.pop.data
  // spack.io.push.head := 0.U
  // spack.io.push.tail := sseg.io.pop.bytes
  // sseg.io.pop.ready := spack.io.push.ready

  // // val lcoal = Module(new LoadCoalescer)
  // // lcoal.io.lrq <> lrq.io.deq
  // // lcoal.io.laq <> laq.io.deq
  // // io.load <> lcoal.io.out

  // // val scoal = Module(new StoreCoalescer)
  // // scoal.io.saq <> saq.io.deq
  // // scoal.io.stdata <> io.vstdata
  // // io.dmem.store_req <> scoal.io.req
  // // scoal.io.ack <> io.dmem.store_ack

  // // when (lcoal.io.maq_clear.valid) {
  // //   assert(maq_valids(lcoal.io.maq_clear.bits))
  // //   maq_valids(lcoal.io.maq_clear.bits) := false.B
  // // }
  // // when (scoal.io.maq_clear.valid) {
  // //   assert(maq_valids(scoal.io.maq_clear.bits))
  // //   maq_valids(scoal.io.maq_clear.bits) := false.B
  // // }

  // io.busy := maq_valids.orR
}
