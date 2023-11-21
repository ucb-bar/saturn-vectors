package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

class LSIQEntry(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val op = new VectorMemMacroOp
  def base = op.base_addr
  val bound = UInt(paddrBits.W)
  def bound_all = op.mop =/= mopUnit || !op.phys
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

class MaskIndex(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val mask = Bool()
  val index = UInt(64.W)
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

class StoreData(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val data = UInt(dLen.W)
  val mask = UInt(dLenB.W)
  def asMaskedBytes = {
    val bytes = Wire(Vec(dLenB, new MaskedByte))
    for (i <- 0 until dLenB) {
      bytes(i).data := data(((i+1)*8)-1,i*8)
      bytes(i).mask := mask(i)
    }
    bytes
  }
}

class VectorMemUnit(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val enq = Flipped(Decoupled(new VectorMemMacroOp))

    val dmem = new VectorMemInterface

    val lresp = Decoupled(UInt(dLen.W))
    val sdata = Flipped(Decoupled(new StoreData))

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
  when (liq_lss_fire) { ptrIncr(liq_lss_ptr, vParams.vliqEntries); liq_valids(liq_lss_ptr) := false.B; assert(liq_las(liq_lss_ptr)) }

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
  when (siq_sas_fire) { ptrIncr(siq_sas_ptr, vParams.vsiqEntries); siq_sas(siq_sas_ptr) := true.B; assert(siq_sss(siq_sas_ptr) || (siq_sss_fire && siq_sss_ptr === siq_sas_ptr)) }
  when (siq_deq_fire) { ptrIncr(siq_deq_ptr, vParams.vsiqEntries); siq_valids(siq_deq_ptr) := false.B; assert(siq_sas(siq_deq_ptr)) }

  io.enq.ready := Mux(io.enq.bits.store, siq_enq_ready, liq_enq_ready)
  liq_enq_fire := io.enq.valid && liq_enq_ready && !io.enq.bits.store
  siq_enq_fire := io.enq.valid && siq_enq_ready &&  io.enq.bits.store

  val enq_bound = (((io.enq.bits.nf +& 1.U) * io.enq.bits.vl) << io.enq.bits.elem_size) + io.enq.bits.base_addr

  when (liq_enq_fire) {
    liq(liq_enq_ptr).op := io.enq.bits
    liq(liq_enq_ptr).bound := enq_bound
    liq_las(liq_enq_ptr) := false.B
  }
  when (siq_enq_fire) {
    siq(siq_enq_ptr).op := io.enq.bits
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

  val maskindex_load = Wire(Bool())

  // Load Addr Sequencing
  val las = Module(new AddrGen)
  val las_order_block = (0 until vParams.vsiqEntries).map { i =>
    val addr_conflict = (siq(i).bound_all || liq(liq_las_ptr).bound_all ||
      (siq(i).base < liq(liq_las_ptr).bound && siq(i).bound > liq(liq_las_ptr).base)
    )
    siq_valids(i) && addr_conflict && vatOlder(siq(i).op.vat, liq(liq_las_ptr).op.vat)
  }.orR
  las.io.valid := liq_las_valid && !las_order_block
  las.io.op := liq(liq_las_ptr).op

  las.io.maskindex.valid := io.maskindex.valid && maskindex_load
  las.io.maskindex.bits := io.maskindex.bits

  io.dmem.load_req <> las.io.req
  liq_las_fire := las.io.done

  val lifq = Module(new DCEQueue(new IFQEntry, vParams.vlifqEntries))
  lifq.io.enq <> las.io.out

  val lrq = Module(new DCEQueue(UInt(dLen.W), vParams.vlifqEntries))
  lrq.io.enq.valid := io.dmem.load_resp.valid
  lrq.io.enq.bits := io.dmem.load_resp.bits

  // Load compacting
  val lcu = Module(new Compactor(dLenB, dLenB, UInt(8.W), true))
  lcu.io.push.valid := lifq.io.deq.valid && (lrq.io.deq.valid || lifq.io.deq.bits.masked)
  lcu.io.push.bits.head := lifq.io.deq.bits.head
  lcu.io.push.bits.tail := lifq.io.deq.bits.tail
  lcu.io.push_data := lrq.io.deq.bits.asTypeOf(Vec(dLenB, UInt(8.W)))
  lifq.io.deq.ready := lcu.io.push.ready && (lrq.io.deq.valid || lifq.io.deq.bits.masked)
  lrq.io.deq.ready := lcu.io.push.ready && !lifq.io.deq.bits.masked

  // Load segment sequencing
  val lss = Module(new LoadSegmenter)
  lss.io.valid := liq_lss_valid
  lss.io.op := liq(liq_lss_ptr).op
  lcu.io.pop <> lss.io.compactor
  lss.io.compactor_data := lcu.io.pop_data.asUInt
  io.lresp <> lss.io.resp
  liq_lss_fire := lss.io.done

  // Store segment sequencing
  val scu = Module(new Compactor(dLenB, dLenB, new MaskedByte, true))
  val sss = Module(new StoreSegmenter)
  sss.io.valid := siq_sss_valid
  sss.io.op := siq(siq_sss_ptr).op
  scu.io.push <> sss.io.compactor
  scu.io.push_data := sss.io.compactor_data
  sss.io.stdata <> io.sdata
  siq_sss_fire := sss.io.done

  // Store address sequencing
  val sas = Module(new AddrGen)
  val sas_order_block = (0 until vParams.vliqEntries).map { i =>
    val addr_conflict = (liq(i).bound_all || siq(siq_sas_ptr).bound_all ||
      (liq(i).base < siq(siq_sas_ptr).bound && liq(i).bound > siq(siq_sas_ptr).base)
    )
    liq_valids(i) && addr_conflict && vatOlder(liq(i).op.vat, siq(siq_sas_ptr).op.vat)
  }.orR
  sas.io.valid := siq_sas_valid && !sas_order_block
  sas.io.op := siq(siq_sas_ptr).op
  sas.io.maskindex.valid := io.maskindex.valid && !maskindex_load
  sas.io.maskindex.bits := io.maskindex.bits
  siq_sas_fire := sas.io.done

  io.dmem.store_req <> sas.io.req
  io.dmem.store_req.bits.data := VecInit(scu.io.pop_data.map(_.data)).asUInt
  io.dmem.store_req.bits.mask := VecInit(scu.io.pop_data.map(_.mask)).asUInt & sas.io.req.bits.mask

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
  io.vat_release.bits := siq(siq_deq_ptr).op.vat

  io.busy := liq_valids.orR || siq_valids.orR

  maskindex_load := (vatOlder(las.io.op.vat, sas.io.op.vat) || !sas.io.valid)
  io.maskindex.ready := Mux(maskindex_load, las.io.maskindex.ready, sas.io.maskindex.ready)
}
