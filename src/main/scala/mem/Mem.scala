package saturn.mem

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import saturn.common._

class LSIQEntry(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val op = new VectorMemMacroOp
  def bound_all = op.mop =/= mopUnit
  val bound_offset = UInt(pgIdxBits.W)

  def containsBlock(addr: UInt) = {
    val cl = addr(pgIdxBits-1,lgCacheBlockBytes)
    val base_cl = op.base_offset >> lgCacheBlockBytes
    val bound_cl = bound_offset >> lgCacheBlockBytes
    ((addr >> pgIdxBits) === op.page) && (bound_all || (
      (base_cl <= cl && bound_cl >= cl)
    ))
  }
  def overlaps(other: LSIQEntry) = {
    (op.page === other.op.page) && (bound_all || other.bound_all || (
      (op.base_offset <= other.bound_offset && bound_offset >= other.op.base_offset)
    ))
  }
}

class IFQEntry(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val head   = UInt(log2Ceil(dLenB).W)
  val tail   = UInt(log2Ceil(dLenB).W)
  val masked = Bool()
  val last   = Bool()
  val lsiq_id  = UInt(lsiqIdBits.W)
  val page_offset = UInt(pgIdxBits.W)
}

class MemRequest(bytes: Int, tagBits: Int)(implicit p: Parameters) extends CoreBundle()(p) {
  val addr = UInt(coreMaxAddrBits.W)
  val data = UInt((bytes*8).W)
  val mask = UInt(bytes.W)
  val tag = UInt(tagBits.W)
}

class LoadResponse(bytes: Int, tagBits: Int)(implicit p: Parameters) extends CoreBundle()(p) {
  val data = UInt((bytes*8).W)
  val tag = UInt(tagBits.W)
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

class VectorMemIO(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val load_req = Decoupled(new MemRequest(dLenB, dmemTagBits))
  val load_resp = Input(Valid(new LoadResponse(dLenB, dmemTagBits)))
  val store_req = Decoupled(new MemRequest(dLenB, dmemTagBits))
  val store_ack = Input(Valid(UInt(dmemTagBits.W)))
}

class VectorSGMemIO(ports: Int)(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val load_req = Vec(ports, Decoupled(new MemRequest(1, dmemTagBits)))
  val load_resp = Vec(ports, Input(Valid(new LoadResponse(1, dmemTagBits))))
  val store_req = Vec(ports, Decoupled(new MemRequest(1, dmemTagBits)))
  val store_ack = Vec(ports, Input(Valid(UInt(dmemTagBits.W))))
}

class VectorMemUnit(sgports: Int)(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val enq = Flipped(Decoupled(new VectorMemMacroOp))

    val dmem = new VectorMemIO
    val sgmem = new VectorSGMemIO(sgports)
    val scalar_check = new ScalarMemOrderCheckIO

    val lresp = Decoupled(new Bundle {
      val data = UInt(dLen.W)
      val debug_vat = UInt(vParams.vatSz.W)
    })
    val sdata = Flipped(Decoupled(new StoreDataMicroOp))

    val maskindex = Flipped(Decoupled(new MaskIndex))

    val busy = Output(Bool())

    val vat_tail = Input(UInt(vParams.vatSz.W))
    val vat_release = Output(Valid(UInt(vParams.vatSz.W)))
  })

  for (i <- 0 until sgports) {
    io.sgmem.load_req(i).valid := false.B
    io.sgmem.load_req(i).bits := DontCare
    io.sgmem.store_req(i).valid := false.B
    io.sgmem.store_req(i).bits := DontCare
  }

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

  val enq_bound_max = (((io.enq.bits.nf +& 1.U) * io.enq.bits.vl) << io.enq.bits.elem_size) + io.enq.bits.base_offset - 1.U
  val enq_bound = Mux((enq_bound_max >> pgIdxBits) =/= 0.U, ~(0.U(pgIdxBits.W)), enq_bound_max)

  when (liq_enq_fire) {
    liq(liq_enq_ptr).op := io.enq.bits
    liq(liq_enq_ptr).bound_offset := enq_bound
    liq_las(liq_enq_ptr) := false.B
  }
  when (siq_enq_fire) {
    siq(siq_enq_ptr).op := io.enq.bits
    siq(siq_enq_ptr).bound_offset := enq_bound
    siq_sss(siq_enq_ptr) := false.B
    siq_sas(siq_enq_ptr) := false.B
  }

  val scalar_store_conflict = (0 until vParams.vsiqEntries).map { i =>
    siq_valids(i) && siq(i).containsBlock(io.scalar_check.addr)
  }.orR
  val scalar_load_conflict = (0 until vParams.vliqEntries).map { i =>
    liq_valids(i) && liq(i).containsBlock(io.scalar_check.addr)
  }.orR
  io.scalar_check.conflict := scalar_store_conflict || (scalar_load_conflict && io.scalar_check.store)

  val maskindex_load = Wire(Bool())

  // Load Addr Sequencing
  val las = Module(new AddrGen)
  val las_order_block = (0 until vParams.vsiqEntries).map { i =>
    val addr_conflict = siq(i).overlaps(liq(liq_las_ptr))
    siq_valids(i) && addr_conflict && vatOlder(siq(i).op.vat, liq(liq_las_ptr).op.vat)
  }.orR
  las.io.valid := liq_las_valid && !las_order_block
  las.io.lsiq_id := liq_las_ptr
  las.io.op := liq(liq_las_ptr).op
  las.io.maskindex.valid := io.maskindex.valid && maskindex_load
  las.io.maskindex.bits := io.maskindex.bits
  liq_las_fire := las.io.done

  val lifq = Module(new LoadOrderBuffer(vParams.vlifqEntries, vParams.vlrobEntries))
  las.io.tag <> lifq.io.reserve
  las.io.out.ready := lifq.io.reserve.valid
  lifq.io.entry := las.io.out.bits

  lifq.io.push.valid := io.dmem.load_resp.valid
  lifq.io.push.bits.data := io.dmem.load_resp.bits.data
  lifq.io.push.bits.tag := io.dmem.load_resp.bits.tag

  val load_arb = Module(new Arbiter(new MemRequest(dLenB, dmemTagBits), 2))
  load_arb.io.in(1) <> las.io.req
  load_arb.io.in(0) <> lifq.io.replay
  load_arb.io.in(0).bits.addr := Cat(liq(lifq.io.replay_liq_id).op.page, lifq.io.replay.bits.addr(pgIdxBits-1,0))
  when (io.dmem.store_req.valid) {
    load_arb.io.in(0).valid := false.B
    lifq.io.replay.ready := false.B
  }
  io.dmem.load_req <> load_arb.io.out
  io.dmem.load_req.bits.mask := ~(0.U(dLenB.W))

  // Load compacting
  val lcu = Module(new Compactor(dLenB, dLenB, UInt(8.W), true))
  lcu.io.push.valid := lifq.io.deq.valid
  lcu.io.push.bits.head := lifq.io.deq.bits.head
  lcu.io.push.bits.tail := lifq.io.deq.bits.tail
  lcu.io.push_data := lifq.io.deq_data.asTypeOf(Vec(dLenB, UInt(8.W)))
  lifq.io.deq.ready := lcu.io.push.ready

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
    val addr_conflict = liq(i).overlaps(siq(siq_sas_ptr))
    liq_valids(i) && addr_conflict && vatOlder(liq(i).op.vat, siq(siq_sas_ptr).op.vat)
  }.orR
  sas.io.valid := siq_sas_valid && !sas_order_block
  sas.io.lsiq_id := siq_sas_ptr
  sas.io.op := siq(siq_sas_ptr).op
  sas.io.maskindex.valid := io.maskindex.valid && !maskindex_load
  sas.io.maskindex.bits := io.maskindex.bits
  siq_sas_fire := sas.io.done

  val store_req_q = Module(new DCEQueue(new MemRequest(dLenB, dmemTagBits), 2))
  io.dmem.store_req <> store_req_q.io.deq
  store_req_q.io.enq <> sas.io.req
  store_req_q.io.enq.bits.data := VecInit(scu.io.pop_data.map(_.data)).asUInt
  store_req_q.io.enq.bits.mask := VecInit(scu.io.pop_data.map(_.mask)).asUInt & sas.io.req.bits.mask

  val store_rob = Module(new ReorderBuffer(Bool(), vParams.vsifqEntries))
  sas.io.tag <> store_rob.io.reserve
  store_rob.io.reserve.ready := sas.io.tag.ready && sas.io.req.valid

  val sifq = Module(new DCEQueue(new IFQEntry, vParams.vsifqEntries))
  sas.io.out.ready := sifq.io.enq.ready && scu.io.pop.ready
  sifq.io.enq.valid := sas.io.out.valid && scu.io.pop.ready
  sifq.io.enq.bits := sas.io.out.bits
  scu.io.pop.valid := sas.io.out.valid && sifq.io.enq.ready
  when (scu.io.pop.fire) {
    for (i <- 0 until dLenB) {
      assert(scu.io.pop_data(i).debug_vat === sas.io.op.vat ||
        i.U < sas.io.out.bits.head ||
        (i.U >= sas.io.out.bits.tail && sas.io.out.bits.tail =/= 0.U))
    }
  }

  scu.io.pop.bits.head := sas.io.out.bits.head
  scu.io.pop.bits.tail := sas.io.out.bits.tail

  store_rob.io.push.valid := io.dmem.store_ack.valid
  store_rob.io.push.bits.tag := io.dmem.store_ack.bits
  store_rob.io.push.bits.data := DontCare

  sifq.io.deq.ready := sifq.io.deq.bits.masked || store_rob.io.deq.valid
  store_rob.io.deq.ready := !sifq.io.deq.bits.masked && sifq.io.deq.valid
  when (store_rob.io.deq.valid) { assert(sifq.io.deq.valid) }
  siq_deq_fire := sifq.io.deq.fire && sifq.io.deq.bits.last
  io.vat_release.valid := siq_deq_fire
  io.vat_release.bits := siq(siq_deq_ptr).op.vat

  io.busy := liq_valids.orR || siq_valids.orR

  maskindex_load := las.io.valid && (vatOlder(las.io.op.vat, sas.io.op.vat) || !sas.io.valid)
  io.maskindex.ready := Mux(maskindex_load, las.io.maskindex.ready, sas.io.maskindex.ready)
}
