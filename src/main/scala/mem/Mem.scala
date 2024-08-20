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
  val ld_dep_mask = Vec(vParams.vliqEntries, Bool())
  val st_dep_mask = Vec(vParams.vsiqEntries, Bool())

  def containsBlock(addr: UInt) = {
    val cl = addr(pgIdxBits-1,lgCacheBlockBytes)
    val base_cl = op.base_offset >> lgCacheBlockBytes
    val bound_cl = bound_offset >> lgCacheBlockBytes
    (((addr >> pgIdxBits) === op.page) && (base_cl <= cl && bound_cl >= cl)) || bound_all

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
  val store = Bool()
}

class MemResponse(bytes: Int, tagBits: Int)(implicit p: Parameters) extends CoreBundle()(p) {
  val data = UInt((bytes*8).W)
  val tag = UInt(tagBits.W)
}

class ScalarMemOrderCheckIO(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val addr = Input(UInt(coreMaxAddrBits.W))
  val size = Input(UInt(2.W))
  val store = Input(Bool())
  val conflict = Output(Bool())
}

class VectorMemIO(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val load_req = Decoupled(new MemRequest(dLenB, dmemTagBits))
  val load_resp = Input(Valid(new MemResponse(dLenB, dmemTagBits)))
  val store_req = Decoupled(new MemRequest(dLenB, dmemTagBits))
  val store_ack = Input(Valid(new MemResponse(dLenB, dmemTagBits)))
}

class VectorSGMemIO(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val req = Vec(vParams.vsgPorts, Decoupled(new MemRequest(1, sgmemTagBits)))
  val resp = Vec(vParams.vsgPorts, Input(Valid(new MemResponse(1, sgmemTagBits))))
}

class VectorStoreData(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val stdata = UInt(dLen.W)
  val stmask = UInt(dLenB.W)
  val debug_id = UInt(debugIdSz.W)
  def asMaskedBytes = {
    val bytes = Wire(Vec(dLenB, new MaskedByte))
    for (i <- 0 until dLenB) {
      bytes(i).data := stdata(((i+1)*8)-1,i*8)
      bytes(i).mask := stmask(i)
      bytes(i).debug_id := debug_id
    }
    bytes
  }
}


class VectorMemDatapathIO(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val lresp = Decoupled(new Bundle {
    val data = UInt(dLen.W)
    val debug_id = UInt(debugIdSz.W)
  })
  val sdata = Flipped(Decoupled(new VectorStoreData))

  val mask_pop = Decoupled(new CompactorReq(dLenB))
  val mask_data = Input(Vec(dLenB, Bool()))
  val index_pop = Decoupled(new CompactorReq(dLenB))
  val index_data = Input(Vec(dLenB, UInt(8.W)))
}

class VectorMemUnit(sgSize: Option[BigInt] = None)(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val enq = Flipped(Decoupled(new VectorMemMacroOp))

    val dmem = new VectorMemIO
    val sgmem = sgSize.map(_ => new VectorSGMemIO)
    val scalar_check = new ScalarMemOrderCheckIO

    val vu = new VectorMemDatapathIO

    val busy = Output(Bool())
  })

  def ptrIncr(u: UInt, sz: Int): Unit = {
    val n = u +& 1.U
    u := Mux(n === sz.U, 0.U, n)
  }

  val sgas = sgSize.map { size => Module(new ScatterGatherAddrGen(size)) }

  val las = Module(new AddrGen)
  val lifq = Module(new LoadOrderBuffer(vParams.vlifqEntries, vParams.vlrobEntries))
  val lcu = Module(new Compactor(dLenB, dLenB, UInt(8.W), true))
  val lss = Module(new LoadSegmenter)

  val scu = Module(new Compactor(dLenB, dLenB, new MaskedByte, false))
  val sss = Module(new StoreSegmenter)
  val sas = Module(new AddrGen)
  val sifq = Module(new DCEQueue(new IFQEntry, vParams.vsifqEntries))

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

  val enq_bound_max = (((io.enq.bits.nf +& 1.U) * io.enq.bits.vl) << io.enq.bits.elem_size) + io.enq.bits.base_offset - 1.U
  val enq_bound = Mux((enq_bound_max >> pgIdxBits) =/= 0.U, ~(0.U(pgIdxBits.W)), enq_bound_max)

  when (liq_enq_fire) {
    liq(liq_enq_ptr).op := io.enq.bits
    liq(liq_enq_ptr).bound_offset := enq_bound
    liq(liq_enq_ptr).st_dep_mask := siq_valids
    liq_las(liq_enq_ptr) := false.B
    ptrIncr(liq_enq_ptr, vParams.vliqEntries)
    liq_valids(liq_enq_ptr) := true.B
  }
  when (liq_las_fire) {
    ptrIncr(liq_las_ptr, vParams.vliqEntries)
    liq_las(liq_las_ptr) := true.B
  }
  when (liq_lss_fire) {
    ptrIncr(liq_lss_ptr, vParams.vliqEntries)
    liq_valids(liq_lss_ptr) := false.B
    assert(liq_las(liq_lss_ptr) || (liq_lss_ptr === liq_las_ptr && liq_las_fire))
  }

  when (siq_enq_fire) {
    siq(siq_enq_ptr).op := io.enq.bits
    siq(siq_enq_ptr).bound_offset := enq_bound
    siq(siq_enq_ptr).ld_dep_mask := liq_valids
    siq_sss(siq_enq_ptr) := false.B
    siq_sas(siq_enq_ptr) := false.B
    ptrIncr(siq_enq_ptr, vParams.vsiqEntries)
    siq_valids(siq_enq_ptr) := true.B
  }
  when (siq_sss_fire) {
    ptrIncr(siq_sss_ptr, vParams.vsiqEntries)
    siq_sss(siq_sss_ptr) := true.B
  }
  when (siq_sas_fire) {
    ptrIncr(siq_sas_ptr, vParams.vsiqEntries)
    siq_sas(siq_sas_ptr) := true.B
    assert(siq_sss(siq_sas_ptr) || (siq_sss_fire && siq_sss_ptr === siq_sas_ptr))
  }
  when (siq_deq_fire) {
    ptrIncr(siq_deq_ptr, vParams.vsiqEntries)
    siq_valids(siq_deq_ptr) := false.B
    assert(siq_sas(siq_deq_ptr) || (siq_sas_fire && siq_sas_ptr === siq_deq_ptr))
  }

  io.enq.ready := Mux(io.enq.bits.store, siq_enq_ready, liq_enq_ready)
  liq_enq_fire := io.enq.valid && liq_enq_ready && !io.enq.bits.store
  siq_enq_fire := io.enq.valid && siq_enq_ready &&  io.enq.bits.store

  when (liq_lss_fire) { siq.foreach(_.ld_dep_mask(liq_lss_ptr) := false.B) }
  when (siq_deq_fire) { liq.foreach(_.st_dep_mask(siq_deq_ptr) := false.B) }

  val scalar_store_conflict = (0 until vParams.vsiqEntries).map { i =>
    siq_valids(i) && (siq(i).containsBlock(io.scalar_check.addr) || !vParams.enableScalarVectorAddrDisambiguation.B)
  }.orR
  val scalar_load_conflict = (0 until vParams.vliqEntries).map { i =>
    liq_valids(i) && (liq(i).containsBlock(io.scalar_check.addr) || !vParams.enableScalarVectorAddrDisambiguation.B)
  }.orR
  io.scalar_check.conflict := scalar_store_conflict || (scalar_load_conflict && io.scalar_check.store)

  // Send indices/masks to las/sas

  val las_older_than_sas = (liq_las_valid && !liq(liq_las_ptr).st_dep_mask(siq_sas_ptr)) || !siq_sas_valid
  val maskindex_load    = liq_las_valid &&  las_older_than_sas && !liq(liq_las_ptr).op.fast_sg
  val maskindex_store   = siq_sas_valid && !las_older_than_sas && !siq(siq_sas_ptr).op.fast_sg
  val maskindex_gather  = liq_las_valid &&  las_older_than_sas &&  liq(liq_las_ptr).op.fast_sg
  val maskindex_scatter = siq_sas_valid && !las_older_than_sas &&  siq(siq_sas_ptr).op.fast_sg
  las.io.maskindex.index := io.vu.index_data.asUInt
  sas.io.maskindex.index := io.vu.index_data.asUInt
  las.io.maskindex.mask := io.vu.mask_data(0)
  sas.io.maskindex.mask := io.vu.mask_data(0)
  io.vu.mask_pop.valid := false.B
  io.vu.mask_pop.bits.head := 0.U
  io.vu.mask_pop.bits.tail := 1.U
  io.vu.index_pop.valid := false.B
  io.vu.index_pop.bits.head := 0.U
  io.vu.index_pop.bits.tail := 1.U

  when (maskindex_load) {
    io.vu.mask_pop.valid := las.io.maskindex.needs_mask && las.io.maskindex.ready
    io.vu.index_pop.valid := las.io.maskindex.needs_index && las.io.maskindex.ready
    io.vu.index_pop.bits.tail := 1.U << las.io.maskindex.eew
  }
  when (maskindex_store) {
    io.vu.mask_pop.valid := sas.io.maskindex.needs_mask && sas.io.maskindex.ready
    io.vu.index_pop.valid := sas.io.maskindex.needs_index && sas.io.maskindex.ready
    io.vu.index_pop.bits.tail := 1.U << sas.io.maskindex.eew
  }

  // scatter/gather paths
  sgas.foreach { sgas =>
    sgas.io.index_pop.ready := false.B
    sgas.io.mask_pop.ready := false.B
    when (maskindex_gather || maskindex_scatter) {
      io.vu.mask_pop <> sgas.io.mask_pop
      io.vu.index_pop <> sgas.io.index_pop
    }
    sgas.io.index_data := io.vu.index_data
    sgas.io.mask_data := io.vu.mask_data
    sgas.io.valid := maskindex_gather || maskindex_scatter
    sgas.io.lsiq_id := Mux(maskindex_gather, liq_las_ptr, siq_sas_ptr)
    sgas.io.op := Mux(maskindex_gather, liq(liq_las_ptr).op, siq(siq_sas_ptr).op)
    sgas.io.req <> io.sgmem.get.req
    sgas.io.resp <> io.sgmem.get.resp
  }

  las.io.maskindex.valid :=  maskindex_load && (io.vu.mask_pop.ready || !las.io.maskindex.needs_mask) && (io.vu.index_pop.ready || !las.io.maskindex.needs_index)
  sas.io.maskindex.valid := !maskindex_load && (io.vu.mask_pop.ready || !sas.io.maskindex.needs_mask) && (io.vu.index_pop.ready || !sas.io.maskindex.needs_index)

  // Load Addr Sequencing
  val las_order_block = (0 until vParams.vsiqEntries).map { i =>
    val addr_conflict = siq(i).overlaps(liq(liq_las_ptr))
    siq_valids(i) && addr_conflict && liq(liq_las_ptr).st_dep_mask(i)
  }.orR
  val dae_block = !vParams.enableDAE.B && (!io.vu.lresp.ready ||
    io.vu.lresp.bits.debug_id =/= liq(liq_las_ptr).op.debug_id)
  las.io.valid := liq_las_valid && !las_order_block && !liq(liq_las_ptr).op.fast_sg && !dae_block
  las.io.lsiq_id := liq_las_ptr
  las.io.op := liq(liq_las_ptr).op
  liq_las_fire := Mux(liq(liq_las_ptr).op.fast_sg,
    sgas.map(_.io.done && maskindex_gather).getOrElse(false.B), las.io.done)

  las.io.tag <> lifq.io.reserve
  las.io.out.ready := lifq.io.reserve.valid
  lifq.io.entry := las.io.out.bits

  lifq.io.push.valid := io.dmem.load_resp.valid
  lifq.io.push.bits.data := io.dmem.load_resp.bits.data
  lifq.io.push.bits.tag := io.dmem.load_resp.bits.tag

  val load_arb = Module(new Arbiter(new MemRequest(dLenB, dmemTagBits), 2))
  load_arb.io.in(1) <> las.io.req
  load_arb.io.in(1).bits.store := false.B
  load_arb.io.in(0) <> lifq.io.replay
  load_arb.io.in(0).bits.addr := Cat(liq(lifq.io.replay_liq_id).op.page, lifq.io.replay.bits.addr(pgIdxBits-1,0))
  when (io.dmem.store_req.valid) {
    load_arb.io.in(0).valid := false.B
    lifq.io.replay.ready := false.B
  }

  // Load compacting
  lcu.io.push.valid := lifq.io.deq.valid
  lcu.io.push.bits.head := lifq.io.deq.bits.head
  lcu.io.push.bits.tail := lifq.io.deq.bits.tail
  lcu.io.push_data := lifq.io.deq_data.asTypeOf(Vec(dLenB, UInt(8.W)))
  lifq.io.deq.ready := lcu.io.push.ready

  sgas.foreach { sgas =>
    sgas.io.load_resp.ready := false.B
    when (maskindex_gather && !lifq.io.busy) {
      sgas.io.load_resp.ready := lcu.io.push.ready
      lcu.io.push.valid := sgas.io.load_resp.valid
      lcu.io.push.bits := sgas.io.load_resp.bits
      lcu.io.push_data := sgas.io.load_data
    }
  }

  // Load segment sequencing
  lss.io.valid := liq_lss_valid
  lss.io.op := liq(liq_lss_ptr).op
  lcu.io.pop <> lss.io.compactor
  lss.io.compactor_data := lcu.io.pop_data.asUInt
  io.vu.lresp <> lss.io.resp
  liq_lss_fire := lss.io.done

  // Store segment sequencing
  sss.io.valid := siq_sss_valid
  sss.io.op := siq(siq_sss_ptr).op
  scu.io.push <> sss.io.compactor
  scu.io.push_data := sss.io.compactor_data
  sss.io.stdata <> io.vu.sdata
  siq_sss_fire := sss.io.done

  // Store address sequencing
  val sas_order_block = (0 until vParams.vliqEntries).map { i =>
    val addr_conflict = liq(i).overlaps(siq(siq_sas_ptr))
    liq_valids(i) && addr_conflict && siq(siq_sas_ptr).ld_dep_mask(i)
  }.orR
  sas.io.valid := siq_sas_valid && !sas_order_block && !siq(siq_sas_ptr).op.fast_sg
  sas.io.lsiq_id := siq_sas_ptr
  sas.io.op := siq(siq_sas_ptr).op
  siq_sas_fire := Mux(siq(siq_sas_ptr).op.fast_sg, sgas.map(_.io.done && maskindex_scatter).getOrElse(false.B), sas.io.done)

  val store_req_q = Module(new DCEQueue(new MemRequest(dLenB, dmemTagBits), 2))
  store_req_q.io.enq <> sas.io.req
  store_req_q.io.enq.bits.store := true.B
  store_req_q.io.enq.bits.data := VecInit(scu.io.pop_data.map(_.data)).asUInt
  store_req_q.io.enq.bits.mask := VecInit(scu.io.pop_data.map(_.mask)).asUInt & sas.io.req.bits.mask

  val store_rob = Module(new ReorderBuffer(Bool(), vParams.vsifqEntries))
  sas.io.tag <> store_rob.io.reserve
  store_rob.io.reserve.ready := sas.io.tag.ready && sas.io.req.valid

  sas.io.out.ready := sifq.io.enq.ready && scu.io.pop.ready
  sifq.io.enq.valid := sas.io.out.valid && scu.io.pop.ready
  sifq.io.enq.bits := sas.io.out.bits
  scu.io.pop.valid := sas.io.out.valid && sifq.io.enq.ready
  when (scu.io.pop.fire) {
    for (i <- 0 until dLenB) {
      assert(scu.io.pop_data(i).debug_id === sas.io.op.debug_id ||
        i.U < scu.io.pop.bits.head ||
        (i.U >= scu.io.pop.bits.tail && scu.io.pop.bits.tail =/= 0.U))
    }
  }

  scu.io.pop.bits.head := sas.io.out.bits.head
  scu.io.pop.bits.tail := sas.io.out.bits.tail

  sgas.foreach { sgas =>
    sgas.io.store_pop.ready := false.B
    sgas.io.store_data := scu.io.pop_data.map(_.data)
    when (maskindex_scatter && !store_rob.io.busy) {
      sgas.io.store_pop.ready := scu.io.pop.ready
      scu.io.pop.valid := sgas.io.store_pop.valid
      scu.io.pop.bits := sgas.io.store_pop.bits
    }
  }

  store_rob.io.push.valid := io.dmem.store_ack.valid
  store_rob.io.push.bits.tag := io.dmem.store_ack.bits.tag
  store_rob.io.push.bits.data := DontCare

  sifq.io.deq.ready := sifq.io.deq.bits.masked || store_rob.io.deq.valid
  store_rob.io.deq.ready := !sifq.io.deq.bits.masked && sifq.io.deq.valid
  when (store_rob.io.deq.valid) { assert(sifq.io.deq.valid) }
  siq_deq_fire := sifq.io.deq.fire && sifq.io.deq.bits.last

  sgas.foreach { sgas =>
    when (maskindex_scatter && sgas.io.valid && sgas.io.done) { siq_deq_fire := true.B }
  }

  if (vParams.latencyInject) {
    val latency = Wire(UInt(32.W))
    latency := PlusArg("saturn_mem_latency")
    val delay_timer = RegInit(0.U(64.W))
    delay_timer := delay_timer + 1.U
    val load_delay = Module(new DelayQueue(new MemRequest(dLenB, dmemTagBits), 1024, 64))
    val store_delay = Module(new DelayQueue(new MemRequest(dLenB, dmemTagBits), 1024, 64))
    load_delay.io.timer := delay_timer
    store_delay.io.timer := delay_timer
    load_delay.io.delay := latency
    store_delay.io.delay := latency
    load_delay.io.enq <> load_arb.io.out
    store_delay.io.enq <> store_req_q.io.deq
    io.dmem.load_req <> load_delay.io.deq
    io.dmem.store_req <> store_delay.io.deq
  } else {
    io.dmem.load_req <> load_arb.io.out
    io.dmem.store_req <> store_req_q.io.deq
  }
  io.dmem.load_req.bits.mask := ~(0.U(dLenB.W))

  io.busy := liq_valids.orR || siq_valids.orR
}
