package saturn.mem

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import saturn.common._


class ScatterGatherAddrGen(sgSize: BigInt)(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val sgPorts = vParams.vsgPorts
  assert(sgPorts <= mLenB && sgPorts >= 8)
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val lsiq_id = Input(UInt(lsiqIdBits.W))
    val done = Output(Bool())
    val op = Input(new VectorMemMacroOp)
    val index_pop = Decoupled(new CompactorReq(mLenB))
    val index_data = Input(Vec(mLenB, UInt(8.W)))
    val mask_pop = Decoupled(new CompactorReq(mLenB))
    val mask_data = Input(Vec(mLenB, Bool()))
    val store_pop = Decoupled(new CompactorReq(mLenB))
    val store_data = Input(Vec(mLenB, UInt(8.W)))
 
    val req = Vec(sgPorts, Decoupled(new MemRequest(1, sgmemTagBits)))
    val resp = Vec(sgPorts, Input(Valid(new MemResponse(1, sgmemTagBits))))

    val load_resp = Decoupled(new CompactorReq(mLenB))
    val load_data = Output(Vec(mLenB, UInt(8.W)))
  })
  val vsgifqEntries = vParams.vsgifqEntries

  val resp_buffer = Reg(Vec(vsgifqEntries, Vec(sgPorts, UInt(8.W))))
  val resp_busys  = Reg(Vec(vsgifqEntries, Vec(sgPorts, Bool())))
  val resp_bytes  = Reg(Vec(vsgifqEntries, UInt(log2Ceil(mLenB).W)))
  val resp_valids = RegInit(VecInit.fill(vsgifqEntries)(false.B))
  val fired = RegInit(VecInit.fill(sgPorts)(false.B))

  val r_eidx = Reg(UInt((1 + log2Ceil(8*maxVLMax)).W))
  val r_enq  = RegInit(0.U(log2Ceil(vsgifqEntries).W))
  val r_deq  = RegInit(0.U(log2Ceil(vsgifqEntries).W))
  val r_head = RegInit(true.B)
  val r_done = RegInit(false.B)

  val eidx = Mux(r_head, io.op.vstart, r_eidx)
  val idx_incr = mLenB.U >> io.op.idx_size
  val elem_incr = sgPorts.U >> io.op.elem_size
  val incr = min(idx_incr, elem_incr)
  val next_act_elems = min(incr, io.op.vl - eidx)
  val next_eidx = eidx +& incr
  val next_row = Mux(r_enq +& 1.U === vsgifqEntries.U, 0.U, r_enq + 1.U)
  val store = io.op.store

  val enq_stall = resp_valids(r_enq)
  val port_stalls = Wire(Vec(sgPorts, Bool()))
  val fire = io.valid && !port_stalls.orR && !enq_stall && io.index_pop.ready && (io.mask_pop.ready || io.op.vm) && !r_done && (io.store_pop.ready || !store)


  val base = Cat(io.op.page, io.op.base_offset)
  val addrs: Seq[Vec[UInt]] = (0 until 4).map { sew =>
    val offsets = io.index_data.asTypeOf(Vec(mLenB >> sew, UInt((8 << sew).W)))
    VecInit(offsets.map(o => Cat(base >> log2Ceil(sgSize), (o +& base)(log2Ceil(sgSize)-1,0))))
  }


  for (i <- 0 until sgPorts) {
    val port_eidx_offset = (i.U >> io.op.elem_size)
    val port_byte_offset = i.U & ((1.U << io.op.elem_size) - 1.U)
    val port_eidx = eidx +& port_eidx_offset
    val port_masked = !io.op.vm && !io.mask_data(port_eidx_offset)
    val port_addr = VecInit((0 until 4).map { sew => addrs(sew)(port_eidx_offset) })(io.op.idx_size)

    val port_active = io.valid && !r_done && port_eidx < io.op.vl && !port_masked && !fired(i)
    port_stalls(i) := port_active && !io.req(i).ready

    io.req(i).valid := port_active && !enq_stall && io.index_pop.ready && (io.mask_pop.ready || io.op.vm) && (io.store_pop.ready || !store)
    io.req(i).bits.mask  := true.B
    io.req(i).bits.data  := io.store_data(i)
    io.req(i).bits.tag   := r_enq
    io.req(i).bits.addr  := port_addr | port_byte_offset // this is broken if the addrs are misaligned
    io.req(i).bits.store := io.op.store


    when (io.req(i).fire) {
      resp_busys(r_enq)(i) := true.B
      fired(i) := true.B
    }
  }

  when (fire) {
    r_head := false.B
    r_eidx := next_eidx
    r_enq  := next_row
    resp_valids(r_enq) := true.B
    resp_bytes(r_enq) := next_act_elems << io.op.elem_size
    fired := VecInit.fill(sgPorts)(false.B)
    when (next_eidx >= io.op.vl) {
      r_done := true.B
    }
  }

  io.index_pop.valid := io.valid && !r_done && !enq_stall && (io.mask_pop.ready || io.op.vm) && !port_stalls.orR && (io.store_pop.ready || !store)
  io.index_pop.bits.head := 0.U
  io.index_pop.bits.tail := next_act_elems << io.op.idx_size

  io.mask_pop.valid := io.valid && !r_done && !io.op.vm && io.index_pop.ready && !enq_stall && !port_stalls.orR && (io.store_pop.ready || !store)
  io.mask_pop.bits.head := 0.U
  io.mask_pop.bits.tail := next_act_elems

  io.store_pop.valid := io.valid && !r_done && (io.mask_pop.ready || io.op.vm) && io.index_pop.ready && !enq_stall && !port_stalls.orR
  io.store_pop.bits.head := 0.U
  io.store_pop.bits.tail := next_act_elems << io.op.elem_size

  for (i <- 0 until sgPorts) {
    when (io.resp(i).valid) {
      assert(resp_busys(io.resp(i).bits.tag)(i))
      resp_busys(io.resp(i).bits.tag)(i) := false.B
      resp_buffer(io.resp(i).bits.tag)(i) := io.resp(i).bits.data
    }
  }

  val resp_fire = io.valid && resp_valids(r_deq) && !resp_busys(r_deq).orR && (store || io.load_resp.ready)
  io.load_resp.valid := io.valid && resp_valids(r_deq) && !resp_busys(r_deq).orR && !store
  io.load_resp.bits.head := 0.U
  io.load_resp.bits.tail := Mux(resp_bytes(r_deq) === 0.U, sgSize.U, resp_bytes(r_deq))
  io.load_data := resp_buffer(r_deq).asUInt.asTypeOf(Vec(mLenB, UInt(8.W)))

  when (resp_fire) {
    r_deq := Mux(r_deq === (sgSize-1).U, 0.U, r_deq + 1.U)
    resp_valids(r_deq) := false.B
  }

  io.done := r_done && resp_fire && (resp_valids.asUInt === UIntToOH(r_deq))
  when (io.done) { r_head := true.B; r_done := false.B }

}
