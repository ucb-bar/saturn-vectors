package saturn.rocket

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._
import saturn.common._
import saturn.mem.{VectorMemIO}

class HellaCacheInterface(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val status = Input(new MStatus)
    val dmem = new HellaCacheIO
    val vec = Flipped(new VectorMemIO)
    val vec_busy = Input(Bool())
    val mem_busy = Output(Bool())
  })

  val hella_simple = Module(new SimpleHellaCacheIF)
  val hella_arb = Module(new HellaCacheArbiter(2))
  hella_simple.io.requestor <> hella_arb.io.mem
  io.dmem <> hella_simple.io.cache

  val hella_load = hella_arb.io.requestor(1)
  val hella_store = hella_arb.io.requestor(0)

  val hella_load_q = Module(new Queue(new HellaCacheReq, 2))
  hella_load.req <> hella_load_q.io.deq
  val hella_store_q = Module(new Queue(new HellaCacheReq, 2))
  hella_store.req <> hella_store_q.io.deq

  hella_arb.io.requestor.foreach { h =>
    h.s1_kill := false.B
    h.s1_data := DontCare
    h.s2_kill := false.B
    h.keep_clock_enabled := io.vec_busy
  }

  val inflights = RegInit(0.U((1+dmemTagBits).W))

  io.vec.load_req.ready        := hella_load_q.io.enq.ready
  hella_load_q.io.enq.valid       := io.vec.load_req.valid
  hella_load_q.io.enq.bits.addr   := io.vec.load_req.bits.addr
  hella_load_q.io.enq.bits.size   := log2Ceil(dLenB).U
  hella_load_q.io.enq.bits.tag    := Cat(0.U, io.vec.load_req.bits.tag)
  hella_load_q.io.enq.bits.cmd    := M_XRD
  hella_load_q.io.enq.bits.signed := false.B
  hella_load_q.io.enq.bits.dprv   := io.status.prv
  hella_load_q.io.enq.bits.dv     := io.status.dv
  hella_load_q.io.enq.bits.data   := DontCare
  hella_load_q.io.enq.bits.mask   := DontCare
  hella_load_q.io.enq.bits.phys   := io.vec.load_req.bits.phys
  hella_load_q.io.enq.bits.no_alloc := false.B
  hella_load_q.io.enq.bits.no_xcpt := true.B

  io.vec.load_resp.valid := hella_load.resp.valid
  io.vec.load_resp.bits.data  := hella_load.resp.bits.data_raw
  io.vec.load_resp.bits.tag   := hella_load.resp.bits.tag

  io.vec.store_req.ready        := hella_store_q.io.enq.ready
  hella_store_q.io.enq.valid       := io.vec.store_req.valid
  hella_store_q.io.enq.bits.addr   := io.vec.store_req.bits.addr
  hella_store_q.io.enq.bits.tag    := Cat(1.U, io.vec.store_req.bits.tag)
  hella_store_q.io.enq.bits.cmd    := M_PWR
  hella_store_q.io.enq.bits.size   := log2Ceil(dLenB).U
  hella_store_q.io.enq.bits.signed := false.B
  hella_store_q.io.enq.bits.dprv   := io.status.prv
  hella_store_q.io.enq.bits.dv     := io.status.dv
  hella_store_q.io.enq.bits.data   := io.vec.store_req.bits.data
  hella_store_q.io.enq.bits.mask   := io.vec.store_req.bits.mask
  hella_store_q.io.enq.bits.phys   := io.vec.store_req.bits.phys
  hella_store_q.io.enq.bits.no_alloc := false.B
  hella_store_q.io.enq.bits.no_xcpt := true.B

  io.vec.store_ack.valid := hella_store.resp.valid
  io.vec.store_ack.bits := hella_store.resp.bits.tag

  io.mem_busy := inflights =/= 0.U

  val load_enq = hella_load_q.io.enq.fire
  val store_enq = hella_store_q.io.enq.fire
  val load_deq = hella_load.resp.fire
  val store_deq = hella_store.resp.fire
  when (load_enq || store_enq || load_deq || store_deq) {
    inflights := inflights + (load_enq +& store_enq) - (load_deq +& store_deq)
  }
}
