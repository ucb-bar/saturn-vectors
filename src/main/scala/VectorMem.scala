package booster

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

class LSAQEntry(implicit p: Parameters) extends CoreBundle()(p) {
  val inst = new VectorIssueInst
  val addr = UInt(vaddrBitsExtended.W)
  val eidx = UInt(log2Ceil(maxVLMax).W)
  val iterative = Bool()
  val head = Bool()
  val tail = Bool()
  val prestart = Bool()
}


class VectorMemUnit(val params: BoosterVectorParams)(implicit p: Parameters) extends CoreModule()(p) with HasBoosterVectorParams {
  val io = IO(new Bundle {
    val status = Input(new MStatus)
    val enq = Flipped(Decoupled(new VectorIssueInst))
    val dmem = new HellaCacheIO
    val tlb = Flipped(new DCacheTLBPort)

    val load = Decoupled(UInt(dLen.W))
    val vstdata = Flipped(Decoupled(UInt(dLen.W)))
  })

  val dmem_simple = Module(new SimpleHellaCacheIF)
  val dmem_arb = Module(new HellaCacheArbiter(2))
  dmem_simple.io.requestor <> dmem_arb.io.mem
  io.dmem <> dmem_simple.io.cache
  val dmem_load = dmem_arb.io.requestor(1)
  val dmem_store = dmem_arb.io.requestor(0)

  val valid = RegInit(false.B)
  val inst = Reg(new VectorIssueInst)
  val addr = Reg(UInt(vaddrBitsExtended.W))
  val eidx = Reg(UInt(log2Ceil(maxVLMax).W))
  val stride = Reg(UInt(vaddrBitsExtended.W))
  val iterative = Reg(Bool())
  val clear = WireInit(false.B)

  io.enq.ready := !valid || clear
  when (io.enq.fire) {
    valid := true.B
    inst := io.enq.bits
    eidx := 0.U
    addr := io.enq.bits.rs1_data
    stride := Mux(io.enq.bits.mop === mopUnit, 1.U << io.enq.bits.mem_size, io.enq.bits.rs2_data)

    val enq_iterative = io.enq.bits.mop =/= mopUnit || io.enq.bits.vstart =/= 0.U
    when (!enq_iterative) {
      stride := dLenB.U
      addr := (io.enq.bits.rs1_data >> dLenOffBits) << dLenOffBits
    }
    iterative := enq_iterative
  } .elsewhen (clear) {
    valid := false.B
  }

  val aligned_addr = (addr >> dLenOffBits) << dLenOffBits
  val alignment = addr(dLenOffBits-1,0)
  val alignment_elems = alignment >> inst.mem_size
  val load = !inst.opcode(5)
  val eg_elems = dLenB.U >> inst.mem_size
  val next_eidx = eidx +& Mux(iterative, 1.U, eg_elems)
  val may_clear = next_eidx >= (inst.vconfig.vl +& Mux(alignment === 0.U && !iterative, eg_elems, 0.U))
  val prestart = eidx < inst.vstart
  val load_tag = RegInit(0.U(log2Ceil(params.vlaqEntries).W))

  val laq = Module(new Queue(new LSAQEntry, params.vlaqEntries))
  val saq = Module(new Queue(new LSAQEntry, params.vsaqEntries))
  val inflight_loads = RegInit(0.U(log2Ceil(params.vlaqEntries).W))

  dmem_load.req.valid := valid && load && !prestart && laq.io.enq.ready
  dmem_load.req.bits.addr := Mux(iterative, addr, aligned_addr)
  dmem_load.req.bits.tag := load_tag
  dmem_load.req.bits.cmd := M_XRD
  dmem_load.req.bits.size := Mux(iterative, inst.mem_size, log2Ceil(vMemDataBits/8).U)
  dmem_load.req.bits.signed := false.B
  dmem_load.req.bits.dprv := io.status.prv
  dmem_load.req.bits.dv := io.status.v
  dmem_load.req.bits.data := DontCare
  dmem_load.req.bits.mask := DontCare
  dmem_load.req.bits.phys := false.B
  dmem_load.req.bits.no_alloc := false.B
  dmem_load.req.bits.no_xcpt := true.B

  dmem_load.s1_kill := false.B
  dmem_load.s1_data := DontCare
  dmem_load.s2_kill := false.B
  dmem_load.keep_clock_enabled := true.B

  when (dmem_load.req.fire) { load_tag := Mux(load_tag === (params.vlaqEntries-1).U, 0.U, load_tag + 1.U) }

  laq.io.enq.bits.inst := inst
  laq.io.enq.bits.eidx := eidx
  laq.io.enq.bits.iterative := iterative
  laq.io.enq.bits.head := eidx === 0.U
  laq.io.enq.bits.tail := may_clear
  laq.io.enq.bits.addr := addr
  laq.io.enq.bits.prestart := prestart
  laq.io.enq.valid := valid && load && (prestart || dmem_load.req.ready)

  saq.io.enq.bits.inst := inst
  saq.io.enq.bits.eidx := eidx
  saq.io.enq.bits.iterative := iterative
  saq.io.enq.bits.head := eidx === 0.U
  saq.io.enq.bits.tail := may_clear
  saq.io.enq.bits.addr := addr
  saq.io.enq.bits.prestart := prestart
  saq.io.enq.valid := valid && !load

  val fire = valid && Mux(load, laq.io.enq.fire, saq.io.enq.fire)

  when (fire) {
    clear := next_eidx >= inst.vconfig.vl
    when (!clear) {
      addr := addr + stride
      eidx := next_eidx
    }
  }

  val load_coalescer = Module(new VectorLoadCoalescer(params))
  val lrq = Module(new Queue(new HellaCacheResp, params.vlaqEntries, flow=true))
  lrq.io.enq.valid := dmem_load.resp.valid
  lrq.io.enq.bits := dmem_load.resp.bits
  assert(!(lrq.io.enq.valid && !lrq.io.enq.ready))

  load_coalescer.io.lrq <> lrq.io.deq
  load_coalescer.io.laq <> laq.io.deq
  io.load <> load_coalescer.io.out

  dontTouch(io.load)

  io.vstdata.ready := saq.io.deq.ready
  dontTouch(io.vstdata)

  dmem_store.req.valid := false.B
  dmem_store.req.bits := DontCare
  dmem_store.s1_kill := false.B
  dmem_store.s1_data := DontCare
  dmem_store.s2_kill := false.B
  dmem_store.keep_clock_enabled := false.B
  saq.io.deq.ready := false.B

}
