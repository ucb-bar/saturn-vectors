package vector.common

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
  val masked = Bool()
}

class StoreData(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val data = UInt(dLen.W)
  val mask = UInt(dLenB.W)
}

class MemRequest(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val addr = UInt(coreMaxAddrBits.W)
  val size = UInt(2.W)
  val data = UInt(dLen.W)
  val mask = UInt(dLenB.W)
}

class VectorMemInterface(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val load_req = Decoupled(new MemRequest)
  val load_resp = Input(Valid(UInt(dLen.W)))
  val store_req = Decoupled(new MemRequest)
}

class VectorMemUnit(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val status = Input(new MStatus)
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

    val enq_iterative = io.enq.bits.mop =/= mopUnit || io.enq.bits.vstart =/= 0.U || !io.enq.bits.vm
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
  laq.io.enq.valid := valid && load && (prestart || io.dmem.load_req.ready) && !mask_hazard

  saq.io.enq.bits.inst := inst
  saq.io.enq.bits.eidx := eidx
  saq.io.enq.bits.iterative := iterative
  saq.io.enq.bits.head := eidx === 0.U
  saq.io.enq.bits.tail := may_clear
  saq.io.enq.bits.addr := addr
  saq.io.enq.bits.prestart := prestart
  saq.io.enq.bits.masked := masked
  saq.io.enq.valid := valid && !load && !mask_hazard

  val fire = valid && Mux(load, laq.io.enq.fire, saq.io.enq.fire)

  when (fire) {
    clear := next_eidx >= inst.vconfig.vl
    when (!clear) {
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
  scoal.io.status := io.status
  scoal.io.saq <> saq.io.deq
  scoal.io.stdata <> io.vstdata
  io.dmem.store_req <> scoal.io.req

  io.busy := lrq.io.deq.valid || laq.io.deq.valid || saq.io.deq.valid
}
