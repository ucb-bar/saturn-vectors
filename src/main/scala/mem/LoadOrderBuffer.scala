package saturn.mem

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import saturn.common._

class LoadOrderBuffer[T <: Data](entries: Int)(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  require(entries > 0, "Queue must have non-negative number of entries")

  val tagBits = log2Ceil(entries)
  val io = IO(new Bundle {
    val reserve = Decoupled(UInt(tagBits.W))
    val entry = Input(new IFQEntry)
    val push = Input(Valid(new Bundle {
      val data = UInt(dLen.W)
      val tag = UInt(tagBits.W)
    }))
    val deq = Decoupled(new IFQEntry)
    val deq_data = Output(UInt(dLen.W))
  })

  val valids = RegInit(VecInit.fill(entries)(false.B))
  val ram_entries = Reg(Vec(entries, new IFQEntry))
  val ram_data = Reg(Vec(entries, UInt(dLen.W)))
  val enq_ptr = Counter(entries)
  val deq_ptr = Counter(entries)
  val maybe_full = RegInit(false.B)
  val ptr_match = enq_ptr.value === deq_ptr.value
  val empty = ptr_match && !maybe_full
  val full = ptr_match && maybe_full

  io.reserve.valid := !full
  io.reserve.bits := enq_ptr.value
  when (io.reserve.fire) {
    ram_entries(enq_ptr.value) := io.entry
    valids(enq_ptr.value) := io.entry.masked
    enq_ptr.inc()
  }

  when (io.push.fire) {
    assert(!valids(io.push.bits.tag))
    valids(io.push.bits.tag) := !(io.deq.ready && deq_ptr.value === io.push.bits.tag)
    ram_data(io.push.bits.tag) := io.push.bits.data
  }

  io.deq.valid := !empty && (valids(deq_ptr.value) || (io.push.fire && io.push.bits.tag === deq_ptr.value))
  io.deq.bits := ram_entries(deq_ptr.value)
  io.deq_data := Mux(valids(deq_ptr.value), ram_data(deq_ptr.value), io.push.bits.data)

  when (io.deq.fire) {
    deq_ptr.inc()
    valids(deq_ptr.value) := false.B
  }

  when (io.reserve.fire =/= io.deq.fire) {
    maybe_full := io.reserve.fire
  }
}
