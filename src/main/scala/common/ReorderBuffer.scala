package saturn.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.rocket.Instructions._

class ReorderBuffer[T <: Data](
  gen:            => T,
  val entries:        Int)(implicit val p: Parameters) extends Module {
  require(entries > -1, "Queue must have non-negative number of entries")
  require(entries != 0, "Use companion object Queue.apply for zero entries")

  val tagBits = log2Ceil(entries)
  val io = IO(new Bundle {
    val reserve = Decoupled(UInt(tagBits.W))
    val push = Input(Valid(new Bundle {
      val data = gen
      val tag = UInt(tagBits.W)
    }))
    val deq = Decoupled(gen)
  })

  val valids = RegInit(VecInit.fill(entries)(false.B))
  val ram = Reg(Vec(entries, gen))
  val enq_ptr = Counter(entries)
  val deq_ptr = Counter(entries)
  val maybe_full = RegInit(false.B)
  val ptr_match = enq_ptr.value === deq_ptr.value
  val empty = ptr_match && !maybe_full
  val full = ptr_match && maybe_full

  io.reserve.valid := !full
  io.reserve.bits := enq_ptr.value
  when (io.reserve.fire) {
    enq_ptr.inc()
  }

  when (io.push.fire) {
    assert(!valids(io.push.bits.tag))
    valids(io.push.bits.tag) := true.B
    ram(io.push.bits.tag) := io.push.bits.data
  }

  io.deq.valid := !empty && valids(deq_ptr.value)
  io.deq.bits := ram(deq_ptr.value)

  when (io.deq.fire) {
    deq_ptr.inc()
    valids(deq_ptr.value) := false.B
  }

  when (io.reserve.fire =/= io.deq.fire) {
    maybe_full := io.reserve.fire
  }
}
