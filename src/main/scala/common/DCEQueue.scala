package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.rocket.Instructions._

class DCEQueue[T <: Data](
  val gen:            T,
  val entries:        Int,
  val pipe:           Boolean = false,
  val flow:           Boolean = false)(implicit val p: Parameters) extends Module {
  require(entries > -1, "Queue must have non-negative number of entries")
  require(entries != 0, "Use companion object Queue.apply for zero entries")

  val io = IO(new QueueIO(gen, entries, false) {
    val peek = Output(Vec(entries, Valid(gen)))
  })
  val valids = RegInit(VecInit.fill(entries)(false.B))
  val ram = Reg(Vec(entries, gen))
  val enq_ptr = Counter(entries)
  val deq_ptr = Counter(entries)
  val maybe_full = RegInit(false.B)
  val ptr_match = enq_ptr.value === deq_ptr.value
  val empty = ptr_match && !maybe_full
  val full = ptr_match && maybe_full
  val do_enq = WireDefault(io.enq.fire)
  val do_deq = WireDefault(io.deq.fire)

  for (i <- 0 until entries) {
    io.peek(i).bits := ram(i)
    io.peek(i).valid := valids(i)
  }

  when(do_deq) {
    deq_ptr.inc()
    valids(deq_ptr.value) := false.B
  }

  when(do_enq) {
    ram(enq_ptr.value) := io.enq.bits
    valids(enq_ptr.value) := true.B
    enq_ptr.inc()
  }

  when(do_enq =/= do_deq) {
    maybe_full := do_enq
  }

  io.deq.valid := !empty
  io.enq.ready := !full

  io.deq.bits := ram(deq_ptr.value)

  if (flow) {
    when(io.enq.valid) { io.deq.valid := true.B }
    when(empty) {
      io.deq.bits := io.enq.bits
      do_deq := false.B
      when(io.deq.ready) { do_enq := false.B }
    }
  }

  if (pipe) {
    when(io.deq.ready) { io.enq.ready := true.B }
  }

  val ptr_diff = enq_ptr.value - deq_ptr.value

  if (isPow2(entries)) {
    io.count := Mux(maybe_full && ptr_match, entries.U, 0.U) | ptr_diff
  } else {
    io.count := Mux(
      ptr_match,
      Mux(maybe_full, entries.asUInt, 0.U),
      Mux(deq_ptr.value > enq_ptr.value, entries.asUInt + ptr_diff, ptr_diff)
    )
  }
}
