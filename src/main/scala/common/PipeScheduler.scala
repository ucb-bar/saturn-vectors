package saturn.common

import chisel3._
import chisel3.util._

class PipeScheduler(nReqs: Int, maxDepth: Int) extends Module {
  val io = IO(new Bundle {
    val reqs = Vec(nReqs, new Bundle {
      val request = Input(Bool())
      val available = Output(Bool())
      val fire = Input(Bool())
      val depth = Input(UInt(log2Ceil(maxDepth).W))
    })
  })

  val tracker = RegInit(0.U(maxDepth.W))

  val ohs = io.reqs.map(r => UIntToOH(r.depth))
  io.reqs.foreach(_.available := true.B)

  for (d <- 0 until maxDepth) {
    var allocated = tracker(d)
    for (i <- 0 until nReqs) {
      val available = !allocated
      val active = io.reqs(i).request && ohs(i)(d)
      when (!available && active) { io.reqs(i).available := false.B }
      allocated = allocated || active
    }
  }
  val allocs = io.reqs.zip(ohs).map { case (r,oh) => Mux(r.fire, oh, 0.U) }.reduce(_|_)
  when (tracker =/= 0.U || allocs =/= 0.U) {
    tracker := (tracker | allocs) >> 1
  }
}
