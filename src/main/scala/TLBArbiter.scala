package vref

import chisel3._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._

class TLBArbiter(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val in0 = new DCacheTLBPort
    val in1 = new DCacheTLBPort
    val out = Flipped(new DCacheTLBPort)
  })
  io.in0.req.ready := io.out.req.ready
  io.in1.req.ready := io.out.req.ready && !io.in0.req.valid
  io.out.req.valid := io.in0.req.valid || io.in1.req.valid
  io.out.req.bits := Mux(io.in0.req.valid, io.in0.req.bits, io.in1.req.bits)

  io.in0.s1_resp := io.out.s1_resp
  io.in1.s1_resp := io.out.s1_resp
  io.out.s2_kill := false.B
}
