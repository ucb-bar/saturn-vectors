package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

class CompactorReq(n: Int) extends Bundle {
  val head  = UInt(log2Ceil(n).W)
  val tail  = UInt(log2Ceil(n).W)
  def count = Mux(tail === 0.U, n.U, tail) - head
}

class Compactor(n: Int, w: Int) extends Module {
  val io = IO(new Bundle {
    val push = Flipped(Decoupled(new CompactorReq(n)))
    val push_data = Input(Vec(n, UInt(w.W)))

    val pop = Flipped(Decoupled(new CompactorReq(n)))
    val pop_data = Output(Vec(n, UInt(w.W)))
  })

  def wshr(in: Seq[UInt], shamt: UInt): Seq[UInt] =
    (0 until in.size).map { i => VecInit(in.drop(i))(shamt) }
  def wshl(in: Seq[UInt], shamt: UInt): Seq[UInt] =
    wshr(in.reverse, shamt).reverse

  val count = RegInit(0.U((1+log2Ceil(n)).W))
  val regs = Seq.fill(n) { Reg(UInt(w.W)) }
  val valid = (1.U << count) - 1.U

  val may_forward = io.pop.bits.count > count

  io.push.ready := n.U +& Mux(io.pop.valid, io.pop.bits.count, 0.U) >= count +& io.push.bits.count
  io.pop.ready := count +& Mux(io.push.valid, io.push.bits.count, 0.U) >= io.pop.bits.count

  val regs_shr = wshr(regs, io.pop.bits.count)
  val valid_shr = valid >> io.pop.bits.count

  when (io.push.fire || io.pop.fire) {
    count := count +& Mux(io.push.fire, io.push.bits.count, 0.U) - Mux(io.pop.fire, io.pop.bits.count, 0.U)
  }

  val push_elems = io.push_data.asTypeOf(Vec(n, UInt(w.W)))
  val push_shr     = wshr((Seq.fill(n)(0.U(w.W)) ++ push_elems), n.U +& io.push.bits.head - count)
  val push_shr_pop = wshr((Seq.fill(n)(0.U(w.W)) ++ push_elems), n.U +& io.push.bits.head +& io.pop.bits.count - count)

  when (io.pop.fire) {
    for (i <- 0 until n) regs(i) := Mux(valid_shr(i), regs_shr(i), push_shr_pop(i))
  } .elsewhen (io.push.fire) {
    for (i <- 0 until n) when (!valid(i)) {
      regs(i) := push_shr(i)
    }
  }

  val out_data = (0 until n).map { i => Mux(valid(i), regs(i), push_shr(i)) }
  io.pop_data := VecInit(wshl(out_data, io.pop.bits.head))
}
