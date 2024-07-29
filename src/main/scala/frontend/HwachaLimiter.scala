package saturn.frontend

import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import saturn.common._


class HwachaLimiter(n: Int)(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val vatSz = vParams.vatSz
  val io = IO(new Bundle {
    val block = Output(Bool())
    val fire = Input(Bool())
    val inst = Input(new VectorIssueInst)

    val vat_release = Input(Vec(1 << vatSz, Bool()))
  })

  val slot_valids = RegInit(VecInit.fill(n)(false.B))
  val slot_vats = Reg(Vec(n, UInt(vatSz.W)))
  val head = RegInit(0.U(log2Ceil(n).W))
  val head_p1 = incr(head)
  val head_p2 = incr(head_p1)

  def incr(x: UInt) = Mux(x +& 1.U === n.U, 0.U, x + 1.U)

  val issue_2 = io.inst.vmu
  val issue_3 = io.inst.vmu && io.inst.mop =/= mopUnit
  val slots_available = !slot_valids(head) && (!issue_2 || !slot_valids(head_p1)) && (!issue_3 || !slot_valids(head_p2))

  io.block := !slots_available

  when (io.fire) {
    slot_valids(head) := true.B
    slot_vats(head) := io.inst.vat
    head := incr(head)
    when (issue_2) {
      slot_valids(head_p1) := true.B
      slot_vats(head_p1) := io.inst.vat
      head := incr(head_p1)
    }
    when (issue_3) {
      slot_valids(head_p2) := true.B
      slot_vats(head_p2) := io.inst.vat
      head := incr(head_p2)
    }
  }

  for (i <- 0 until n) {
    when (io.vat_release(slot_vats(i))) {
      slot_valids(i) := false.B
    }
  }
}
