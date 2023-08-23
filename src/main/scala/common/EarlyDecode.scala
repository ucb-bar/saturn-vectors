package vref.common

import chisel3._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._

class EarlyVectorDecode(implicit p: Parameters) extends RocketVectorDecoder()(p) with VectorConsts {

  io.legal := false.B
  io.fp := false.B
  io.read_rs1 := false.B
  io.read_rs2 := false.B
  io.read_frs1 := false.B
  io.write_rd := false.B
  io.write_frd := false.B

  val opcode = io.inst(6,0)

  val v_load = opcode === opcLoad
  val v_store = opcode === opcStore
  val v_arith = opcode === opcVector

  val width = io.inst(14,12)
  val lumop = io.inst(24,20)
  val sumop = lumop
  val vm = io.inst(25)
  val mop = io.inst(27,26)
  val mew = io.inst(28)
  val nf = io.inst(31,29)

  when (v_load || v_store) {
    io.legal := mew === 0.U && width.isOneOf(0.U, 5.U, 6.U, 7.U)
    val unit = mop === 0.U
    when (unit) {
      when (v_load && !lumop.isOneOf(lumopUnit, lumopWhole, lumopMask, lumopFF)) { io.legal := false.B }
      when (v_store && !sumop.isOneOf(sumopUnit, sumopWhole, sumopMask)) { io.legal := false.B }
    }
    when (mew === 1.U) { io.legal := false.B }
    io.read_rs1 := true.B
    io.read_rs2 := mop === mopStrided
  }
}
