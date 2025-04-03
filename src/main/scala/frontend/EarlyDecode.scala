package saturn.frontend

import chisel3._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import saturn.common._
import saturn.insns.{VectorInstruction, VectorDecoder}

class EarlyVectorDecode(supported_ex_insns: Seq[VectorInstruction])(implicit p: Parameters) extends RocketVectorDecoder()(p) with HasVectorConsts {

  io.vector := false.B
  io.legal := false.B
  io.fp := false.B
  io.read_rs1 := false.B
  io.read_rs2 := false.B
  io.read_frs1 := false.B
  io.write_rd := false.B
  io.write_frd := false.B

  val opcode = io.inst(6,0)

  val width = io.inst(14,12)
  val lumop = io.inst(24,20)
  val sumop = lumop
  val vm = io.inst(25)
  val mop = io.inst(27,26)
  val mew = io.inst(28)
  val nf = io.inst(31,29)
  val funct3 = io.inst(14,12)
  val funct6 = io.inst(31,26)
  val rs1 = io.inst(19,15)
  val rs2 = io.inst(24,20)

  val v_load = opcode === opcLoad && !width.isOneOf(1.U, 2.U, 3.U, 4.U)
  val v_store = opcode === opcStore && !width.isOneOf(1.U, 2.U, 3.U, 4.U)
  val v_arith_maybe = opcode === opcVector && funct3 =/= 7.U
  val v_arith = v_arith_maybe && new VectorDecoder(rs1, rs2, funct3, funct6, io.vconfig.vtype.vsew, supported_ex_insns, Nil).matched

  io.vector := v_load || v_store || v_arith_maybe

  when (v_load || v_store) {
    val unit = mop === 0.U
    val whole = unit && ((v_load && lumop === lumopWhole) || (v_store && sumop === sumopWhole))
    io.legal := mew === 0.U && width.isOneOf(0.U, 5.U, 6.U, 7.U) && (!io.vconfig.vtype.vill || whole)
    when (unit) {
      when (v_load && !lumop.isOneOf(lumopUnit, lumopWhole, lumopMask, lumopFF)) { io.legal := false.B }
      when (v_store && !sumop.isOneOf(sumopUnit, sumopWhole, sumopMask)) { io.legal := false.B }
    }
    when (mew === 1.U) { io.legal := false.B }
    io.read_rs1 := true.B
    io.read_rs2 := mop === mopStrided
  } .elsewhen (v_arith) {
    io.legal := !io.vconfig.vtype.vill
    io.read_rs1 := funct3.isOneOf(OPIVX, OPMVX)
    io.read_frs1 := funct3 === OPFVF
    io.write_rd := funct3 === OPMVV && OPMFunct6(funct6) === OPMFunct6.wrxunary0
    io.write_frd := funct3 === OPFVV && OPFFunct6(funct6) === OPFFunct6.wrfunary0
    io.fp := funct3 === OPFVF
  }
}
