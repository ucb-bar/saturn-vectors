package saturn.frontend

import chisel3._
import chisel3.util.MuxLookup
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import saturn.common._
import saturn.insns.{VectorInstruction, VectorDecoder, OPUKind, OPUKinds}

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
  // sf.vtle{eee}/sf.vtse{eee} (Load/Store Tile Subset to Memory): claims the mew=1
  // sub-space, which is otherwise always illegal below. rs2 holds a Tile Subset
  // Specifier rather than a lumop/sumop sub-opcode or stride register; only eee=32b
  // (matching the (e32,w1) tile config sf.vtmv.* already requires) is implemented.
  val tile_mem = mew === 1.U && mop === 0.U && vm === 1.U && width === 7.U && io.inst(11,7) === 0.U
  val tile_eee = nf
  val opve = opcode === opcVectorE
  val v_arith_maybe = (opcode === opcVector && funct3 =/= 7.U) || opve
  val v_decode = new VectorDecoder(rs1, rs2, funct3, funct6, io.vconfig.vtype.vsew, supported_ex_insns, Seq(OPUKind), opve)
  val v_arith = v_arith_maybe && v_decode.matched

  // Xsfmm/VME subset legality: the matrix configuration in vtype must match the instruction class.
  // Only TEW=32 configurations exist in the subset: (e8, w4) for sf.mm, (e32, w1) for tile moves.
  val opu_kind = v_decode.uint(OPUKind)
  val vtwiden = io.vconfig.vtype.vtwiden
  // With KMAX = 4, operand specifiers mod 8 must be < 8/KMAX = 2 (rows stay inside one 8-register group)
  val mm_regs_ok = rs1(2,1) === 0.U && rs2(2,1) === 0.U
  val opu_legal = MuxLookup(opu_kind, true.B)(Seq(
    OPUKinds.MM_INT.U  -> (vtwiden === 3.U && io.vconfig.vtype.vsew === 0.U && mm_regs_ok),
    OPUKinds.MM_FP8.U  -> (vtwiden === 3.U && io.vconfig.vtype.vsew === 0.U && mm_regs_ok),
    OPUKinds.MV_V_T.U  -> (vtwiden === 1.U && io.vconfig.vtype.vsew === 2.U),
    OPUKinds.MV_T_V.U  -> (vtwiden === 1.U && io.vconfig.vtype.vsew === 2.U),
    OPUKinds.ZERO.U    -> (vtwiden =/= 0.U),
    OPUKinds.DISCARD.U -> true.B
  ))

  io.vector := v_load || v_store || v_arith_maybe

  when (v_load || v_store) {
    when (tile_mem) {
      io.legal := tile_eee === 2.U && vtwiden === 1.U && io.vconfig.vtype.vsew === 2.U && !io.vconfig.vtype.vill
      io.read_rs1 := true.B
      io.read_rs2 := true.B
    } .otherwise {
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
    }
  } .elsewhen (v_arith) {
    io.legal := !io.vconfig.vtype.vill && opu_legal
    io.read_rs1 := funct3.isOneOf(OPIVX, OPMVX)
    io.read_frs1 := funct3 === OPFVF
    io.write_rd := funct3 === OPMVV && OPMFunct6(funct6) === OPMFunct6.wrxunary0
    io.write_frd := funct3 === OPFVV && OPFFunct6(funct6) === OPFFunct6.wrfunary0
    io.fp := funct3 === OPFVF
  }
}
