package saturn.backend

import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import saturn.common._
import saturn.insns._

class ReductionSequencerIO(implicit p: Parameters) extends SequencerIO(Bool()) {
  val rvs = new VectorReadIO

  val acc_data = Decoupled(UInt(dLen.W))
  val acc_init = Output(UInt(dLen.W))
  val acc_init_resp = Input(UInt(dLen.W))
  val acc_fu_resp = Input(Valid(new VectorWrite(dLen)))

  val done = Input(Bool())
}

// This "sequencer" doesn't sequence ops, it just tracks and refreshes the accumulator register for reductions
class ReductionSequencer(supported_insns: Seq[VectorInstruction])(implicit p: Parameters) extends Sequencer[Bool]()(p) {
  def accepts(inst: VectorIssueInst) = !inst.vmu && new VectorDecoder(inst, supported_insns, Seq(Reduction)).bool(Reduction)

  val acc_insns = supported_insns.filter(_.props.contains(Reduction.Y))

  val io = IO(new ReductionSequencerIO)

  val valid = RegInit(false.B)
  val inst = Reg(new BackendIssueInst)
  val acc_e0 = Reg(Bool())
  val acc_busy = Reg(Bool())
  val acc = Reg(Vec(dLenB, UInt(8.W)))
  val acc_init_sel = Reg(Vec(6, Bool()))

  val vd_eew = inst.vconfig.vtype.vsew + inst.wide_vd

  val ctrl     = new VectorDecoder(inst, supported_insns,
    Seq(Elementwise))

  val acc_elementwise = ctrl.bool(Elementwise)

  io.dis.ready := !valid
  when (io.dis.fire) {
    val dis_inst = io.dis.bits
    val dis_vd_eew = dis_inst.vconfig.vtype.vsew + dis_inst.wide_vd
    val dis_ctrl = new VectorDecoder(dis_inst, acc_insns, Seq(AccInitZeros, AccInitOnes, AccInitPos, AccInitNeg))
    valid := true.B
    inst := io.dis.bits

    val acc_init_fp_pos = dis_inst.opff6 === OPFFunct6.fredmin
    val acc_init_fp_neg = dis_inst.opff6 === OPFFunct6.fredmax
    acc_e0 := true.B
    acc_busy := false.B
    acc := Mux1H(Seq(
      (dis_ctrl.bool(AccInitZeros) ,   0.U(dLen.W)),
      (dis_ctrl.bool(AccInitOnes)  , ~(0.U(dLen.W))),
      (dis_ctrl.bool(AccInitPos)   , VecInit.tabulate(4)({sew => Fill(dLenB >> sew, maxPosUInt(sew))})(dis_vd_eew)),
      (dis_ctrl.bool(AccInitNeg)   , VecInit.tabulate(4)({sew => Fill(dLenB >> sew, minNegUInt(sew))})(dis_vd_eew)),
      (acc_init_fp_pos, VecInit.tabulate(4)({sew => Fill(dLenB >> sew, maxPosFPUInt(sew))})(dis_vd_eew)),
      (acc_init_fp_neg, VecInit.tabulate(4)({sew => Fill(dLenB >> sew, minNegFPUInt(sew))})(dis_vd_eew))
    )).asTypeOf(Vec(dLenB, UInt(8.W)))

    acc_init_sel(0) := dis_ctrl.bool(AccInitZeros)
    acc_init_sel(1) := dis_ctrl.bool(AccInitOnes)
    acc_init_sel(2) := dis_ctrl.bool(AccInitPos)
    acc_init_sel(3) := dis_ctrl.bool(AccInitNeg)
    acc_init_sel(4) := acc_init_fp_pos
    acc_init_sel(5) := acc_init_fp_neg
  }

  val raw_hazard = (UIntToOH(io.rvs.req.bits.eg) & io.older_writes) =/= 0.U

  io.rvs.req.valid := valid && acc_e0
  io.rvs.req.bits.eg := getEgId(inst.rs1, 0.U, vd_eew, false.B)
  io.rvs.req.bits.oldest := inst.vat === io.vat_head

  io.seq_hazard.valid := valid && acc_e0
  io.seq_hazard.bits.rintent := get_arch_mask(inst.rs1, 0.U)
  io.seq_hazard.bits.wintent := 0.U
  io.seq_hazard.bits.vat := inst.vat
  io.busy := valid
  io.head := valid && acc_e0
  io.vat := inst.vat
  io.iss.valid := false.B
  io.iss.bits := DontCare

  io.acc_init := Mux1H(acc_init_sel, Seq(
    0.U(dLen.W),
    ~(0.U(dLen.W)),
    VecInit.tabulate(4)({sew => Fill(dLenB >> sew, maxPosUInt(sew))})(vd_eew),
    VecInit.tabulate(4)({sew => Fill(dLenB >> sew, minNegUInt(sew))})(vd_eew),
    VecInit.tabulate(4)({sew => Fill(dLenB >> sew, maxPosFPUInt(sew))})(vd_eew),
    VecInit.tabulate(4)({sew => Fill(dLenB >> sew, minNegFPUInt(sew))})(vd_eew)
  ))

  when (io.rvs.req.fire && !raw_hazard) {
    val v0_mask = eewByteMask(vd_eew)
    val init_resp = io.acc_init_resp.asTypeOf(Vec(dLenB, UInt(8.W)))
    for (i <- 0 until 8) {
      when (v0_mask(i)) {
        acc(i) := init_resp(i)
      }
    }
    acc_e0 := false.B
  }

  io.acc_data.valid := valid && !acc_e0 && !acc_busy
  io.acc_data.bits := acc.asUInt
  when (io.acc_data.fire) {
    acc_busy := true.B
  }
  when (io.done) {
    valid := false.B
  }

  when (io.acc_fu_resp.valid) {
    acc_busy := false.B
    for (i <- 0 until dLenB) when (io.acc_fu_resp.bits.mask(i*8)) { acc(i) := io.acc_fu_resp.bits.data >> (i*8) }
  }
}
