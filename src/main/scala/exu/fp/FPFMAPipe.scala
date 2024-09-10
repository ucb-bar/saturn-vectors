package saturn.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import saturn.common._
import saturn.insns._


class TandemFMAPipe(depth: Int)(implicit p: Parameters) extends FPUModule()(p) with InlineInstance {
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val frm = Input(UInt(3.W))
    val addsub = Input(Bool())
    val mul = Input(Bool())
    val op = Input(UInt(2.W))
    val a_eew = Input(UInt(2.W))
    val b_eew = Input(UInt(2.W))
    val c_eew = Input(UInt(2.W))
    val out_eew = Input(UInt(2.W))
    val a = Input(UInt(64.W))
    val b = Input(UInt(64.W))
    val c = Input(UInt(64.W))
    val mask = Input(UInt(4.W))
    val out = Output(UInt(64.W))
    val exc = Output(UInt(5.W))
  })

  val out_eew_pipe = Pipe(io.valid, io.out_eew, depth-1)
  val frm_pipe = Pipe(io.valid, io.frm, depth-1)
  val mask_pipe = Pipe(io.valid, io.mask, depth-1)

  val fTypes = Seq(FType.D, FType.S, FType.H)

  val fma_results = fTypes.zipWithIndex.map { case(fType, j) =>
    val n = 64 / fType.ieeeWidth
    val fma_eew = log2Ceil(fType.ieeeWidth >> 3)

    val results = (0 until n).map { i =>
      val fma = Module(new MulAddRecFNPipe((depth-1) min 2, fType.exp, fType.sig))
      val validin = io.valid && (io.out_eew === fma_eew.U)
      val msb_idx = ((i + 1) * fType.ieeeWidth) - 1
      val lsb_idx = i * fType.ieeeWidth

      val inputs = Seq((io.a, io.a_eew), (io.b, io.b_eew), (io.c, io.c_eew)).map { case(in, eew) =>
        if (j <= 1) {
          val widen = Module(new hardfloat.RecFNToRecFN(fTypes(j+1).exp, fTypes(j+1).sig, fType.exp, fType.sig))
          widen.io.in := fTypes(j+1).recode(Mux(validin && io.mask(i*(4/n)), in(fTypes(j+1).ieeeWidth-1,0), 0.U))
          widen.io.roundingMode := io.frm
          widen.io.detectTininess := hardfloat.consts.tininess_afterRounding

          Mux(eew =/= fma_eew.U, widen.io.out, fType.recode(Mux(validin && io.mask(i*(4/n)), in(msb_idx, lsb_idx), 0.U)))
        } else {
          fType.recode(Mux(validin && io.mask(i*(4/n)), in(msb_idx, lsb_idx), 0.U))
        }
      }

      fma.io.validin := validin 
      fma.io.op := Mux(validin, io.op, 0.U)
      fma.io.roundingMode := Mux(validin, io.frm, 0.U)
      fma.io.detectTininess := hardfloat.consts.tininess_afterRounding
      fma.io.a := inputs(0) 
      fma.io.b := Mux(io.addsub, 1.U << (fType.ieeeWidth - 1), inputs(1))
      fma.io.c := Mux(io.mul, (inputs(0) ^ inputs(1)) & (1.U << fType.ieeeWidth), inputs(2))

      val out = Pipe(fma.io.validout, fType.ieee(fma.io.out), (depth-3) max 0).bits
      val exc = Pipe(fma.io.validout, fma.io.exceptionFlags, (depth-3) max 0).bits
      (out, exc)
    }
    val out = results.map(_._1).asUInt
    val exc = results.map(_._2).zipWithIndex.map{ case(e, i) => e & Fill(5, mask_pipe.bits(i)) }.reduce(_ | _) 
    (out, exc)
  }

  val out_sel_oh = fTypes.map{ fType => log2Ceil(fType.ieeeWidth >> 3).U === out_eew_pipe.bits}
  io.out := Mux1H(out_sel_oh, fma_results.map(_._1))
  io.exc := Mux1H(out_sel_oh, fma_results.map(_._2))
}

case class FPFMAFactory(depth: Int, sharedScalar: Boolean) extends FunctionalUnitFactory {
  def base_insns = Seq(
    FADD.VV, FADD.VF, FSUB.VV, FSUB.VF, FRSUB.VF,
    FMUL.VV, FMUL.VF,
    FMACC.VV, FMACC.VF, FNMACC.VV, FNMACC.VF,
    FMSAC.VV, FMSAC.VF, FNMSAC.VV, FNMSAC.VF,
    FMADD.VV, FMADD.VF, FNMADD.VV, FNMADD.VF,
    FMSUB.VV, FMSUB.VF, FNMSUB.VV, FNMSUB.VF,
    FWADD.VV, FWADD.VF, FWSUB.VV, FWSUB.VF,
    FWADDW.VV, FWADDW.VF, FWSUBW.VV, FWSUBW.VF,
    FWMUL.VV, FWMUL.VF,
    FWMACC.VV, FWMACC.VF, FWNMACC.VV, FWNMACC.VF,
    FWMSAC.VV, FWMSAC.VF, FWNMSAC.VV, FWNMSAC.VF,
    FREDOSUM.VV, FREDUSUM.VV, FWREDOSUM.VV, FWREDUSUM.VV
  ).map(_.pipelined(depth))
  def insns = if (sharedScalar) base_insns.map(_.elementWise) else base_insns
  def generate(implicit p: Parameters) = if (sharedScalar) {
    new SharedScalarElementwiseFPFMA(depth)(p)
  } else {
    new FPFMAPipe(depth)(p)
  }
}

class FPFMAPipe(depth: Int)(implicit p: Parameters) extends PipelinedFunctionalUnit(depth)(p) with HasFPUParameters {
  val supported_insns = FPFMAFactory(depth, false).insns

  io.stall := false.B
  io.set_vxsat := false.B

  val ctrl = new VectorDecoder(io.pipe(0).bits.funct3, io.pipe(0).bits.funct6, 0.U, 0.U, supported_insns, Seq(
    FPAdd, FPMul, FPSwapVdV2, FPFMACmd))

  val vs1_eew = io.pipe(0).bits.rvs1_eew
  val vs2_eew = io.pipe(0).bits.rvs2_eew
  val vd_eew  = io.pipe(0).bits.vd_eew
  val ctrl_widen_vs2 = vs2_eew =/= vd_eew
  val ctrl_widen_vs1 = vs1_eew =/= vd_eew
  val wmask = io.pipe(0).bits.wmask

  val nTandemFMA = dLenB / 8

  val eidx = Mux(io.pipe(0).bits.acc, 0.U, io.pipe(0).bits.eidx)
  val one_bits = Mux1H(Seq(vd_eew === 3.U, vd_eew === 2.U, vd_eew === 1.U),
                       Seq("h3FF0000000000000".U, "h3F8000003F800000".U, "h3C003C003C003C00".U))
  val fmaCmd = ctrl.uint(FPFMACmd)

  val vec_rvs1 = io.pipe(0).bits.rvs1_data.asTypeOf(Vec(nTandemFMA, UInt(64.W)))
  val vec_rvs2 = io.pipe(0).bits.rvs2_data.asTypeOf(Vec(nTandemFMA, UInt(64.W)))
  val vec_rvd = io.pipe(0).bits.rvd_data.asTypeOf(Vec(nTandemFMA, UInt(64.W)))

  val fma_pipes = Seq.fill(nTandemFMA)(Module(new TandemFMAPipe(depth))).zipWithIndex.map { case(fma_pipe, i) =>
    val widening_vs1_bits = extractElem(io.pipe(0).bits.rvs1_data, 2.U, eidx + i.U)(31,0)
    val rs1_bits = Mux(ctrl_widen_vs1, widening_vs1_bits, vec_rvs1(i))
    val widening_vs2_bits = extractElem(io.pipe(0).bits.rvs2_data, 2.U, eidx + i.U)(31,0)
    val vs2_bits = Mux(ctrl_widen_vs2, widening_vs2_bits, vec_rvs2(i))

    fma_pipe.io.mask := ((vs1_eew === 1.U) && wmask((i*8)+6)) ## ((vs1_eew <= 2.U) && wmask((i*8)+4)) ##
                        ((vs1_eew === 1.U) && wmask((i*8)+2)) ## wmask(i*8)
    fma_pipe.io.addsub := ctrl.bool(FPAdd) && !ctrl.bool(FPMul)
    fma_pipe.io.mul := ctrl.bool(FPMul) && !ctrl.bool(FPAdd)
    fma_pipe.io.out_eew := vd_eew

    // FMA
    when (ctrl.bool(FPMul) && ctrl.bool(FPAdd)) {
      fma_pipe.io.b     := rs1_bits
      fma_pipe.io.b_eew := vs1_eew
      when (ctrl.bool(FPSwapVdV2)) {
        fma_pipe.io.a     := vec_rvd(i)
        fma_pipe.io.a_eew := vd_eew
        fma_pipe.io.c     := vs2_bits
        fma_pipe.io.c_eew := vs2_eew
      } .otherwise {
        fma_pipe.io.a     := vs2_bits
        fma_pipe.io.a_eew := vs2_eew
        fma_pipe.io.c     := vec_rvd(i)
        fma_pipe.io.c_eew := vd_eew
      }
    }
    // Multiply
    .elsewhen (ctrl.bool(FPMul)) {
      fma_pipe.io.a     := vs2_bits
      fma_pipe.io.a_eew := vs2_eew
      fma_pipe.io.b     := rs1_bits
      fma_pipe.io.b_eew := vs1_eew
      fma_pipe.io.c     := 0.U
      fma_pipe.io.c_eew := vs2_eew
    }
    // Add type
    .elsewhen (ctrl.bool(FPAdd)) {
      fma_pipe.io.a     := vs2_bits
      fma_pipe.io.a_eew := vs2_eew
      fma_pipe.io.b     := one_bits
      fma_pipe.io.b_eew := vd_eew
      fma_pipe.io.c     := rs1_bits
      fma_pipe.io.c_eew := vs1_eew
    } .otherwise {
      fma_pipe.io.a     := 0.U
      fma_pipe.io.a_eew := 0.U
      fma_pipe.io.b     := 0.U
      fma_pipe.io.b_eew := 0.U
      fma_pipe.io.c     := 0.U
      fma_pipe.io.c_eew := 0.U
    }

    fma_pipe.io.valid := io.pipe(0).valid
    fma_pipe.io.frm := io.pipe(0).bits.frm
    fma_pipe.io.op := fmaCmd

    fma_pipe.io
  }

  io.write.valid := io.pipe(depth-1).valid
  io.write.bits.eg := io.pipe(depth-1).bits.wvd_eg
  io.write.bits.mask := FillInterleaved(8, io.pipe(depth-1).bits.wmask)
  io.write.bits.data := fma_pipes.map(pipe => pipe.out).asUInt

  io.set_fflags.valid := io.write.valid
  io.set_fflags.bits := fma_pipes.map(pipe => pipe.exc).reduce(_ | _)
  io.scalar_write.valid := false.B
  io.scalar_write.bits := DontCare
}
