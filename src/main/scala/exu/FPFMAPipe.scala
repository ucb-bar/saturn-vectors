package vector.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import vector.common._

class TandemFMAPipe(depth: Int)(implicit p: Parameters) extends FPUModule()(p) {
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val frm = Input(UInt(3.W))
    val fma = Input(Bool())
    val op = Input(UInt(2.W))
    val a_eew = Input(UInt(2.W))
    val b_eew = Input(UInt(2.W))
    val c_eew = Input(UInt(2.W))
    val out_eew = Input(UInt(2.W))
    val a = Input(UInt(64.W))
    val b = Input(UInt(64.W))
    val c = Input(UInt(64.W))
    val mask = Input(UInt(2.W))
    val out = Output(UInt(64.W))
    val exc = Output(UInt(5.W))
  })

  val out_eew_pipe = Pipe(io.valid, io.out_eew, depth-1)
  val frm_pipe = Pipe(io.valid, io.frm, depth-1)
  val mask_pipe = Pipe(io.valid, io.mask, depth-1)

  val sfma = Module(new MulAddRecFNPipe(depth-1, 8, 24))
  sfma.io.validin := io.valid && (io.out_eew === 2.U) && io.mask(1)
  sfma.io.op := io.op
  sfma.io.roundingMode := io.frm
  sfma.io.detectTininess := hardfloat.consts.tininess_afterRounding
  sfma.io.a := FType.S.recode(io.a(63,32))
  sfma.io.b := FType.S.recode(io.b(63,32))
  sfma.io.c := FType.S.recode(io.c(63,32))

  val widen = Seq(io.a, io.b, io.c).zip(Seq.fill(3)(Module(new hardfloat.RecFNToRecFN(8, 24, 11, 53)))).map { case(input, upconvert) =>
    upconvert.io.in := FType.S.recode(input(31,0))
    upconvert.io.roundingMode := io.frm
    upconvert.io.detectTininess := hardfloat.consts.tininess_afterRounding
    upconvert
  }

  val dfma = Module(new MulAddRecFNPipe(depth-1, 11, 53))
  dfma.io.validin := io.valid && (io.out_eew === 3.U) && io.mask(0)
  dfma.io.op := io.op
  dfma.io.roundingMode := io.frm
  dfma.io.detectTininess := hardfloat.consts.tininess_afterRounding
  dfma.io.a := Mux(io.a_eew === 3.U, FType.D.recode(io.a), widen(0).io.out)
  dfma.io.b := Mux(io.b_eew === 3.U, FType.D.recode(io.b), widen(1).io.out)
  dfma.io.c := Mux(io.c_eew === 3.U, FType.D.recode(io.c), widen(2).io.out)

  val narrow = Module(new hardfloat.RecFNToRecFN(11, 53, 8, 24))
  narrow.io.roundingMode := frm_pipe.bits
  narrow.io.detectTininess := hardfloat.consts.tininess_afterRounding
  narrow.io.in := dfma.io.out

  io.out := Mux(out_eew_pipe.bits === 3.U, FType.D.ieee(dfma.io.out), Cat(FType.S.ieee(sfma.io.out), FType.S.ieee(narrow.io.out)))
  io.exc := (dfma.io.exceptionFlags & Fill(5, mask_pipe.bits(0))) | (sfma.io.exceptionFlags & Fill(5, mask_pipe.bits(1)))
}


class FMAPipe(depth: Int)(implicit p: Parameters) extends PipelinedFunctionalUnit(depth)(p) with HasFPUParameters {
  io.iss.sub_dlen := Mux(io.iss.op.opff6.isOneOf(OPFFunct6.fredosum, OPFFunct6.fwredosum),
    dLenOffBits.U - io.iss.op.vd_eew,
    0.U)

  io.set_vxsat := false.B

  lazy val ctrl_table = Seq(
    (OPFFunct6.fadd,     Seq(Y,N,N,N,N)),
    (OPFFunct6.fsub,     Seq(Y,N,N,Y,N)),
    (OPFFunct6.frsub,    Seq(Y,N,N,N,Y)),
    (OPFFunct6.fmul,     Seq(N,Y,N,N,N)),
    (OPFFunct6.fmacc,    Seq(Y,Y,N,N,N)),
    (OPFFunct6.fnmacc,   Seq(Y,Y,N,Y,Y)),
    (OPFFunct6.fmsac,    Seq(Y,Y,N,Y,N)),
    (OPFFunct6.fnmsac,   Seq(Y,Y,N,N,Y)),
    (OPFFunct6.fmadd,    Seq(Y,Y,Y,N,N)),
    (OPFFunct6.fnmadd,   Seq(Y,Y,Y,Y,Y)),
    (OPFFunct6.fmsub,    Seq(Y,Y,Y,Y,N)),
    (OPFFunct6.fnmsub,   Seq(Y,Y,Y,N,Y)),
    (OPFFunct6.fwadd,    Seq(Y,N,N,N,N)),
    (OPFFunct6.fwsub,    Seq(Y,N,N,Y,N)),
    (OPFFunct6.fwaddw,   Seq(Y,N,N,N,N)),
    (OPFFunct6.fwsubw,   Seq(Y,N,N,Y,N)),
    (OPFFunct6.fwmacc,   Seq(Y,Y,N,N,N)),
    (OPFFunct6.fwnmacc,  Seq(Y,Y,N,Y,Y)),
    (OPFFunct6.fwmsac,   Seq(Y,Y,N,Y,N)),
    (OPFFunct6.fwnmsac,  Seq(Y,Y,N,N,Y)),
    (OPFFunct6.fredosum, Seq(Y,N,N,N,N)),
    (OPFFunct6.fredusum, Seq(Y,N,N,N,N)),
    (OPFFunct6.fwredosum,Seq(Y,N,N,N,N)),
    (OPFFunct6.fwredusum,Seq(Y,N,N,N,N)),
  )
  def accepts(f3: UInt, f6: UInt): Bool = VecDecode(f3, f6, ctrl_table.map(_._1))
  val ctrl_add :: ctrl_mul :: ctrl_swap23 :: ctrl_fmaCmd0 :: ctrl_fmaCmd1 :: Nil = VecDecode.applyBools(
    io.pipe(0).bits.funct3, io.pipe(0).bits.funct6,
    Seq.fill(5)(X), ctrl_table
  )

  val vs1_eew = io.pipe(0).bits.rvs1_eew
  val vs2_eew = io.pipe(0).bits.rvs2_eew
  val vd_eew  = io.pipe(0).bits.vd_eew
  val ctrl_widen_vs2 = vs2_eew =/= vd_eew
  val ctrl_widen_vs1 = vs1_eew =/= vd_eew
  io.iss.ready := accepts(io.iss.op.funct3, io.iss.op.funct6)

  val nTandemFMA = dLenB / 8

  val eidx = io.pipe(0).bits.eidx
  val one_bits = Mux(vd_eew === 3.U, "h3FF0000000000000".U, "h3F8000003F800000".U)
  val fmaCmd = Cat(ctrl_fmaCmd1, ctrl_fmaCmd0)

  val vec_rvs1 = io.pipe(0).bits.rvs1_data.asTypeOf(Vec(nTandemFMA, UInt(64.W)))
  val vec_rvs2 = io.pipe(0).bits.rvs2_data.asTypeOf(Vec(nTandemFMA, UInt(64.W)))
  val vec_rvd = io.pipe(0).bits.rvd_data.asTypeOf(Vec(nTandemFMA, UInt(64.W)))

  val fma_pipes = Seq.fill(nTandemFMA)(Module(new TandemFMAPipe(depth))).zipWithIndex.map { case(fma_pipe, i) =>
    val widening_vs1_bits = extract(io.pipe(0).bits.rvs1_data, false.B, 2.U, eidx + i.U)(31,0)
    val rs1_bits = Mux(ctrl_widen_vs1, widening_vs1_bits, vec_rvs1(i))
    val widening_vs2_bits = extract(io.pipe(0).bits.rvs2_data, false.B, 2.U, eidx + i.U)(31,0)
    val vs2_bits = Mux(ctrl_widen_vs2, widening_vs2_bits, vec_rvs2(i))

    fma_pipe.io.mask := Cat((vs1_eew === 2.U) && io.pipe(0).bits.wmask((i*8)+4), io.pipe(0).bits.wmask(i*8))
    fma_pipe.io.fma := ctrl_mul && ctrl_add
    fma_pipe.io.out_eew := vd_eew

    // FMA
    when (ctrl_mul && ctrl_add) {
      fma_pipe.io.b     := rs1_bits
      fma_pipe.io.b_eew := vs1_eew
      when (ctrl_swap23) {
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
    .elsewhen (ctrl_mul) {
      fma_pipe.io.a     := vs2_bits
      fma_pipe.io.a_eew := vs2_eew
      fma_pipe.io.b     := rs1_bits
      fma_pipe.io.b_eew := vs1_eew
      fma_pipe.io.c     := 0.U
      fma_pipe.io.c_eew := vs2_eew
    }
    // Add type
    .elsewhen (ctrl_add) {
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
}
