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
    val rvs1_eew = Input(UInt(2.W))
    val rvs2_eew = Input(UInt(2.W))
    val vd_eew = Input(UInt(2.W))
    val a = Input(UInt(64.W))
    val b = Input(UInt(64.W))
    val c = Input(UInt(64.W))
    val out = Output(UInt(64.W))
    val exc = Output(UInt(5.W))
  }) 

  val vd_eew_pipe = Pipe(io.valid, io.vd_eew, depth-1)
  val frm_pipe = Pipe(io.valid, io.frm, depth-1)

  val sfma = Module(new MulAddRecFNPipe(depth-1, 8, 24))
  sfma.io.validin := io.valid && (io.vd_eew === 2.U)
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
  dfma.io.validin := io.valid
  dfma.io.op := io.op
  dfma.io.roundingMode := io.frm
  dfma.io.detectTininess := hardfloat.consts.tininess_afterRounding
  dfma.io.a := Mux(io.rvs2_eew === 3.U, FType.D.recode(io.a), widen(0).io.out)
  dfma.io.b := Mux(io.rvs1_eew === 3.U, FType.D.recode(io.b), widen(1).io.out)
  dfma.io.c := Mux((io.vd_eew === 3.U) && (io.fma || io.rvs1_eew === 3.U), FType.D.recode(io.c), widen(2).io.out)

  val narrow = Module(new hardfloat.RecFNToRecFN(11, 53, 8, 24))
  narrow.io.roundingMode := frm_pipe.bits
  narrow.io.detectTininess := hardfloat.consts.tininess_afterRounding
  narrow.io.in := dfma.io.out

  io.out := Mux(vd_eew_pipe.bits === 3.U, FType.D.ieee(dfma.io.out), Cat(FType.S.ieee(sfma.io.out), FType.S.ieee(narrow.io.out)))
  io.exc := dfma.io.exceptionFlags | sfma.io.exceptionFlags
}


class FPPipe(implicit p: Parameters) extends PipelinedFunctionalUnit(3, true)(p) with HasFPUParameters {
  io.iss.sub_dlen := 0.U
  io.set_vxsat := false.B

  lazy val ctrl_table = Seq(
    (OPFFunct6.vfadd,     Seq(Y,N,N,N,N,N,N)),
    (OPFFunct6.vfsub,     Seq(Y,N,N,Y,N,N,N)),
    (OPFFunct6.vfrsub,    Seq(Y,N,N,N,Y,N,N)),
    (OPFFunct6.vfmul,     Seq(N,Y,N,N,N,N,N)),
    (OPFFunct6.vfmacc,    Seq(Y,Y,N,N,N,N,N)),
    (OPFFunct6.vfnmacc,   Seq(Y,Y,N,Y,Y,N,N)),
    (OPFFunct6.vfmsac,    Seq(Y,Y,N,Y,N,N,N)),
    (OPFFunct6.vfnmsac,   Seq(Y,Y,N,N,Y,N,N)),
    (OPFFunct6.vfmadd,    Seq(Y,Y,Y,N,N,N,N)),
    (OPFFunct6.vfnmadd,   Seq(Y,Y,Y,Y,Y,N,N)),
    (OPFFunct6.vfmsub,    Seq(Y,Y,Y,Y,N,N,N)),
    (OPFFunct6.vfnmsub,   Seq(Y,Y,Y,N,Y,N,N)),
    (OPFFunct6.vfwadd,    Seq(Y,N,N,N,N,N,Y)),
    (OPFFunct6.vfwaddw,   Seq(Y,N,N,N,N,Y,Y)),
  )
  override def accepts(f3: UInt, f6: UInt): Bool = f3.isOneOf(OPFVV, OPFVF)
  val ctrl_add :: ctrl_fma :: ctrl_swap23 :: ctrl_fmaCmd0 :: ctrl_fmaCmd1 :: ctrl_widen_vs2 :: ctrl_widen_vd :: Nil = VecDecode.applyBools(
    io.pipe(0).bits.funct3, io.pipe(0).bits.funct6,
    Seq.fill(7)(X), ctrl_table
  )

  val eidx = io.pipe(0).bits.eidx
  val is_opfvf = io.pipe(0).bits.funct3.isOneOf(OPFVF)
  val frs1_data = io.pipe(0).bits.frs1_data
  val one_bits = Mux(io.pipe(0).bits.rvs1_eew === 3.U, "h3FF0000000000000".U, "h3F8000003F800000".U)
  val scalar_operand_bits = Mux(io.pipe(0).bits.rvs1_eew === 3.U, frs1_data, Fill(2, frs1_data(31,0)))
  val vec_rvs1 = io.pipe(0).bits.rvs1_data.asTypeOf(Vec(fmaCount, UInt(64.W)))
  val vec_rvs2 = io.pipe(0).bits.rvs2_data.asTypeOf(Vec(fmaCount, UInt(64.W)))
  val vec_rvd = io.pipe(0).bits.rvd_data.asTypeOf(Vec(fmaCount, UInt(64.W)))
  
  val widening_w = io.pipe(0).bits.rvs2_eew > io.pipe(0).bits.rvs1_eew

  val fmaCmd = Cat(ctrl_fmaCmd1, ctrl_fmaCmd0)

  val ieee_out = Wire(Vec(fmaCount, UInt(64.W)))
  val exc = Wire(Vec(fmaCount, UInt(5.W))) 

  val fma_pipes = Seq.fill(fmaCount)(Module(new TandemFMAPipe(depth))).zipWithIndex.map { case(fma_pipe, i) =>
    val widening_vs1_bits = extract(io.pipe(0).bits.rvs1_data, false.B, 2.U, eidx + i.U)(31,0)
    val rs1_bits = Mux(is_opfvf, scalar_operand_bits, Mux(ctrl_widen_vd, widening_vs1_bits, vec_rvs1(i)))
    val widening_vs2_bits = extract(io.pipe(0).bits.rvs2_data, false.B, 2.U, eidx + i.U)(31,0)
    val vs2_bits = Mux(ctrl_widen_vd && !ctrl_widen_vs2, widening_vs2_bits, vec_rvs2(i))

    fma_pipe.io.fma := ctrl_fma && ctrl_add

    // FMA
    when (ctrl_fma && ctrl_add) {
      fma_pipe.io.b := rs1_bits
      when (ctrl_swap23) {
        fma_pipe.io.a := vec_rvd(i)
        fma_pipe.io.c := vs2_bits
      } .otherwise {
        fma_pipe.io.a := vs2_bits
        fma_pipe.io.c := vec_rvd(i)
      }
    }
    // Multiply
    .elsewhen (ctrl_fma) {
      fma_pipe.io.a := vs2_bits
      fma_pipe.io.b := rs1_bits
      fma_pipe.io.c := 0.U
    }
    // Add type
    .elsewhen (ctrl_add) {
      fma_pipe.io.a := vs2_bits
      fma_pipe.io.b := one_bits
      fma_pipe.io.c := rs1_bits
    } .otherwise {
      fma_pipe.io.a := 0.U
      fma_pipe.io.b := 0.U
      fma_pipe.io.c := 0.U
    }
    
    fma_pipe.io.valid := io.pipe(0).valid
    fma_pipe.io.frm := io.pipe(0).bits.frm
    fma_pipe.io.op := fmaCmd
    fma_pipe.io.rvs1_eew := io.pipe(0).bits.rvs1_eew
    fma_pipe.io.rvs2_eew := io.pipe(0).bits.rvs2_eew
    fma_pipe.io.vd_eew := io.pipe(0).bits.vd_eew
    ieee_out(i) := fma_pipe.io.out
    exc(i) := fma_pipe.io.exc
    fma_pipe
  }

  io.write.valid := io.pipe(depth-1).valid
  io.write.bits.eg := io.pipe(depth-1).bits.wvd_eg >> 1
  io.write.bits.mask := Fill(2, FillInterleaved(8, io.pipe(depth-1).bits.wmask))
  io.write.bits.data := Fill(2, ieee_out.asTypeOf(UInt(dLen.W)))
}
