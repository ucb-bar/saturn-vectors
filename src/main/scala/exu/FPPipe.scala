package vector.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import vector.common._

class VFMAPipe(depth: Int, maxType: FType)(implicit p: Parameters) extends FPUModule()(p) {
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val frm = Input(UInt(3.W))
    val op = Input(UInt(2.W))
    val eew = Input(UInt(2.W))
    val a = Input(UInt((maxType.ieeeWidth).W))
    val b = Input(UInt((maxType.ieeeWidth).W))
    val c = Input(UInt((maxType.ieeeWidth).W))
    val out = Output(UInt((maxType.ieeeWidth).W))
  }) 

  val eew_pipe = Pipe(io.valid, io.eew, depth-1)
  val frm_pipe = Pipe(io.valid, io.frm, depth-1)

  val fma_pipe = Module(new MulAddRecFNPipe(depth-1, maxType.exp, maxType.sig))
  fma_pipe.io.validin := io.valid
  fma_pipe.io.op := io.op
  fma_pipe.io.roundingMode := io.frm
  fma_pipe.io.detectTininess := hardfloat.consts.tininess_afterRounding

  if (maxType == FType.D) {
    val input_data = Seq(io.a, io.b, io.c)
    val widen = input_data.zip(Seq.fill(3)(Module(new hardfloat.RecFNToRecFN(8, 24, 11, 53)))).map { case(input, upconvert) =>
      upconvert.io.in := FType.S.recode(input(31,0))
      upconvert.io.roundingMode := io.frm
      upconvert.io.detectTininess := hardfloat.consts.tininess_afterRounding
      upconvert
    }

    fma_pipe.io.a := Mux(io.eew === 2.U, widen(0).io.out, FType.D.recode(io.a))
    fma_pipe.io.b := Mux(io.eew === 2.U, widen(1).io.out, FType.D.recode(io.b))
    fma_pipe.io.c := Mux(io.eew === 2.U, widen(2).io.out, FType.D.recode(io.c))

    val narrow = Module(new hardfloat.RecFNToRecFN(11, 53, 8, 24))
    narrow.io.roundingMode := frm_pipe.bits
    narrow.io.detectTininess := hardfloat.consts.tininess_afterRounding
    narrow.io.in := fma_pipe.io.out

    io.out := Mux(eew_pipe.bits === 2.U, Cat("h00000000".U, FType.S.ieee(narrow.io.out)), FType.D.ieee(fma_pipe.io.out))
  } else {
    fma_pipe.io.a := FType.S.recode(io.a)
    fma_pipe.io.b := FType.S.recode(io.b)
    fma_pipe.io.c := FType.S.recode(io.c)
    io.out := FType.S.ieee(fma_pipe.io.out)
  }
}

class FPPipe(implicit p: Parameters) extends PipelinedFunctionalUnit(3, true)(p) with HasFPUParameters {
  io.iss.sub_dlen := 0.U
  io.set_vxsat := false.B

  override def accepts(f3: UInt, f6: UInt): Bool = f3.isOneOf(OPFVV, OPFVF)

  val default = List(X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X)

  val useOne :: ldst :: wen :: ren1 :: ren2 :: ren3 :: swap12 :: swap23 :: fromint :: toint :: fastpipe :: fma :: fmaCmd0 :: fmaCmd1 :: div :: sqrt :: wflags :: Nil = VecDecode.applyBools(
    io.pipe(0).bits.funct3, io.pipe(0).bits.funct6,
    default, Seq(
      (OPFFunct6.vfadd,   Seq(Y,N,N,Y,Y,Y,N,N,N,N,N,Y,N,N,N,N,N)),
      (OPFFunct6.vfsub,   Seq(Y,N,N,Y,Y,Y,N,N,N,N,N,Y,Y,N,N,N,N)),
      (OPFFunct6.vfmacc,  Seq(N,N,N,Y,Y,Y,N,N,N,N,N,Y,N,N,N,N,Y)),
      (OPFFunct6.vfnmacc, Seq(N,N,N,Y,Y,Y,N,N,N,N,N,Y,Y,Y,N,N,Y)),
      (OPFFunct6.vfmsac,  Seq(N,N,N,Y,Y,Y,N,N,N,N,N,Y,Y,N,N,N,Y)),
      (OPFFunct6.vfnmsac, Seq(N,N,N,Y,Y,Y,N,N,N,N,N,Y,N,Y,N,N,Y)),
    ))  

  val scalar_rs1 = io.pipe(0).bits.funct3.isOneOf(OPFVF)
  val frm = io.pipe(0).bits.frm
  val fma_count = dLen / 64
  val one_d = "h3FF0000000000000".U
  val one_s = "h3F800000".U
  val rec_one_d = FType.D.recode(one_d)
  val rec_one_s = FType.S.recode(one_s)
  val ieee_s_out = Wire(Vec(2 * fma_count, UInt(32.W)))
  val ieee_d_out = Wire(Vec(fma_count, UInt(64.W)))

  val fmaCmd = Cat(fmaCmd1, fmaCmd0)

  val vec_rvs1_s = io.pipe(0).bits.rvs1_data.asTypeOf(Vec(2 * fma_count, UInt(32.W)))
  val vec_rvs2_s = io.pipe(0).bits.rvs2_data.asTypeOf(Vec(2 * fma_count, UInt(32.W)))
  val vec_rvd_s = io.pipe(0).bits.rvd_data.asTypeOf(Vec(2 * fma_count, UInt(32.W)))

  val vec_rvs1_d = io.pipe(0).bits.rvs1_data.asTypeOf(Vec(fma_count, UInt(64.W)))
  val vec_rvs2_d = io.pipe(0).bits.rvs2_data.asTypeOf(Vec(fma_count, UInt(64.W)))
  val vec_rvd_d = io.pipe(0).bits.rvd_data.asTypeOf(Vec(fma_count, UInt(64.W)))

  val sfmas = Seq.fill(fma_count)(Module(new VFMAPipe(3, FType.S)))
  val dfmas = Seq.fill(fma_count)(Module(new VFMAPipe(3, FType.D)))

  sfmas.zipWithIndex.foreach { case(sfma,i) =>
    sfma.io.valid := io.pipe(0).valid
    sfma.io.frm := io.pipe(0).bits.frm
    sfma.io.op := fmaCmd
    sfma.io.eew := io.pipe(0).bits.vd_eew
    sfma.io.a := Mux(useOne, one_s, Mux(scalar_rs1, io.pipe(0).bits.frs1_data(63,32), vec_rvs1_s(fma_count + i)))
    sfma.io.b := vec_rvs2_s(fma_count + i)
    sfma.io.c := Mux(useOne, vec_rvs1_s(fma_count + i), vec_rvd_s(fma_count + i))
    ieee_s_out(fma_count + i) := sfma.io.out
  }

  dfmas.zipWithIndex.foreach { case(dfma, i) =>
    dfma.io.valid := io.pipe(0).valid
    dfma.io.frm := io.pipe(0).bits.frm
    dfma.io.op := fmaCmd
    dfma.io.eew := io.pipe(0).bits.vd_eew
    dfma.io.a := Mux(useOne, one_d, Mux(io.pipe(0).bits.vd_eew === 2.U, Mux(scalar_rs1, io.pipe(0).bits.frs1_data(63,32), vec_rvs1_s(i)), Mux(scalar_rs1, io.pipe(0).bits.frs1_data, vec_rvs1_d(i))))
    dfma.io.b := Mux(io.pipe(0).bits.vd_eew === 2.U, vec_rvs2_s(i), vec_rvs2_d(i))
    dfma.io.c := Mux(io.pipe(0).bits.vd_eew === 2.U, Mux(useOne, vec_rvs1_s(i), vec_rvd_s(i)), Mux(useOne, vec_rvs1_d(i), vec_rvd_d(i))) 
    ieee_s_out(i) := dfma.io.out(31,0)
    ieee_d_out(i) := dfma.io.out
  }

  val ieee_out = Wire(UInt(dLen.W))
  ieee_out := Mux(io.pipe(depth-1).bits.vd_eew === 3.U, ieee_d_out.asTypeOf(UInt(dLen.W)), ieee_s_out.asTypeOf(UInt(dLen.W)))

  io.write.valid := io.pipe(depth-1).valid
  io.write.bits.eg := io.pipe(depth-1).bits.wvd_eg >> 1
  io.write.bits.mask := Fill(2, FillInterleaved(8, io.pipe(depth-1).bits.wmask))
  io.write.bits.data := Fill(2, ieee_out)
}
