package vector.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import vector.common._


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

  val vec_rvs1_s = io.pipe(0).bits.rvs1_data.asTypeOf(Vec(2 * fma_count, UInt(32.W)))
  val vec_rvs2_s = io.pipe(0).bits.rvs2_data.asTypeOf(Vec(2 * fma_count, UInt(32.W)))
  val vec_rvd_s = io.pipe(0).bits.rvd_data.asTypeOf(Vec(2 * fma_count, UInt(32.W)))

  val vec_rvs1_d = io.pipe(0).bits.rvs1_data.asTypeOf(Vec(fma_count, UInt(64.W)))
  val vec_rvs2_d = io.pipe(0).bits.rvs2_data.asTypeOf(Vec(fma_count, UInt(64.W)))
  val vec_rvd_d = io.pipe(0).bits.rvd_data.asTypeOf(Vec(fma_count, UInt(64.W)))

  val sfmas = Seq.fill(fma_count)(Module(new MulAddRecFNPipe(depth-1, 8, 24)))
  val dfmas = Seq.fill(fma_count)(Module(new MulAddRecFNPipe(depth-1, 11, 53)))
  val wider_rvs1 = Seq.fill(fma_count)(Module(new hardfloat.RecFNToRecFN(8, 24, 11, 53)))
  val wider_rvs2 = Seq.fill(fma_count)(Module(new hardfloat.RecFNToRecFN(8, 24, 11, 53)))
  val wider_rvd = Seq.fill(fma_count)(Module(new hardfloat.RecFNToRecFN(8, 24, 11, 53)))
  val narrower = Seq.fill(fma_count)(Module(new hardfloat.RecFNToRecFN(11, 53, 8, 24)))

  val ieee_s_out = Wire(Vec(2 * fma_count, UInt(32.W)))
  val ieee_d_out = Wire(Vec(fma_count, UInt(64.W)))

  val fmaCmd = Cat(fmaCmd1, fmaCmd0)

  for (i <- 0 until fma_count) {
    wider_rvs1(i).io.in := Mux(scalar_rs1, FType.S.recode(io.pipe(0).bits.frs1_data(63,32)), FType.S.recode(vec_rvs1_s(i)))
    wider_rvs1(i).io.roundingMode := frm
    wider_rvs1(i).io.detectTininess := hardfloat.consts.tininess_afterRounding
    wider_rvs2(i).io.in := FType.S.recode(vec_rvs2_s(i))
    wider_rvs2(i).io.roundingMode := frm
    wider_rvs2(i).io.detectTininess := hardfloat.consts.tininess_afterRounding
    wider_rvd(i).io.in := FType.S.recode(vec_rvd_s(i))
    wider_rvd(i).io.roundingMode := frm
    wider_rvd(i).io.detectTininess := hardfloat.consts.tininess_afterRounding

    dfmas(i).io.validin := io.pipe(0).valid
    dfmas(i).io.a := Mux(io.pipe(0).bits.vd_eew === 3.U, FType.D.recode(vec_rvs2_d(i)), wider_rvs2(i).io.out)
    dfmas(i).io.b := Mux(useOne, rec_one_d, Mux(io.pipe(0).bits.vd_eew === 3.U, Mux(scalar_rs1, FType.D.recode(io.pipe(0).bits.frs1_data), FType.D.recode(vec_rvs1_d(i))), wider_rvs1(i).io.out))
    dfmas(i).io.c := Mux(useOne, Mux(io.pipe(0).bits.vd_eew === 3.U, FType.D.recode(vec_rvs1_d(i)), wider_rvs1(i).io.out), Mux(io.pipe(0).bits.vd_eew === 3.U, FType.D.recode(vec_rvd_d(i)), wider_rvd(i).io.out))
    dfmas(i).io.op := fmaCmd
    dfmas(i).io.roundingMode := frm
    dfmas(i).io.detectTininess := hardfloat.consts.tininess_afterRounding
    ieee_d_out(i) := FType.D.ieee(dfmas(i).io.out)

    narrower(i).io.in := dfmas(i).io.out
    narrower(i).io.roundingMode := frm
    narrower(i).io.detectTininess := hardfloat.consts.tininess_afterRounding
    
    sfmas(i).io.validin := io.pipe(0).valid && (io.pipe(0).bits.vd_eew === 2.U)
    sfmas(i).io.a := FType.S.recode(vec_rvs2_s(fma_count + i))
    sfmas(i).io.b := Mux(useOne, rec_one_s, Mux(scalar_rs1, FType.S.recode(io.pipe(0).bits.frs1_data(63,32)), FType.S.recode(vec_rvs1_s(fma_count + i))))
    sfmas(i).io.c := Mux(useOne, FType.S.recode(vec_rvs1_s(fma_count + i)), FType.S.recode(vec_rvd_s(fma_count + i)))
    sfmas(i).io.op := fmaCmd
    sfmas(i).io.roundingMode := frm
    sfmas(i).io.detectTininess := hardfloat.consts.tininess_afterRounding 
    ieee_s_out(fma_count + i) := FType.S.ieee(sfmas(i).io.out)

    ieee_s_out(i) := FType.S.ieee(narrower(i).io.out)
  }

  val ieee_out = Wire(UInt(dLen.W))
  ieee_out := Mux(io.pipe(depth-1).bits.vd_eew === 3.U, ieee_d_out.asTypeOf(UInt(dLen.W)), ieee_s_out.asTypeOf(UInt(dLen.W)))


  io.write.valid := io.pipe(depth-1).valid
  io.write.bits.eg := io.pipe(depth-1).bits.wvd_eg >> 1
  io.write.bits.mask := Fill(2, FillInterleaved(8, io.pipe(depth-1).bits.wmask))
  io.write.bits.data := Fill(2, ieee_out)
}
