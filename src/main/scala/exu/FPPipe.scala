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

  val fma_count = dLen / 64
  val sfma_count = dLen / 32
  val dfma_count = dLen / 64

  val one_dp = "h3FF0000000000000".U
  val one_sp = "h3F800000".U

  val vec_in1 = io.pipe(0).bits.rvs1_data.asTypeOf(Vec(sfma_count, UInt(32.W)))
  val vec_in2 = io.pipe(0).bits.rvs2_data.asTypeOf(Vec(sfma_count, UInt(32.W)))
  val vec_in3 = io.pipe(0).bits.rvd_data.asTypeOf(Vec(sfma_count, UInt(32.W)))

  val vec_in1_dfma = io.pipe(0).bits.rvs1_data.asTypeOf(Vec(dfma_count, UInt(64.W)))
  val vec_in2_dfma = io.pipe(0).bits.rvs2_data.asTypeOf(Vec(dfma_count, UInt(64.W)))
  val vec_in3_dfma = io.pipe(0).bits.rvd_data.asTypeOf(Vec(dfma_count, UInt(64.W)))

  def fuInput(fType: Option[FType], typeTagIn: UInt, typeTagOut: UInt, fmt: UInt, in1: UInt, in2: UInt, in3: UInt): FPInput = {
    val req = Wire(new FPInput)
    req.ldst := ldst
    req.wen := wen
    req.ren1 := ren1
    req.ren2 := ren2
    req.ren3 := ren3
    req.swap12 := swap12
    req.swap23 := swap23
    req.typeTagIn := typeTagIn
    req.typeTagOut := typeTagOut
    req.fromint := fromint
    req.toint := toint
    req.fastpipe := fastpipe
    req.fma := fma
    req.div := div
    req.sqrt := sqrt
    req.wflags := wflags
    req.rm := 0.U
    req.fmaCmd := Cat(fmaCmd1,fmaCmd0)
    req.typ := 0.U
    req.fmt := fmt
    req.in1 := fType.get.recode(in1) 
    req.in2 := fType.get.recode(in2) 
    req.in3 := fType.get.recode(in3) 
    req
  } 

  val ieee_out = Wire(UInt(dLen.W))

  val sfmas = Seq.fill(sfma_count)(Module(new FPUFMAPipe(depth-1, FType.S)))
  val ieee_s_out = Wire(Vec(sfma_count, UInt(32.W)))

  for (i <- 0 until sfma_count) {
    sfmas(i).io.in.valid := io.pipe(0).valid && fma && (io.pipe(0).bits.vd_eew === 2.U)
    sfmas(i).io.in.bits := fuInput(Some(FType.S), S, S, 0.U, 
      vec_in2(i), 
      Mux(useOne, one_sp, vec_in1(i)), 
      Mux(useOne, vec_in1(i), vec_in3(i)))
    ieee_s_out(i) := FType.S.ieee(sfmas(i).io.out.bits.data)
  }

  val dfmas = Seq.fill(dfma_count)(Module(new FPUFMAPipe(depth-1, FType.D)))
  val ieee_d_out = Wire(Vec(dfma_count, UInt(64.W)))

  for (i <- 0 until dfma_count) {
    dfmas(i).io.in.valid := io.pipe(0).valid && fma && (io.pipe(0).bits.vd_eew === 3.U)
    dfmas(i).io.in.bits := fuInput(Some(FType.D), D, D, 1.U, 
      vec_in2_dfma(i), 
      Mux(useOne, one_dp, vec_in1_dfma(i)), 
      Mux(useOne, vec_in1_dfma(i), vec_in3_dfma(i)))
    ieee_d_out(i) := FType.D.ieee(dfmas(i).io.out.bits.data)
  }

  when (io.pipe(depth-1).bits.vd_eew === 3.U) {
    ieee_out := ieee_d_out.asTypeOf(UInt(dLen.W))
  } .otherwise {
    ieee_out := ieee_s_out.asTypeOf(UInt(dLen.W))  
  }

  io.write.valid := io.pipe(depth-1).valid
  io.write.bits.eg := io.pipe(depth-1).bits.wvd_eg >> 1
  io.write.bits.mask := Fill(2, FillInterleaved(8, io.pipe(depth-1).bits.wmask))
  io.write.bits.data := Fill(2, ieee_out)
}
