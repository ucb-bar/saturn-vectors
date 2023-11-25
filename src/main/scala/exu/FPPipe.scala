package vector.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import vector.common._

class FPPipe(implicit p: Parameters) extends PipelinedFunctionalUnit(3)(p) with HasFPUParameters {

  override def accepts(f3: UInt, f6: UInt): Bool = f3.isOneOf(OPFVV, OPFVF)

  private val X2 = BitPat.dontCare(2)
  val default = List(X,X,X,X,X,X,X,X,X,X,X,X,X,X)

  val ldst :: wen :: ren1 :: ren2 :: ren3 :: swap12 :: swap23 :: fromint :: toint :: fastpipe :: fma :: div :: sqrt :: wflags :: Nil = VecDecode.applyBools(
    io.pipe(0).bits.funct3, io.pipe(0).bits.funct6,
    default, Seq(
      (OPFFunct6.vfmacc, Seq(N,N,Y,Y,Y,N,N,N,N,N,Y,N,N,Y))
    ))  

  val sfma_count = dLen / 32
  val dfma_count = dLen / 64

  val eidx = io.pipe(0).bits.eidx
  val elemsPerEg = (dLen/32).U
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
    req.fmaCmd := 0.U
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
    sfmas(i).io.in.bits := fuInput(Some(FType.S), S, S, 0.U, vec_in1(i), vec_in2(i), vec_in3(i))
    ieee_s_out(i) := FType.S.ieee(sfmas(i).io.out.bits.data)
  }

  val dfmas = Seq.fill(dfma_count)(Module(new FPUFMAPipe(depth-1, FType.D)))
  val ieee_d_out = Wire(Vec(dfma_count, UInt(64.W)))

  for (i <- 0 until dfma_count) {
    dfmas(i).io.in.valid := io.pipe(0).valid && fma && (io.pipe(0).bits.vd_eew === 3.U)
    dfmas(i).io.in.bits := fuInput(Some(FType.D), D, D, 1.U, vec_in1_dfma(i), vec_in2_dfma(i), vec_in3_dfma(i))
    ieee_d_out(i) := FType.D.ieee(dfmas(i).io.out.bits.data)
  }

  when (io.pipe(depth-1).bits.vd_eew === 3.U) {
    ieee_out := ieee_d_out.asTypeOf(UInt(dLen.W))
  } .otherwise {
    ieee_out := ieee_s_out.asTypeOf(UInt(dLen.W))  
  }

  io.writes(0).valid := io.pipe(depth-1).valid && (io.pipe(depth-1).bits.wvd_eg(0) === 0.U)
  io.writes(0).bits.eg := io.pipe(depth-1).bits.wvd_eg >> 1
  io.writes(0).bits.mask := FillInterleaved(8, io.pipe(depth-1).bits.wmask)
  io.writes(0).bits.data := ieee_out

  io.writes(1).valid := io.pipe(depth-1).valid && (io.pipe(depth-1).bits.wvd_eg(0) === 1.U)
  io.writes(1).bits.eg := io.pipe(depth-1).bits.wvd_eg >> 1
  io.writes(1).bits.mask := FillInterleaved(8, io.pipe(depth-1).bits.wmask)
  io.writes(1).bits.data := ieee_out
}
