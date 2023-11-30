package vector.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import vector.common._

class ElementwiseMultiplyPipe(depth: Int)(implicit p: Parameters) extends PipelinedFunctionalUnit(depth, false)(p) {

  io.iss.sub_dlen := log2Ceil(dLenB).U - io.iss.op.rvs1_eew
  io.set_vxsat := false.B

  val aluFn = new ALUFN
  lazy val ctrl_table = Seq(
    (OPMFunct6.mul    , Seq(N,X,X,N,N,X)),
    (OPMFunct6.mulh   , Seq(Y,Y,Y,N,N,X)),
    (OPMFunct6.mulhu  , Seq(Y,N,N,N,N,X)),
    (OPMFunct6.mulhsu , Seq(Y,N,Y,N,N,X)),
    (OPMFunct6.wmul   , Seq(N,Y,Y,N,N,X)),
    (OPMFunct6.wmulu  , Seq(N,N,N,N,N,X)),
    (OPMFunct6.wmulsu , Seq(N,N,Y,N,N,X)),
    (OPMFunct6.macc   , Seq(X,X,X,N,Y,N)),
    (OPMFunct6.nmsac  , Seq(X,X,X,N,Y,Y)),
    (OPMFunct6.madd   , Seq(X,X,X,Y,Y,N)),
    (OPMFunct6.nmsub  , Seq(X,X,X,Y,Y,Y)),
    (OPMFunct6.wmaccu , Seq(X,N,N,N,Y,N)),
    (OPMFunct6.wmacc  , Seq(X,Y,Y,N,Y,N)),
    (OPMFunct6.wmaccsu, Seq(X,Y,N,N,Y,N)),
    (OPMFunct6.wmaccus, Seq(X,N,Y,N,Y,N)),
  )

  override def accepts(f3: UInt, f6: UInt): Bool = VecDecode(f3, f6, ctrl_table.map(_._1))

  val ctrl_hi :: ctrl_sign1 :: ctrl_sign2 :: ctrl_swapvdvs2 :: ctrl_madd :: ctrl_sub :: Nil = VecDecode.applyBools(
    io.pipe(0).bits.funct3, io.pipe(0).bits.funct6,
    Seq.fill(6)(X), ctrl_table)

  val in_eew = io.pipe(0).bits.rvs1_eew
  val out_eew = io.pipe(0).bits.vd_eew
  val eidx = io.pipe(0).bits.eidx

  val in_vs1 = extract(io.pipe(0).bits.rvs1_data, ctrl_sign1, in_eew , eidx)(64,0)
  val in_vs2 = extract(io.pipe(0).bits.rvs2_data, ctrl_sign2, in_eew , eidx)(64,0)
  val in_vd  = extract(io.pipe(0).bits.rvd_data , false.B   , out_eew, eidx)(64,0)

  val prod = in_vs1.asSInt * Mux(ctrl_swapvdvs2, in_vd, in_vs2).asSInt
  val hi = VecInit.tabulate(4)({ eew => prod >> (8 << eew) })(out_eew)(63,0)
  val lo = VecInit.tabulate(4)({ eew => prod((8 << eew)-1,0)})(out_eew)(63,0)
  val madd = Mux(ctrl_sub, ~lo, lo) + ctrl_sub + Mux(ctrl_swapvdvs2, in_vs2, in_vd)
  val out = Mux(ctrl_madd, madd, Mux(ctrl_hi, hi, lo))

  val pipe_out = Pipe(io.pipe(0).valid, out, depth-1).bits

  val wdata = VecInit.tabulate(4)({ eew => Fill(dLenB >> eew, pipe_out((8<<eew)-1,0)) })(io.pipe(depth-1).bits.vd_eew)
  io.write.valid     := io.pipe(depth-1).valid
  io.write.bits.eg   := io.pipe(depth-1).bits.wvd_eg
  io.write.bits.data := wdata
  io.write.bits.mask := FillInterleaved(8, io.pipe(depth-1).bits.wmask)
}

class SegmentedMultiplyPipe(depth: Int)(implicit p: Parameters) extends PipelinedFunctionalUnit(depth, true)(p) {
  io.iss.sub_dlen := 0.U
  io.set_vxsat := false.B

  lazy val ctrl_table = Seq(  
      (OPMFunct6.mulhu,   Seq(N,N,N,N,X,X,Y)),
      (OPMFunct6.mul,     Seq(Y,Y,N,N,X,X,N)),
      (OPMFunct6.mulhsu,  Seq(N,Y,N,N,X,X,Y)),
      (OPMFunct6.mulh,    Seq(Y,Y,N,N,X,X,Y)),
      (OPMFunct6.madd,    Seq(Y,Y,Y,Y,N,N,N)),
      (OPMFunct6.nmsub,   Seq(Y,Y,Y,Y,N,Y,N)),
      (OPMFunct6.macc,    Seq(Y,Y,N,Y,N,N,N)),
      (OPMFunct6.nmsac,   Seq(Y,Y,N,Y,N,Y,N)),

      (OPMFunct6.wmulu,   Seq(N,N,N,N,X,X,X)),
      (OPMFunct6.wmulsu,  Seq(N,Y,N,N,X,X,X)),
      (OPMFunct6.wmul,    Seq(Y,Y,N,N,X,X,X)),
      (OPMFunct6.wmaccu,  Seq(N,N,N,Y,Y,N,X)),
      (OPMFunct6.wmacc,   Seq(Y,Y,N,Y,Y,N,X)),
      (OPMFunct6.wmaccus, Seq(N,Y,N,Y,Y,N,X)),
      (OPMFunct6.wmaccsu, Seq(Y,N,N,Y,Y,N,X))
  )
  override def accepts(f3: UInt, f6: UInt): Bool = VecDecode(f3, f6, ctrl_table.map(_._1))
  val ctrl_rvs1_signed :: ctrl_rvs2_signed :: ctrl_madd :: ctrl_acc :: ctrl_wacc :: ctrl_sub :: ctrl_hi :: Nil = VecDecode.applyBools(
    io.pipe(0).bits.funct3, io.pipe(0).bits.funct6,
    Seq.fill(7)(BitPat.dontCare(1)), ctrl_table)

  val in_eew = io.pipe(0).bits.rvs1_eew
  val out_eew = io.pipe(0).bits.vd_eew

  val in1 = io.pipe(0).bits.rvs1_data
  val in2 = Mux(ctrl_madd, io.pipe(0).bits.rvd_data, io.pipe(0).bits.rvs2_data)
  val ind = Mux(ctrl_madd, io.pipe(0).bits.rvs2_data, io.pipe(0).bits.rvd_data)

  val numSegMul = dLen/xLen
  val vMul = Seq.fill(numSegMul) {Module(new SegmentedIntegerMultiplier(1))}
  for (i <- 0 until numSegMul) {
    vMul(i).io.in1_signed := ctrl_rvs1_signed
    vMul(i).io.in2_signed := ctrl_rvs2_signed
    vMul(i).io.eew := in_eew
    vMul(i).io.in1 := in1((i+1)*xLen-1, i*xLen)
    vMul(i).io.in2 := in2((i+1)*xLen-1, i*xLen)
  }
  val vWideMulOut = VecInit((0 until numSegMul).map { i => vMul(i).io.out_data }).asUInt
  val vNarrowMulOutEew = Wire(Vec(4, UInt(dLen.W)))
  for (eew <- 0 until 4) {
    val wide = vWideMulOut.asTypeOf(Vec(dLenB >> eew, UInt((16 << eew).W)))
    vNarrowMulOutEew(eew) := wide.map { i => 
    println("i: " + i + " eew: " + eew + " wide: " + wide)
    println("(16 << eew)-1, (8 << eew)", (16 << eew)-1, (8 << eew))
      val lo = wide(i)((8 << eew)-1, 0)
      val hi = wide(i)((16 << eew)-1, (8 << eew))
      Mux(ctrl_hi, hi, lo)
    }.asUInt
  }
  val vNarrowMulOut = vNarrowMulOutEew(out_eew)


  //TODO: do something like this to support vwmacc
  val halfSel = VecInit((0 until 3).map { eew =>
    (io.pipe(0).bits.eidx(log2Ceil(vLen)-eew-1 - 1))
  })(out_eew)
  // val halfSel = io.pipe(0).bits.eidx(log2Ceil(dLen)+1 - 1)
  val wacc_in1 = Mux(halfSel, vWideMulOut >> dLen, vWideMulOut)
  val vAcc = Module(new SegmentedAdd)
  vAcc.io.ctrl_sub := ctrl_sub
  vAcc.io.eew := out_eew
  vAcc.io.in1 := Mux(ctrl_wacc, wacc_in1, vNarrowMulOut)
  vAcc.io.in2 := ind

  val narrow_out = Mux(ctrl_acc, vAcc.io.out, vNarrowMulOut)
  val wide_out = vWideMulOut
  val out = Pipe(
    io.pipe(0).valid, 
    Mux(io.pipe(0).bits.wvd_widen2, wide_out, Fill(2, narrow_out)), 
    depth-1
  ).bits
  
  io.write.valid     := io.pipe(depth-1).valid
  io.write.bits.eg   := io.pipe(depth-1).bits.wvd_eg >> 1
  io.write.bits.mask := Mux(io.pipe(depth-1).bits.wvd_widen2, 
    FillInterleaved(8, FillInterleaved(2, io.pipe(depth-1).bits.wmask)),
    Fill(2, FillInterleaved(8, io.pipe(depth-1).bits.wmask))
  )
  io.write.bits.data := out
}
