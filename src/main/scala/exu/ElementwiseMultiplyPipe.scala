package vector.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import vector.common._

class ElementwiseMultiplyPipe(depth: Int)(implicit p: Parameters) extends PipelinedFunctionalUnit(depth)(p) {

  io.iss.sub_dlen := log2Ceil(dLenB).U - io.iss.op.vd_eew
  io.set_vxsat := false.B
  io.set_fflags.valid := false.B
  io.set_fflags.bits := DontCare

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
    (OPIFunct6.smul   , Seq(X,Y,Y,N,N,N))
  )

  override def accepts(f3: UInt, f6: UInt): Bool = VecDecode(f3, f6, ctrl_table.map(_._1))

  val ctrl_hi :: ctrl_sign1 :: ctrl_sign2 :: ctrl_swapvdvs2 :: ctrl_madd :: ctrl_sub :: Nil = VecDecode.applyBools(
    io.pipe(0).bits.funct3, io.pipe(0).bits.funct6,
    Seq.fill(6)(X), ctrl_table)

  val ctrl_smul = io.pipe(0).bits.isOpi

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
  val rounding_incr = VecInit.tabulate(4)({ eew => RoundingIncrement(io.pipe(0).bits.vxrm, prod((8 << eew)-1,0)) })(out_eew)
  val smul = VecInit.tabulate(4)({ eew => prod >> ((8 << eew) - 1) })(out_eew) + Cat(0.U(1.W), rounding_incr).asSInt
  val smul_clip_neg = VecInit.tabulate(4)({ eew => (-1 << ((8 << eew)-1)).S })(out_eew)
  val smul_clip_pos = VecInit.tabulate(4)({ eew => ((1 << ((8 << eew)-1)) - 1).S })(out_eew)
  val smul_clip_hi = smul > smul_clip_pos
  val smul_clip_lo = smul < smul_clip_neg
  val smul_clipped = Mux(smul_clip_hi, smul_clip_pos, 0.S) | Mux(smul_clip_lo, smul_clip_neg, 0.S) | Mux(!smul_clip_hi && !smul_clip_lo, smul, 0.S)
  val smul_sat = smul_clip_hi || smul_clip_lo
  val out = Mux(ctrl_madd, madd, 0.U) | Mux(ctrl_smul, smul_clipped.asUInt, 0.U) | Mux(!ctrl_madd && !ctrl_smul, Mux(ctrl_hi, hi, lo), 0.U)

  val pipe_out = Pipe(io.pipe(0).valid, out(63,0), depth-1).bits
  val pipe_vxsat = Pipe(io.pipe(0).valid, smul_sat && ctrl_smul, depth-1).bits

  val wdata = VecInit.tabulate(4)({ eew => Fill(dLenB >> eew, pipe_out((8<<eew)-1,0)) })(io.pipe(depth-1).bits.vd_eew)
  io.write.valid     := io.pipe(depth-1).valid
  io.write.bits.eg   := io.pipe(depth-1).bits.wvd_eg
  io.write.bits.data := wdata
  io.write.bits.mask := FillInterleaved(8, io.pipe(depth-1).bits.wmask)

  io.set_vxsat := io.pipe(depth-1).valid && pipe_vxsat
}
