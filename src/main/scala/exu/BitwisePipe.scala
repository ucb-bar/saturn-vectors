package vector.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import vector.common._

class BitwisePipe(implicit p: Parameters) extends PipelinedFunctionalUnit(1)(p) {
  io.iss.sub_dlen := 0.U

  lazy val ctrl_table = Seq(
    (OPIFunct6.and      , Seq(Y,N,N,N,N)),
    (OPIFunct6.or       , Seq(N,Y,N,N,N)),
    (OPIFunct6.xor      , Seq(N,N,Y,N,N)),
    (OPMFunct6.mandnot  , Seq(Y,N,N,N,Y)),
    (OPMFunct6.mand     , Seq(Y,N,N,N,N)),
    (OPMFunct6.mor      , Seq(N,Y,N,N,N)),
    (OPMFunct6.mxor     , Seq(N,N,Y,N,N)),
    (OPMFunct6.mornot   , Seq(N,Y,N,N,Y)),
    (OPMFunct6.mnand    , Seq(Y,N,N,Y,N)),
    (OPMFunct6.mnor     , Seq(N,Y,N,Y,N)),
    (OPMFunct6.mxnor    , Seq(N,N,Y,Y,N)),
    (OPMFunct6.redand   , Seq(Y,N,N,N,N)),
    (OPMFunct6.redor    , Seq(N,Y,N,N,N)),
    (OPMFunct6.redxor   , Seq(N,N,Y,N,N)),
  )
  def accepts(f3: UInt, f6: UInt): Bool = VecDecode(f3, f6, ctrl_table.map(_._1))
  io.iss.ready := accepts(io.iss.op.funct3, io.iss.op.funct6)
  val ctrl_and :: ctrl_or :: ctrl_xor :: ctrl_invout :: ctrl_inv1 :: Nil = VecDecode.applyBools(
    io.pipe(0).bits.funct3, io.pipe(0).bits.funct6, Seq.fill(5)(X), ctrl_table)

  val in1 = Mux(ctrl_inv1, ~io.pipe(0).bits.rvs1_data, io.pipe(0).bits.rvs1_data)
  val in2 = io.pipe(0).bits.rvs2_data
  val op = Mux1H(Seq(
    (ctrl_and, (in1 & in2)),
    (ctrl_or , (in1 | in2)),
    (ctrl_xor, (in1 ^ in2))
  ))
  val out = Mux(ctrl_invout, ~op, op)

  io.write.valid := io.pipe(0).valid
  io.write.bits.eg := io.pipe(0).bits.wvd_eg
  io.write.bits.mask := Mux(io.pipe(0).bits.isOpm && !io.pipe(0).bits.acc,
    ~(0.U(dLen.W)) >> Mux(io.pipe(0).bits.tail, (0.U(log2Ceil(dLen).W) - io.pipe(0).bits.vl(log2Ceil(dLen)-1,0)), 0.U),
    FillInterleaved(8, io.pipe(0).bits.wmask))
  io.write.bits.data := out

  io.set_vxsat := false.B
  io.set_fflags.valid := false.B
  io.set_fflags.bits := DontCare
}
