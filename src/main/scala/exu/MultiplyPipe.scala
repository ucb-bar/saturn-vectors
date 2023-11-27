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

  val aluFn = new ALUFN
  lazy val ctrl_table = Seq(
    (OPMFunct6.mul   , Seq(N,X,X)),
    (OPMFunct6.mulh  , Seq(Y,Y,Y)),
    (OPMFunct6.mulhu , Seq(Y,N,N)),
    (OPMFunct6.mulhsu, Seq(Y,N,Y))
  )

  override def accepts(f3: UInt, f6: UInt): Bool = VecDecode(f3, f6, ctrl_table.map(_._1))

  val ctrl_hi :: ctrl_sign1 :: ctrl_sign2 :: Nil = VecDecode.applyBools(
    io.pipe(0).bits.funct3, io.pipe(0).bits.funct6,
    Seq.fill(3)(X), ctrl_table)

  val in_eew = io.pipe(0).bits.rvs1_eew
  val eidx = io.pipe(0).bits.eidx

  val in1 = extract(io.pipe(0).bits.rvs1_data, ctrl_sign1, in_eew, eidx)(64,0).asSInt
  val in2 = extract(io.pipe(0).bits.rvs2_data, ctrl_sign2, in_eew, eidx)(64,0).asSInt

  val prod = in1 * in2
  val hi = VecInit.tabulate(4)({ eew => prod >> (8 << eew) })(in_eew)
  val out = Pipe(io.pipe(0).valid, Mux(ctrl_hi, hi, prod)(63,0), depth-1).bits

  val wdata = VecInit.tabulate(4)({ eew => Fill(dLenB >> eew, out((8<<eew)-1,0)) })(io.pipe(depth-1).bits.rvs1_eew)
  io.write.valid     := io.pipe(depth-1).valid
  io.write.bits.eg   := io.pipe(depth-1).bits.wvd_eg
  io.write.bits.data := wdata
  io.write.bits.mask := FillInterleaved(8, io.pipe(depth-1).bits.wmask)
}
