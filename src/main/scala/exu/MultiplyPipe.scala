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

  def extract(in: UInt, sext: Bool): SInt = {
    val bytes = in.asTypeOf(Vec(dLenB, UInt(8.W)))
    VecInit.tabulate(4) { eew =>
      val elem = if (dLen == 64 && eew == 3) {
        in
      } else {
        VecInit(bytes.grouped(1 << eew).map(g => VecInit(g).asUInt).toSeq)(eidx(log2Ceil(dLenB)-1-eew,0))
      }
      val hi = sext && elem((8 << eew)-1)
      Cat(hi, elem((8 << eew)-1,0)).asSInt
    }(in_eew)
  }
  val in1 = extract(io.pipe(0).bits.rvs1_data, ctrl_sign1)(64,0).asSInt
  val in2 = extract(io.pipe(0).bits.rvs2_data, ctrl_sign2)(64,0).asSInt

  val prod = in1 * in2
  val hi = VecInit.tabulate(4)({ eew => prod >> (8 << eew) })(in_eew)
  val out = Pipe(io.pipe(0).valid, Mux(ctrl_hi, hi, prod)(63,0), depth-1).bits

  val wdata = VecInit.tabulate(4)({ eew => Fill(dLenB >> eew, out((8<<eew)-1,0)) })(io.pipe(depth-1).bits.rvs1_eew)
  io.write.valid     := io.pipe(depth-1).valid
  io.write.bits.eg   := io.pipe(depth-1).bits.wvd_eg
  io.write.bits.data := wdata
  io.write.bits.mask := FillInterleaved(8, io.pipe(depth-1).bits.wmask)
}

class SegmentedMultiplyPipe(depth: Int)(implicit p: Parameters) extends PipelinedFunctionalUnit(depth, false)(p) {

  io.iss.sub_dlen := log2Ceil(dLenB).U - io.iss.op.rvs1_eew

  lazy val ctrl_table = Seq(  
      (OPMFunct6.mulhu,   Seq(N,N,N,N,N,X,Y)),
      (OPMFunct6.mul,     Seq(N,Y,Y,N,N,X,N)),
      (OPMFunct6.mulhsu,  Seq(N,N,Y,N,N,X,Y)),
      (OPMFunct6.mulh,    Seq(N,Y,Y,N,N,X,Y)),
      (OPMFunct6.madd,    Seq(N,Y,Y,Y,Y,N,N)),
      (OPMFunct6.nmsub,   Seq(N,Y,Y,Y,Y,Y,N)),
      (OPMFunct6.macc,    Seq(N,Y,Y,N,Y,N,N)),
      (OPMFunct6.nmsac,   Seq(N,Y,Y,N,Y,Y,N)),

      (OPMFunct6.wmulu,   Seq(Y,N,N,N,N,X,X)),
      (OPMFunct6.wmulsu,  Seq(Y,N,Y,N,N,X,X)),
      (OPMFunct6.wmul,    Seq(Y,Y,Y,N,N,X,X)),
      (OPMFunct6.wmaccu,  Seq(Y,N,N,N,Y,N,X)),
      (OPMFunct6.wmacc,   Seq(Y,Y,Y,N,Y,N,X)),
      (OPMFunct6.wmaccus, Seq(Y,N,Y,N,Y,N,X)),
      (OPMFunct6.wmaccsu, Seq(Y,Y,N,N,Y,N,X))
  )
  override def accepts(f3: UInt, f6: UInt): Bool = VecDecode(f3, f6, ctrl_table.map(_._1))
  val  ctrl_wide_in :: ctrl_rvs1_signed :: ctrl_rvs2_signed :: ctrl_madd :: ctrl_acc :: ctrl_sub :: ctrl_hi :: Nil = VecDecode.applyBools(
    io.pipe(0).bits.funct3, io.pipe(0).bits.funct6,
    Seq.fill(7)(BitPat.dontCare(1)), ctrl_table)

  val wvd_eg = io.pipe(0).bits.wvd_eg(0)
  val eew = io.pipe(0).bits.rvs1_eew // = to io.pipe(0).bits.rvs2_eew
  val in1 = io.pipe(0).bits.rvs1_data
  val in2 = Mux(ctrl_madd, io.pipe(0).bits.rvd_data, io.pipe(0).bits.rvs2_data)
  val ind = Mux(ctrl_madd, io.pipe(0).bits.rvs2_data, io.pipe(0).bits.rvd_data)

  val numSegMul = dLen/xLen
  val viMul = Seq.fill(numSegMul) {Module(new SegmentedIntegerMultiplier(1))}
  for (i <- 0 until numSegMul) {
    viMul(i).io.in1_signed := ctrl_rvs1_signed
    viMul(i).io.in2_signed := ctrl_rvs2_signed
    viMul(i).io.ctrl_acc := ctrl_acc
    viMul(i).io.ctrl_madd := ctrl_madd
    viMul(i).io.ctrl_sub := ctrl_sub
    viMul(i).io.ctrl_wide_in := ctrl_wide_in
    viMul(i).io.eew := eew
    viMul(i).io.in1 := in1((i+1)*xLen-1, i*xLen)
    viMul(i).io.in2 := in2((i+1)*xLen-1, i*xLen)
  }
  val viMulOut = VecInit((0 until numSegMul).map { i => viMul(i).io.out_data }).asUInt
  
  val viAcc = Module(new SegmentedAdd)
  viAcc.io.ctrl_sub := ctrl_sub
  viAcc.io.eew := Mux(ctrl_wide_in, eew + 1.U, eew)
  viAcc.io.in1 := Mux(ctrl_wide_in, viMulOut >> dLen, viMulOut)
  viAcc.io.in2 := ind
    // io.out_data := Mux(io.ctrl_acc, viAcc(i).io.out_data, mulOut)
  val wide_out = Mux(ctrl_acc, viAcc.io.out, viMulOut)
  io.write.valid     := io.pipe(0).valid
  io.write.bits.eg   := io.pipe(0).bits.wvd_eg >> 1
  io.write.bits.mask := Fill(2, FillInterleaved(8, io.pipe(0).bits.wmask))
  io.write.bits.data := Fill(2, wide_out)

  when (io.pipe(0).bits.wvd_widen2 && ~ctrl_acc) {
    io.write.bits.data := wide_out
  }
}
