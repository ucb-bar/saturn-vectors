package vector.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import vector.common._

class SegmentedMultiplyPipe(depth: Int)(implicit p: Parameters) extends PipelinedFunctionalUnit(depth)(p) {
  // TODO: SMUL currently operates at 1 element/cycle
  io.iss.sub_dlen := Mux(io.iss.op.opif6 === OPIFunct6.smul, log2Ceil(dLenB).U - io.iss.op.vd_eew, 0.U)
  io.set_vxsat := false.B
  io.set_fflags.valid := false.B
  io.set_fflags.bits := DontCare

  lazy val ctrl_table = Seq(
    (OPMFunct6.mul    , Seq(N,X,X,N,N,X)),
    (OPMFunct6.mulh   , Seq(Y,Y,Y,N,N,X)),
    (OPMFunct6.mulhu  , Seq(Y,N,N,N,N,X)),
    (OPMFunct6.mulhsu , Seq(Y,N,Y,N,N,X)),
    (OPMFunct6.wmul   , Seq(N,Y,Y,N,N,X)),
    (OPMFunct6.wmulu  , Seq(N,N,N,N,N,X)),
    (OPMFunct6.wmulsu , Seq(N,N,Y,N,N,X)),
    (OPMFunct6.macc   , Seq(N,X,X,N,Y,N)),
    (OPMFunct6.nmsac  , Seq(N,X,X,N,Y,Y)),
    (OPMFunct6.madd   , Seq(N,X,X,Y,Y,N)),
    (OPMFunct6.nmsub  , Seq(N,X,X,Y,Y,Y)),
    (OPMFunct6.wmaccu , Seq(N,N,N,N,Y,N)),
    (OPMFunct6.wmacc  , Seq(N,Y,Y,N,Y,N)),
    (OPMFunct6.wmaccsu, Seq(N,Y,N,N,Y,N)),
    (OPMFunct6.wmaccus, Seq(N,N,Y,N,Y,N)),
    (OPIFunct6.smul   , Seq(N,Y,Y,N,N,N))
  )

  def accepts(f3: UInt, f6: UInt): Bool = VecDecode(f3, f6, ctrl_table.map(_._1))

  val ctrl_hi :: ctrl_sign1 :: ctrl_sign2 :: ctrl_swapvdvs2 :: ctrl_madd :: ctrl_sub :: Nil = VecDecode.applyBools(
    io.pipe(0).bits.funct3, io.pipe(0).bits.funct6,
    Seq.fill(6)(X), ctrl_table)
  io.iss.ready := accepts(io.iss.op.funct3, io.iss.op.funct6)

  val in_eew = io.pipe(0).bits.rvs1_eew
  val out_eew = io.pipe(0).bits.vd_eew

  val ctrl_smul = io.pipe(0).bits.isOpi
  val ctrl_wmul = out_eew > in_eew

  val in_vs1 = io.pipe(0).bits.rvs1_data
  val in_vs2 = io.pipe(0).bits.rvs2_data
  val in_vd  = io.pipe(0).bits.rvd_data

  val mul_in1 = in_vs1
  val mul_in2 = Mux(ctrl_swapvdvs2, in_vd, in_vs2)

  val multipliers = Seq.fill(dLenB >> 3)(Module(new SegmentedMultiplyBlock))
  for (i <- 0 until (dLenB >> 3)) {
    multipliers(i).io.in1_signed := ctrl_sign1
    multipliers(i).io.in2_signed := ctrl_sign2
    multipliers(i).io.eew        := io.pipe(0).bits.rvs1_eew
    multipliers(i).io.in1        := mul_in1.asTypeOf(Vec(dLenB >> 3, UInt(64.W)))(i)
    multipliers(i).io.in2        := mul_in2.asTypeOf(Vec(dLenB >> 3, UInt(64.W)))(i)
  }
  val mul_out = VecInit(multipliers.map(_.io.out_data)).asUInt

  val hi = VecInit.tabulate(4)({sew =>
    VecInit(mul_out.asTypeOf(Vec((2*dLenB) >> sew, UInt((8 << sew).W))).grouped(2).map(_.last).toSeq).asUInt
  })(in_eew)
  val lo = VecInit.tabulate(4)({sew =>
    VecInit(mul_out.asTypeOf(Vec((2*dLenB) >> sew, UInt((8 << sew).W))).grouped(2).map(_.head).toSeq).asUInt
  })(in_eew)
  val half_sel = (io.pipe(0).bits.eidx >> (dLenOffBits.U - out_eew))(0)
  val wide = Mux(half_sel, mul_out >> dLen, mul_out)(dLen-1,0)

  // TODO, handle SMUL > 1 elem/cycle
  val smul_prod = VecInit.tabulate(4)({sew =>
    if (sew == 3 && dLenB == 8) { mul_out } else {
      mul_out.asTypeOf(Vec(dLenB >> sew, SInt((16 << sew).W)))(io.pipe(0).bits.eidx(log2Ceil(dLenB)-1-sew,0))
    }
  })(out_eew).asSInt

  val rounding_incr = VecInit.tabulate(4)({ sew => RoundingIncrement(io.pipe(0).bits.vxrm, smul_prod((8 << sew)-1,0)) })(out_eew)
  val smul = VecInit.tabulate(4)({ sew => smul_prod >> ((8 << sew) - 1) })(out_eew) + Cat(0.U(1.W), rounding_incr).asSInt
  val smul_clip_neg = VecInit.tabulate(4)({ sew => (-1 << ((8 << sew)-1)).S })(out_eew)
  val smul_clip_pos = VecInit.tabulate(4)({ sew => ((1 << ((8 << sew)-1)) - 1).S })(out_eew)
  val smul_clip_hi = smul > smul_clip_pos
  val smul_clip_lo = smul < smul_clip_neg
  val smul_clipped = Mux(smul_clip_hi, smul_clip_pos, 0.S) | Mux(smul_clip_lo, smul_clip_neg, 0.S) | Mux(!smul_clip_hi && !smul_clip_lo, smul, 0.S)
  val smul_sat = smul_clip_hi || smul_clip_lo
  val smul_splat = VecInit.tabulate(4)({ sew => Fill(dLenB >> sew, smul_clipped((8<<sew)-1,0)) })(out_eew)

  val adder_arr = Module(new AdderArray(dLenB))
  adder_arr.io.in1 := Mux(ctrl_wmul, wide, lo).asTypeOf(Vec(dLenB, UInt(8.W)))
  adder_arr.io.in2 := Mux(ctrl_madd, Mux(ctrl_swapvdvs2, in_vs2, in_vd), 0.U(dLen.W)).asTypeOf(Vec(dLenB, UInt(8.W)))
  adder_arr.io.incr := VecInit.fill(dLenB)(false.B)
  adder_arr.io.mask_carry := 0.U
  adder_arr.io.signed := DontCare
  adder_arr.io.eew := out_eew
  adder_arr.io.avg := false.B
  adder_arr.io.rm := DontCare
  adder_arr.io.sub := ctrl_sub
  adder_arr.io.cmask := false.B

  val add_out = adder_arr.io.out

  val out = Mux(ctrl_smul, smul_splat, 0.U) | Mux(ctrl_hi, hi, 0.U) | Mux(!ctrl_smul && !ctrl_hi, add_out.asUInt, 0.U)
  val pipe_out = Pipe(io.pipe(0).valid, out, depth-1).bits
  val pipe_vxsat = Pipe(io.pipe(0).valid, smul_sat && ctrl_smul, depth-1).bits

  io.write.valid     := io.pipe(depth-1).valid
  io.write.bits.eg   := io.pipe(depth-1).bits.wvd_eg
  io.write.bits.data := pipe_out
  io.write.bits.mask := FillInterleaved(8, io.pipe(depth-1).bits.wmask)

  io.set_vxsat := io.pipe(depth-1).valid && pipe_vxsat

}

class SegmentedMultiplyBlock extends Module {
  val xLen = 64
  val xBytes = xLen / 8
  val io = IO(new Bundle {
    val in1_signed = Input(Bool())
    val in2_signed = Input(Bool())
    val eew = Input(UInt(2.W))

    val in1 = Input(UInt(xLen.W))
    val in2 = Input(UInt(xLen.W))

    val out_data = Output(UInt((2*xLen).W))
  })

  //////////////////////////////////////////////
  //generate block-diagonal matrix validPProds//
  //////////////////////////////////////////////
  val valid_pprods = Wire(Vec(4, Vec(xBytes, UInt(xBytes.W))))
  for (vsew <- 0 until 4) {
    val lenBlocks = 1 << vsew
    val numBlocks = xBytes/lenBlocks
    val blockValue = (1 << (lenBlocks)) - 1
    for (i <- 0 until numBlocks) {
      for (j <- 0 until lenBlocks) { //lenBlocks = element width (in bytes)
        valid_pprods(vsew)(i*lenBlocks+j) := (blockValue << (lenBlocks*i)).U
      }
    }
  }

  //negate negative elements
  val vins1 = Wire(Vec(4, UInt(xLen.W)))
  val vins2 = Wire(Vec(4, UInt(xLen.W)))
  for (vsew <- 0 until 4) {
    val eew = 8 << vsew
    val nElems = xBytes >> vsew
    val vin1 = io.in1.asTypeOf(Vec(nElems, UInt(eew.W)))
    val vin2 = io.in2.asTypeOf(Vec(nElems, UInt(eew.W)))
    vins1(vsew) := (0 until nElems).map {i =>
      Mux(io.in1_signed && vin1(i)(eew-1).asBool, (~vin1(i)) + 1.U, vin1(i))
    }.asUInt
    vins2(vsew) := (0 until nElems).map {i =>
      Mux(io.in2_signed && vin2(i)(eew-1).asBool, (~vin2(i)) + 1.U, vin2(i))
    }.asUInt
  }

  //compute partial products and set invalid PPs = 0
  val active_pprods = Wire(Vec(xBytes, Vec(xBytes, UInt(16.W))))
  val vinb1 = vins1(io.eew).asTypeOf(Vec(xBytes, UInt(8.W)))
  val vinb2 = vins2(io.eew).asTypeOf(Vec(xBytes, UInt(8.W)))
  for (i <- 0 until xBytes) {
    for (j <- 0 until xBytes) {
      val pprod = (vinb1(i) * vinb2(j))
      active_pprods(i)(j) := Mux(valid_pprods(io.eew)(i).asBools(j), pprod, 0.U)
    }
  }
  //shift and accumulate valid partial products
  val sum_pprods_u = (0 until xBytes).foldLeft(0.U) { (i_partial_sum, i) =>
    i_partial_sum +
      (0 until xBytes).foldLeft(0.U) { (j_partial_sum, j) =>
        j_partial_sum + (active_pprods(i)(j) << (8*(i+j)).U)
      }
  }
  //undo input negation if necessary
  val sum_pprods_s = Wire(Vec(4, UInt((2*xLen).W)))
  for (vsew <- 0 until 4) {
    val eew = 8 << vsew
    val nElems = xBytes >> vsew
    val vin1 = io.in1.asTypeOf(Vec(nElems, UInt(eew.W)))
    val vin2 = io.in2.asTypeOf(Vec(nElems, UInt(eew.W)))
    val v_sum_pprods_u = sum_pprods_u(2*xLen-1, 0).asTypeOf(Vec(nElems, UInt((2*eew).W)))
    sum_pprods_s(vsew) := (0 until nElems).map (i => {
      val snxsp = io.in1_signed && io.in2_signed &&
      (vin1(i)(eew-1).asBool ^ vin2(i)(eew-1).asBool)
      val snxup = ((io.in1_signed && (~io.in2_signed) && vin1(i)(eew-1).asBool) ||
       ((~io.in1_signed) && io.in2_signed && vin2(i)(eew-1).asBool))
      Mux(snxsp || snxup, (~v_sum_pprods_u(i)) + 1.U, v_sum_pprods_u(i))
    }).asUInt
  }
  io.out_data := Mux1H(UIntToOH(io.eew), sum_pprods_s)
}
