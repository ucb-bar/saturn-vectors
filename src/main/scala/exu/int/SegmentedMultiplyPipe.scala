package saturn.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import saturn.common._
import saturn.insns._

class SegmentedMultiplyPipe(implicit p: Parameters) extends PipelinedFunctionalUnit(3)(p) {
  val supported_insns = Seq(
    MUL.VV, MUL.VX, MULH.VV, MULH.VX,
    MULHU.VV, MULHU.VX, MULHSU.VV, MULHSU.VX,
    WMUL.VV, WMUL.VX, WMULU.VV, WMULU.VX,
    WMULSU.VV, WMULSU.VX,
    MACC.VV, MACC.VX, NMSAC.VV, NMSAC.VX,
    MADD.VV, MADD.VX, NMSUB.VV, NMSUB.VX,
    WMACC.VV, WMACC.VX, WMACCU.VV, WMACCU.VX,
    WMACCSU.VV , WMACCSU.VX, WMACCUS.VV, WMACCUS.VX,
    SMUL.VV.elementWise, SMUL.VX.elementWise)
  io.iss.ready := new VectorDecoder(io.iss.op.funct3, io.iss.op.funct6, 0.U, 0.U, supported_insns, Nil).matched
  // TODO: SMUL currently operates at 1 element/cycle
  io.set_vxsat := false.B
  io.set_fflags.valid := false.B
  io.set_fflags.bits := DontCare

   val ctrl = new VectorDecoder(io.pipe(0).bits.funct3, io.pipe(0).bits.funct6, 0.U, 0.U, supported_insns, Seq(
     MULSign1, MULSign2, MULSwapVdV2))

  val in_eew = io.pipe(0).bits.rvs1_eew
  val out_eew = io.pipe(0).bits.vd_eew

  val in_vs1 = io.pipe(0).bits.rvs1_data
  val in_vs2 = io.pipe(0).bits.rvs2_data
  val in_vd  = io.pipe(0).bits.rvd_data

  val mul_in1 = in_vs1
  val mul_in2 = Mux(ctrl.bool(MULSwapVdV2), in_vd, in_vs2)

  val multipliers = Seq.fill(dLenB >> 3)(Module(new SegmentedMultiplyBlock))
  for (i <- 0 until (dLenB >> 3)) {
    multipliers(i).io.in1_signed := ctrl.bool(MULSign1)
    multipliers(i).io.in2_signed := ctrl.bool(MULSign2)
    multipliers(i).io.valid      := io.pipe(0).valid
    multipliers(i).io.eew        := io.pipe(0).bits.rvs1_eew
    multipliers(i).io.in1        := mul_in1.asTypeOf(Vec(dLenB >> 3, UInt(64.W)))(i)
    multipliers(i).io.in2        := mul_in2.asTypeOf(Vec(dLenB >> 3, UInt(64.W)))(i)
  }
  val mul_out = VecInit(multipliers.map(_.io.out_data)).asUInt
  //////////////////////////////////////////////
  // 2 Pipeline Stages in multipliers
  //////////////////////////////////////////////
  val in_eew_pipe = io.pipe(2).bits.rvs1_eew
  val out_eew_pipe = io.pipe(2).bits.vd_eew
  val ctrl_wmul = out_eew_pipe > in_eew_pipe
  val ctrl_smul = io.pipe(2).bits.isOpi
  val ctrl_pipe = new VectorDecoder(io.pipe(2).bits.funct3, io.pipe(2).bits.funct6, 0.U, 0.U, supported_insns, Seq(
    MULHi, MULSwapVdV2, MULAccumulate, MULSub))
  val in_vs2_pipe = io.pipe(2).bits.rvs2_data
  val in_vd_pipe  = io.pipe(2).bits.rvd_data

  val hi = VecInit.tabulate(4)({sew =>
    VecInit(mul_out.asTypeOf(Vec((2*dLenB) >> sew, UInt((8 << sew).W))).grouped(2).map(_.last).toSeq).asUInt
  })(in_eew_pipe)
  val lo = VecInit.tabulate(4)({sew =>
    VecInit(mul_out.asTypeOf(Vec((2*dLenB) >> sew, UInt((8 << sew).W))).grouped(2).map(_.head).toSeq).asUInt
  })(in_eew_pipe)
  val half_sel = (io.pipe(2).bits.eidx >> (dLenOffBits.U - out_eew_pipe))(0)
  val wide = Mux(half_sel, mul_out >> dLen, mul_out)(dLen-1,0)

  // TODO, handle SMUL > 1 elem/cycle
  val smul_prod = VecInit.tabulate(4)({sew =>
    if (sew == 3 && dLenB == 8) { mul_out.asSInt } else {
      mul_out.asTypeOf(Vec(dLenB >> sew, SInt((16 << sew).W)))(io.pipe(2).bits.eidx(log2Ceil(dLenB)-1-sew,0))
    }
  })(out_eew_pipe).asSInt

  val rounding_incr = VecInit.tabulate(4)({ sew => RoundingIncrement(io.pipe(2).bits.vxrm, smul_prod((8 << sew)-1,0)) })(out_eew_pipe)
  val smul = VecInit.tabulate(4)({ sew => smul_prod >> ((8 << sew) - 1) })(out_eew_pipe) + Cat(0.U(1.W), rounding_incr).asSInt
  val smul_clip_neg = VecInit.tabulate(4)({ sew => (-1 << ((8 << sew)-1)).S })(out_eew_pipe)
  val smul_clip_pos = VecInit.tabulate(4)({ sew => ((1 << ((8 << sew)-1)) - 1).S })(out_eew_pipe)
  val smul_clip_hi = smul > smul_clip_pos
  val smul_clip_lo = smul < smul_clip_neg
  val smul_clipped = Mux(smul_clip_hi, smul_clip_pos, 0.S) | Mux(smul_clip_lo, smul_clip_neg, 0.S) | Mux(!smul_clip_hi && !smul_clip_lo, smul, 0.S)
  val smul_sat = smul_clip_hi || smul_clip_lo
  val smul_splat = VecInit.tabulate(4)({ sew => Fill(dLenB >> sew, smul_clipped((8<<sew)-1,0)) })(out_eew_pipe)

  val adder_arr = Module(new AdderArray(dLenB))
  adder_arr.io.in1 := Mux(ctrl_wmul, wide, lo).asTypeOf(Vec(dLenB, UInt(8.W)))
  adder_arr.io.in2 := Mux(ctrl_pipe.bool(MULAccumulate), Mux(ctrl_pipe.bool(MULSwapVdV2), in_vs2_pipe, in_vd_pipe), 0.U(dLen.W)).asTypeOf(Vec(dLenB, UInt(8.W)))
  adder_arr.io.incr := VecInit.fill(dLenB)(false.B)
  adder_arr.io.mask_carry := 0.U
  adder_arr.io.signed := DontCare
  adder_arr.io.eew := out_eew_pipe
  adder_arr.io.avg := false.B
  adder_arr.io.rm := DontCare
  adder_arr.io.sub := ctrl_pipe.bool(MULSub)
  adder_arr.io.cmask := false.B

  val add_out = adder_arr.io.out

  val pipe_out = Mux(ctrl_smul, smul_splat, 0.U) | Mux(ctrl_pipe.bool(MULHi), hi, 0.U) | Mux(!ctrl_smul && !ctrl_pipe.bool(MULHi), add_out.asUInt, 0.U)
  // val pipe_out = Pipe(io.pipe(2).valid, out, depth-3).bits
  // val pipe_vxsat = Pipe(io.pipe(2).valid, smul_sat && ctrl_smul, depth-3).bits
  val pipe_vxsat = smul_sat && ctrl_smul
  io.pipe0_stall     := false.B
  io.write.valid     := io.pipe(2).valid
  io.write.bits.eg   := io.pipe(2).bits.wvd_eg
  io.write.bits.data := pipe_out
  io.write.bits.mask := FillInterleaved(8, io.pipe(2).bits.wmask)

  io.set_vxsat := io.pipe(2).valid && pipe_vxsat
  io.scalar_write.valid := false.B
  io.scalar_write.bits := DontCare
}

class SegmentedMultiplyBlock extends Module {
  val xLen = 64
  val xBytes = xLen / 8
  val io = IO(new Bundle {
    val in1_signed = Input(Bool())
    val in2_signed = Input(Bool())
    val valid = Input(Bool())
    val eew = Input(UInt(2.W))

    val in1 = Input(UInt(xLen.W))
    val in2 = Input(UInt(xLen.W))

    val out_data = Output(UInt((2*xLen).W))
  })
  
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
  val genPProds = Module(new genPartialProducts)
  genPProds.io.eew := io.eew
  genPProds.io.valid_pprods := valid_pprods
  genPProds.io.in1 := vins1
  genPProds.io.in2 := vins2
  // val active_pprods = genPProds.io.out_data
  
  //////////////////////////////////////////////
  // Pipeline Stage
  //////////////////////////////////////////////
  val active_pprods_pipe0 = Pipe(io.valid, genPProds.io.out_data)
  val in1_pipe0 = Pipe(io.valid, io.in1)
  val in2_pipe0 = Pipe(io.valid, io.in2)
  val in1_signed_pipe0 = Pipe(io.valid, io.in1_signed)
  val in2_signed_pipe0 = Pipe(io.valid, io.in2_signed)
  val eew_pipe0 = Pipe(io.valid, io.eew)
  //////////////////////////////////////////////

  val accPProds = Module(new accPartialProducts)
  accPProds.io.inPProds := active_pprods_pipe0.bits
  // val sum_pprods_u = accPProds.io.out_data
  
  //////////////////////////////////////////////
  // Pipeline Stage
  //////////////////////////////////////////////
  val sum_pprods_u_pipe1 = Pipe(active_pprods_pipe0.valid, accPProds.io.out_data).bits
  val in1_pipe1 = Pipe(in1_pipe0).bits
  val in2_pipe1 = Pipe(in2_pipe0).bits
  val in1_signed_pipe1 = Pipe(in1_signed_pipe0).bits
  val in2_signed_pipe1 = Pipe(in2_signed_pipe0).bits
  val eew_pipe1 = Pipe(eew_pipe0).bits
  //////////////////////////////////////////////

  //undo input negation if necessary
  val sum_pprods_s = Wire(Vec(4, UInt((2*xLen).W)))
  for (vsew <- 0 until 4) {
    val eew = 8 << vsew
    val nElems = xBytes >> vsew
    val vin1 = in1_pipe1.asTypeOf(Vec(nElems, UInt(eew.W)))
    val vin2 = in2_pipe1.asTypeOf(Vec(nElems, UInt(eew.W)))
    val v_sum_pprods_u = sum_pprods_u_pipe1(2*xLen-1, 0).asTypeOf(Vec(nElems, UInt((2*eew).W)))
    sum_pprods_s(vsew) := (0 until nElems).map (i => {
      val snxsp = in1_signed_pipe1 && in2_signed_pipe1 && (vin1(i)(eew-1).asBool ^ vin2(i)(eew-1).asBool)
      val snxup = ((in1_signed_pipe1 && (~in2_signed_pipe1) && vin1(i)(eew-1).asBool) ||
       ((~in1_signed_pipe1) && in2_signed_pipe1 && vin2(i)(eew-1).asBool))
      Mux(snxsp || snxup, (~v_sum_pprods_u(i)) + 1.U, v_sum_pprods_u(i))
    }).asUInt
  }
  io.out_data := Mux1H(UIntToOH(eew_pipe1), sum_pprods_s)
}

class genPartialProducts extends Module {
  val xLen = 64
  val xBytes = xLen / 8
  val io = IO(new Bundle {
    val eew = Input(UInt(2.W))
    val valid_pprods = Input(Vec(4, Vec(xBytes, UInt(xBytes.W))))

    val in1 = Input(Vec(4, UInt(xLen.W)))
    val in2 = Input(Vec(4, UInt(xLen.W)))

    val out_data = Output(Vec(xBytes, Vec(xBytes, UInt(16.W))))
  })
  //compute partial products and set invalid PPs = 0
  val vinb1 = io.in1(io.eew).asTypeOf(Vec(xBytes, UInt(8.W)))
  val vinb2 = io.in2(io.eew).asTypeOf(Vec(xBytes, UInt(8.W)))
  for (i <- 0 until xBytes) {
    for (j <- 0 until xBytes) {
      val pprod = (vinb1(i) * vinb2(j))
      io.out_data(i)(j) := Mux(io.valid_pprods(io.eew)(i).asBools(j), pprod, 0.U)
    }
  }
}

class accPartialProducts extends Module {
  val xLen = 64
  val xBytes = xLen / 8
  val io = IO(new Bundle {
    val inPProds = Input(Vec(xBytes, Vec(xBytes, UInt(16.W))))
    val out_data = Output(UInt((2*xLen).W))
  })
  //shift and accumulate valid partial products
  io.out_data := (0 until xBytes).foldLeft(0.U) { (i_partial_sum, i) =>
    i_partial_sum +
      (0 until xBytes).foldLeft(0.U) { (j_partial_sum, j) =>
        j_partial_sum + (io.inPProds(i)(j) << (8*(i+j)).U)
      }
  }
}
