package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._


class VectorAdd(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
    val io = IO(new Bundle {
        val rv1_signed = Input(Bool())
        val rv2_signed = Input(Bool())
        val ctrl_sub = Input(Bool())
        val rvs1_eew = Input(UInt(2.W))
        val rvs2_eew = Input(UInt(2.W))

        val in1 = Input(UInt(2*xLen.W))
        val in2 = Input(UInt(2*xLen.W))
        
        val out = Output(UInt((2*xLen).W))
    })

  val vinB1 = io.in1.asTypeOf(Vec(xBytes, UInt(8.W)))
  val vinB2 = io.in2.asTypeOf(Vec(xBytes, UInt(8.W)))
  val add_use_carry = Mux1H(UIntToOH(io.rvs2_eew),
    (0 until 4).map { vsew => Fill(xBytes >> vsew, ~(1.U((1 << vsew).W))) }
  )
  val add_carry = Wire(Vec(xBytes+1, UInt(1.W)))
  val add_out = Wire(Vec(xBytes, UInt(8.W)))
  val add_wide_out_eew = (0 until 3).map { vsew => Wire(Vec(xBytes >> vsew, UInt((16 << vsew).W))) }
  val add_wide_out = Mux1H(UIntToOH(io.rvs1_eew), add_wide_out_eew.map(_.asUInt))

  add_carry(0) := io.ctrl_sub.asUInt

  for (i <- 0 until xBytes) {
    val full = (Mux(io.ctrl_sub, ~vinB1(i), vinB1(i)) +&
      vinB2(i) +&
      Mux(add_use_carry(i), add_carry(i), io.ctrl_sub.asUInt)
    )
    add_out(i) := full(7,0)
    add_carry(i+1) := full(8)
  }
  for (vsew <- 0 until 3) {
    val in_vec = vinB1.asTypeOf(Vec(xBytes >> vsew, UInt((8 << vsew).W)))
    val out_vec = add_out.asTypeOf(Vec(xBytes >> vsew, UInt((8 << vsew).W)))
    for (i <- 0 until xBytes >> vsew) {
      val carry = add_carry((i+1) << vsew)

      val hi1 = (io.rv1_signed && vinB1(((i + 1) << vsew) - 1)(7)) ^ io.ctrl_sub
      val hi2 = io.rv2_signed && vinB2(((i + 1) << vsew) - 1)(7)
      val hi = Mux(hi1 && hi2, ~(1.U((8 << vsew).W)), Fill(8 << vsew, hi1 ^ hi2))
      add_wide_out_eew(vsew)(i) := Cat(hi + carry, out_vec(i))
    }
  }
    io.out := Mux1H(UIntToOH(io.eew), add_wide_out_eew.map(_.asUInt))
}

class SegmentedIntegerMultiplier(depth: Int)(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
    val io = IO(new Bundle {
        val rv1_signed = Input(Bool())
        val rv2_signed = Input(Bool())
        val ctrl_acc = Input(Bool())
        val ctrl_madd = Input(Bool())
        val ctrl_sub = Input(Bool())
        val eew = Input(UInt(2.W))
        
        val in1 = Input(UInt(xLen.W))
        val in2 = Input(UInt(xLen.W))
        val ind = Input(UInt((2*xLen).W))
        
        val out = Output(UInt((2*xLen).W))
    })
    //////////////////////////////////////////////
    //generate block-diagonal matrix validPProds//
    //////////////////////////////////////////////
    val validPProds = Wire(Vec(4, Vec(xBytes, UInt(xBytes.W))))  
    for (vsew <- 0 until 4) {
        val lenBlocks = 1 << vsew
        val numBlocks = xBytes/lenBlocks
        val blockValue = (1 << (lenBlocks)) - 1
        for (i <- 0 until numBlocks) {
            for (j <- 0 until lenBlocks) { //lenBlocks = element witdth (in bytes)
                validPProds(vsew)(i*lenBlocks+j) := (blockValue << (lenBlocks*i)).U
            }
        }
    }
    //negate negative elements
    val vinS1 = Wire(Vec(4, UInt(xLen.W)))
    val vinS2 = Wire(Vec(4, UInt(xLen.W)))
    for (vsew <- 0 until 4) {
        val eew = 8 << vsew
        val nElems = xBytes >> vsew
        val vin1 = io.in1.asTypeOf(Vec(nElems, UInt(eew.W)))
        val vin2 = io.in2.asTypeOf(Vec(nElems, UInt(eew.W)))
        vinS1(vsew) := (0 until nElems).map {i => 
            Mux(io.rv1_signed && vin1(i)(eew-1).asBool, (~vin1(i)) + 1.U, vin1(i))
        }.asUInt
        vinS2(vsew) := (0 until nElems).map {i => 
            Mux(io.rv2_signed && vin2(i)(eew-1).asBool, (~vin2(i)) + 1.U, vin2(i))
        }.asUInt
    }
    //compute partial products and set invalid PPs = 0
    val activePProds = Wire(Vec(xBytes, Vec(xBytes, UInt(16.W))))
    val vinB1 = vinS1(io.eew).asTypeOf(Vec(xBytes, UInt(8.W)))
    val vinB2 = vinS2(io.eew).asTypeOf(Vec(xBytes, UInt(8.W)))
    for (i <- 0 until xBytes) {
        for (j <- 0 until xBytes) {
            val pProd = (vinB1(i) * vinB2(j))
            activePProds(i)(j) := Mux(validPProds(io.eew)(i).asBools(j), pProd, 0.U)
        }
    }
    //shift and accumulate valid partial products
    val SumPProdsU = (0 until xBytes).foldLeft(0.U) { (iPartialSum, i) =>
                        iPartialSum + 
                            (0 until xBytes).foldLeft(0.U) { (jPartialSum, j) =>
                                jPartialSum + (activePProds(i)(j) << (8*(i+j)).U)
                            }
                    }
    //undo input negation if necessary
    val SumPProdsS = Wire(Vec(4, UInt((2*xLen).W)))
    for (vsew <- 0 until 4) {
        val eew = 8 << vsew
        val nElems = xBytes >> vsew
        val vin1 = io.in1.asTypeOf(Vec(nElems, UInt(eew.W)))
        val vin2 = io.in2.asTypeOf(Vec(nElems, UInt(eew.W)))
        val vSumPProdsU = SumPProdsU(2*xLen-1, 0).asTypeOf(Vec(nElems, UInt((2*eew).W)))
        // val vSumPProdsS = Wire(Vec(nElems, UInt((2*eew).W)))
        SumPProdsS(vsew) := (0 until nElems).map (i => {
            val snXsp = io.rv1_signed && io.rv2_signed && 
                (vin1(i)(eew-1).asBool ^ vin2(i)(eew-1).asBool)
            val snXup = (io.rv1_signed && (~io.rv2_signed) && vin1(i)(eew-1).asBool) || 
                ((~io.rv1_signed) && io.rv2_signed && vin2(i)(eew-1).asBool)
            Mux(snXsp || snXup, (~vSumPProdsU(i)) + 1.U, vSumPProdsU(i))
        }).asUInt
    }
    //TODO: Implement FMA by adding in3 to output
    val mulOut = Mux1H(UIntToOH(io.eew), SumPProdsS)
    
    val vAdd = Module(new VectorAdd)
    vAdd.io.rv1_signed := io.rv1_signed
    vAdd.io.rv2_signed := io.rv2_signed
    vAdd.io.ctrl_sub := io.ctrl_sub
    vAdd.io.rv1_eew := Mux(ctrl_wide_in, 2*io.rv1_eew, io.rv1_eew)
    vAdd.io.rv2_eew := Mux(ctrl_wide_in, 2*io.rv2_eew, io.rv2_eew)
    vAdd.io.in1 := mulOut
    vAdd.io.in2 := Mux(io.ctrl_madd, mulOut, io.in2)
    io.out := Mux(io.ctrl_acc, vAdd.io.out, mulOut)
}