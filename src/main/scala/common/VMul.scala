package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

class SegmentedIntegerMultiplier(depth: Int)(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
    val io = IO(new Bundle {
        val rvs1_signed = Input(Bool())
        val rvs2_signed = Input(Bool())
        val ctrl_acc = Input(Bool())
        val ctrl_madd = Input(Bool())
        val ctrl_sub = Input(Bool())
        val ctrl_wide_in = Input(Bool())
        val eew = Input(UInt(2.W))
        
        val rvs1_data = Input(UInt(xLen.W))
        val rvs2_data = Input(UInt(xLen.W))
        val rvd_data = Input(UInt((2*xLen).W))
        
        val out_data = Output(UInt((2*xLen).W))
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
        val vin1 = io.rvs1_data.asTypeOf(Vec(nElems, UInt(eew.W)))
        val vin2 = io.rvs2_data.asTypeOf(Vec(nElems, UInt(eew.W)))
        vinS1(vsew) := (0 until nElems).map {i => 
            Mux(io.rvs1_signed && vin1(i)(eew-1).asBool, (~vin1(i)) + 1.U, vin1(i))
        }.asUInt
        vinS2(vsew) := (0 until nElems).map {i => 
            Mux(io.rvs2_signed && vin2(i)(eew-1).asBool, (~vin2(i)) + 1.U, vin2(i))
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
        val vin1 = io.rvs1_data.asTypeOf(Vec(nElems, UInt(eew.W)))
        val vin2 = io.rvs2_data.asTypeOf(Vec(nElems, UInt(eew.W)))
        val vSumPProdsU = SumPProdsU(2*xLen-1, 0).asTypeOf(Vec(nElems, UInt((2*eew).W)))
        // val vSumPProdsS = Wire(Vec(nElems, UInt((2*eew).W)))
        SumPProdsS(vsew) := (0 until nElems).map (i => {
            val snXsp = io.rvs1_signed && io.rvs2_signed && 
                (vin1(i)(eew-1).asBool ^ vin2(i)(eew-1).asBool)
            val snXup = (io.rvs1_signed && (~io.rvs2_signed) && vin1(i)(eew-1).asBool) || 
                ((~io.rvs1_signed) && io.rvs2_signed && vin2(i)(eew-1).asBool)
            Mux(snXsp || snXup, (~vSumPProdsU(i)) + 1.U, vSumPProdsU(i))
        }).asUInt
    }
    //TODO: Implement FMA by adding in3 to output
    val mulOut = Mux1H(UIntToOH(io.eew), SumPProdsS)
    
    val vAdd = Module(new VectorAdd)
    vAdd.io.rvs_signed := io.rvs1_signed || io.rvs2_signed
    vAdd.io.ctrl_sub := io.ctrl_sub
    vAdd.io.eew := Mux(io.ctrl_wide_in, io.eew << 1, io.eew)
    vAdd.io.rvs1_data := mulOut
    vAdd.io.rvs2_data := Mux(io.ctrl_madd, io.rvs2_data, io.rvd_data)
    io.out_data := Mux(io.ctrl_acc, vAdd.io.out_data, mulOut)
}

class VectorAdd(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
    val io = IO(new Bundle {
        val rvs_signed = Input(Bool())
        val ctrl_sub = Input(Bool())
        // val ctrl_wide_in = Input(Bool())
        val eew = Input(UInt(3.W))

        val rvs1_data = Input(UInt((2*xLen).W))
        val rvs2_data = Input(UInt((2*xLen).W))
        
        val out_data = Output(UInt((2*xLen).W))
    })

  val in1 = io.rvs1_data.asTypeOf(Vec(xBytes, UInt(8.W)))
  val in2 = io.rvs2_data.asTypeOf(Vec(xBytes, UInt(8.W)))
  val add_use_carry = Mux1H(UIntToOH(io.eew),
    (0 until 4).map { eew => Fill(xBytes >> eew, ~(1.U((1 << eew).W))) }
  )
  val add_carry = Wire(Vec(xBytes+1, UInt(1.W)))
  val add_out = Wire(Vec(xBytes, UInt(8.W)))
  
  add_carry(0) := io.ctrl_sub

  for (i <- 0 until xBytes) {
    val full = (Mux(io.ctrl_sub, ~in1(i), in1(i)) +&
      in2(i) +&
      Mux(add_use_carry(i), add_carry(i), io.ctrl_sub)
    )
    add_out(i) := full(7,0)
    add_carry(i+1) := full(8)
  }
  io.out_data := add_out.asUInt
}