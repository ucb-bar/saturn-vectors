package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

class SegmentedIntegerMultiplier(val xLenB: Int) extends Module{
    val io = IO(new Bundle {
        val eew = Input(UInt(3.W))
        val in1 = Input(Vec(xLenB, UInt(8.W)))
        val in0 = Input(Vec(xLenB, UInt(8.W)))
        
        val out = Output(Vec(xLenB, UInt(16.W)))

    })
    val vsewMax = if (xLenB==8) 3 else if (xLenB==4) 2 else 0
    if (vsewMax == 0) println("ILLEGAL xLen")
    //////////////////////////////////////////////
    //generate block-diagonal matrix validPProds//
    //////////////////////////////////////////////
    val validPProds = Wire(Vec(vsewMax+1, Vec(xLenB, UInt(xLenB.W))))  
    for (vsew <- 0 until vsewMax+1) {
        val lenBlocks = 1 << vsew
        val numBlocks = xLenB/lenBlocks
        val blockValue = (1 << (lenBlocks)) - 1
        for (i <- 0 until numBlocks) {
            for (j <- 0 until lenBlocks) { //lenBlocks = element witdth (in bytes)
                validPProds(vsew)(i*lenBlocks+j) := (blockValue << (lenBlocks*i)).U
            }
        }
    }
    //compute partial products and set invalid PPs = 0
    val activePProds = Wire(Vec(xLenB, Vec(xLenB, UInt(16.W))))
    for (i <- 0 until xLenB) {
        for (j <- 0 until xLenB) {
            val pProd = (io.in1(i) * io.in0(j))
            activePProds(i)(j) := Mux(validPProds(io.eew)(i).asBools(j), pProd, 0.U)
        }
    }
    //shift and accumulate valid partial products
    val vSumsOut = (0 until xLenB).foldLeft(0.U) { (iPartialSum, i) =>
                        iPartialSum + 
                            (0 until xLenB).foldLeft(0.U) { (jPartialSum, j) =>
                                jPartialSum + (activePProds(i)(j) << (8*(i+j)).U)
                            }
                    }
    //output vector of 8bit elements
    for (i <- 0 until xLenB) {
        io.out(i) := vSumsOut((i+1)*16 - 1, i*16)
    }
}