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
        val in1 = Input(Vec(xLenB, UInt(xLenB.W)))
        val in2 = Input(Vec(xLenB, UInt(xLenB.W)))
        
        val out = Input(Vec(xLenB, UInt((xLenB*2).W)))

    })
    ///////////////////////////////////////////
    //generate block-diagonal matrix use_pprod
    ///////////////////////////////////////////
    val use_pprod = Wire(Vec(4, Vec(xLenB, UInt(xLenB.W))))  
    for (vsew <- 0 until 4) {
        val lenBlocks = 1 << vsew
        val numBlocks = xLenB/lenBlocks
        val blockValue = (1 << (lenBlocks)) - 1
        println("xlenB", xLenB)
        // println("vsew", vsew)
        // println("lenBlocks",lenBlocks) 
        // println("nblocks", numBlocks)
        // println("blockVal",blockValue)
        for (i <- 0 until numBlocks) {
            for (j <- 0 until lenBlocks) { //ewB
                use_pprod(vsew)(i*lenBlocks+j) := (blockValue << (lenBlocks*i)).U
            }
        }
    }
    //generate active partial products
    val active_pprods = Wire(Vec(xLenB, Vec(xLenB, UInt((xLenB*2).W))))
    val nDiagonals = xLenB*2-1
    for (i <- 0 until xLenB) {
        for (j <- 0 until xLenB) {
            val pprod = (io.in1(i) * io.in2(j))
            active_pprods(i)(j) := Mux(use_pprod(io.eew)(i).asBools(j), pprod, 0.U)
        }
    }
    //sum partial products along each anti-diagonal of active_pprods
    // val reduced_pprods = (0 until (nDiagonals)).map { k =>
    //     val numPProds = k.min(nDiagonals-k)
    //     println("outer:", k, numPProds)
    //     (0 to numPProds).foldLeft(0.U((xLenB*2 + numPProds/2).W)) { (partialSum, i) =>
    //         println(numPProds-i, i)
    //         partialSum + active_pprods(i)(numPProds - i)
    //     }
    // }
    // println("reduced",reduced_pprods)
    val reduced_pprods = Wire(Vec(nDiagonals, UInt((xLenB*2 + xLenB/2).W)))
    for (k <- 0 until xLenB) {
        val numPProds = k
        // println("outer:", k, numPProds)
        val sum = (0 to numPProds).foldLeft(0.U((xLenB*2 + numPProds/2).W)) { (partialSum, i) =>
            // println(i, numPProds-i)
            partialSum + active_pprods(i)(numPProds - i)
        }
        reduced_pprods(k) := sum
    }
    for (k <- 1 until xLenB) {
        val numPProds = xLenB - k
        // println("outer2:", k, numPProds)
        val sum = (0 to numPProds-1).foldLeft(0.U((xLenB*2 + numPProds/2).W)) { (partialSum, i) =>
            val xind = i + k
            val yind = xLenB-1 - i
            // println(xind,yind)
            partialSum + active_pprods(xind)(yind)
        }
        // println(k+xLenB-1)
        reduced_pprods(k+xLenB-1) := sum
    }
    // final reduce
    val vSumsOut = Wire(Vec(4, UInt((xLenB*2 + xLenB/2 + 1).W)))
    for (vsew <- 0 until 4) {
        val lenBlocks = 1 << vsew
        val numBlocks = xLenB/lenBlocks
        val vSum = Wire(Vec(numBlocks, UInt((xLenB*2 + xLenB/2 + 1).W)))
        for (i <- 0 until numBlocks) {
            vSum(i) := (0 to lenBlocks).foldLeft(0.U) { (partialSum, j) =>
                partialSum + reduced_pprods(lenBlocks*i + j)
            }
        }
        vSumsOut(vsew) := vSum.flatten
    }
    io.out := vSumsOut(io.eew)
}