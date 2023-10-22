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
        val in2 = Input(Vec(xLenB, UInt(8.W)))
        
        val out = Input(Vec(xLenB, UInt(16.W)))

    })
    ///////////////////////////////////////////
    //generate block-diagonal matrix use_pprod
    ///////////////////////////////////////////
    val use_pprod = Wire(Vec(4, Vec(xLenB, UInt(xLenB.W))))  
    for (vsew <- 0 until 4) {
        val lenBlocks = 1 << vsew
        val numBlocks = xLenB/lenBlocks
        val blockValue = (1 << (lenBlocks)) - 1
        // println("xlenB", xLenB)
        // println("vsew", vsew)
        // println("lenBlocks",lenBlocks) 
        // println("nblocks", numBlocks)
        // println("blockVal",blockValue)
        for (i <- 0 until numBlocks) {
            for (j <- 0 until lenBlocks) { //ewB
                use_pprod(vsew)(i*lenBlocks+j) := (blockValue << (lenBlocks*i)).U
                // println((blockValue << (lenBlocks*i)).toBinaryString)

            }
        }
    }
    //sum valid partial products
    val active_pprod = Wire(Vec(xLenB, Vec(xLenB, UInt(16.W))))
    for (i <- 0 until xLenB) {
        for (j <- 0 until xLenB) {
            val pprod = io.in1(i) * io.in2(j)
            println(pprod)
            active_pprod(i)(j) := Mux(use_pprod(io.eew)(i).asBools(j), pprod, 0.U)
            println(active_pprod)
        }
        io.out(i) := active_pprod(i).reduce(_ + _)
    }

// val out = (0 until xLenB).map { i =>
//             (0 until xLenB).map { j =>
//                 val pprod = in1(i) * in2(j)
//                 active_pprod(i)(j) := Mux(use_pprod(i).asBools(j), pprod(i,j), 0.U)

}