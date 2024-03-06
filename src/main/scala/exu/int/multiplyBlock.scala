package saturn.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import saturn.common._
import saturn.insns._

class Multiplier(width: Int) extends Module {
  val io = IO(new Bundle {
    val in1_signed = Input(Bool())
    val in2_signed = Input(Bool())

    val in1 = Input(UInt(width.W))
    val in2 = Input(UInt(width.W))

    val out_data = Output(UInt((2*width).W))
  })

  val lhs = Cat(io.in1_signed && io.in1(width-1), io.in1).asSInt
  val rhs = Cat(io.in2_signed && io.in2(width-1), io.in2).asSInt

  val prod = lhs * rhs
  
  io.out_data := prod(2*width-1,0)
}

class MultiplyBlock extends Module {
  val xLen = 64

  val io = IO(new Bundle {
    val in1_signed = Input(Bool())
    val in2_signed = Input(Bool())
    val eew = Input(UInt(2.W))

    val in1 = Input(UInt(xLen.W))
    val in2 = Input(UInt(xLen.W))

    val out_data = Output(UInt((2*xLen).W))
  })

    val mul64 = Module(new Multiplier(64))
    mul64.io.in1_signed := io.in1_signed
    mul64.io.in2_signed := io.in2_signed
    mul64.io.in1 := io.in1
    mul64.io.in2 := io.in2
    
    val mul32 = Module(new Multiplier(32))
    mul32.io.in1_signed := io.in1_signed
    mul32.io.in2_signed := io.in2_signed
    mul32.io.in1 := io.in1(63,32)
    mul32.io.in2 := io.in2(63,32)
    
    val mul16 = Seq.tabulate(2) { i => 
        val indh = 32*(i+1) - 1
        val indl = 32*i + 16
        val in1 = io.in1(indh, indl)
        val in2 = io.in2(indh, indl)
        println(s"i: $i, indh: $indh, indl: $indl")
        val mul = Module(new Multiplier(16))
        mul.io.in1_signed := io.in1_signed
        mul.io.in2_signed := io.in2_signed
        mul.io.in1 := in1
        mul.io.in2 := in2
        mul
    }
    val mul8 = Seq.tabulate(4) { i => 
        val indh = 16*(i+1) - 1
        val indl = 16*i + 8
        val in1 = io.in1(indh, indl)
        val in2 = io.in2(indh, indl)
        println(s"i: $i, indh: $indh, indl: $indl")
        val mul = Module(new Multiplier(8))
        mul.io.in1_signed := io.in1_signed
        mul.io.in2_signed := io.in2_signed
        mul.io.in1 := in1
        mul.io.in2 := in2
        mul
    }
    when (io.eew === 0.U) {
        mul16(1).io.in1 := Cat(Fill(8, io.in1_signed && io.in1(55)), io.in1(55, 48))
        mul16(1).io.in2 := Cat(Fill(8, io.in2_signed && io.in2(55)), io.in2(55, 48))
        mul32.io.in1 := Cat(Fill(8, io.in1_signed && io.in1(39)), io.in1(39, 32))
        mul32.io.in2 := Cat(Fill(8, io.in2_signed && io.in2(39)), io.in2(39, 32))
        mul16(0).io.in1 := Cat(Fill(8, io.in1_signed && io.in1(23)), io.in1(23, 16))
        mul16(0).io.in2 := Cat(Fill(8, io.in2_signed && io.in2(23)), io.in2(23, 16))
        mul64.io.in1 := Cat(Fill(8, io.in1_signed && io.in1(7)), io.in1(7,0))
        mul64.io.in2 := Cat(Fill(8, io.in2_signed && io.in2(7)), io.in2(7,0))

        io.out_data := Cat(mul8(3).io.out_data, 
                            mul16(1).io.out_data(15,0),
                            mul8(2).io.out_data,
                            mul32.io.out_data(15,0),
                            mul8(1).io.out_data,
                            mul16(0).io.out_data(15,0),
                            mul8(0).io.out_data,
                            mul64.io.out_data(15,0))
      }
    .elsewhen (io.eew === 1.U) {
        mul32.io.in1 := Cat(Fill(16, io.in1_signed && io.in1(47)), io.in1(47, 32))
        mul32.io.in2 := Cat(Fill(16, io.in2_signed && io.in2(47)), io.in2(47, 32))
        mul64.io.in1 := Cat(Fill(16, io.in1_signed && io.in1(15)), io.in1(15,0))
        mul64.io.in2 := Cat(Fill(16, io.in2_signed && io.in2(15)), io.in2(15,0))
    
        io.out_data := Cat(mul16(1).io.out_data,
                            mul32.io.out_data(31,0), 
                            mul16(0).io.out_data, 
                            mul64.io.out_data(31,0))
    }
    .elsewhen (io.eew === 2.U) {
        mul64.io.in1 := Cat(Fill(32, io.in1_signed && io.in1(31)), io.in1(31,0))
        mul64.io.in2 := Cat(Fill(32, io.in2_signed && io.in2(31)), io.in2(31,0))
    
        io.out_data := Cat(mul32.io.out_data, 
                            mul64.io.out_data(63,0))
    }
    .otherwise {
        io.out_data := mul64.io.out_data
    }
    print(s"mul8(3): ${mul8(3).io.out_data}\n")
    print(s"mul16(1) lsbs: ${mul16(1).io.out_data(15,0)}\n")

    // val results = (0 until 4).map { eew => ...
}