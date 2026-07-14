package saturn.common

import chisel3._
import freechips.rocketchip.tile.FType
import hardfloat._

object FloatCasts {
  object FP64U {
    val F = FType.D
    def IEEE2REC(x: UInt) = F.recode(x)
    def REC2IEEE(x: UInt) = F.ieee(x)
    def one_recoded: UInt = F.recode("h3FF0000000000000".U(64.W))
  }
  object FP32U {
    val F = FType.S
    def IEEE2REC(x: UInt) = F.recode(x)
    def REC2IEEE(x: UInt) = F.ieee(x)
    def one_recoded: UInt = F.recode("h3F800000".U(32.W))
  }

  // 폭 변환 모듈
  class RecCast(eIn:Int, sIn:Int, eOut:Int, sOut:Int) extends Module {
    val io = IO(new Bundle{
      val in  = Input(UInt((1+eIn+sIn).W))
      val out = Output(UInt((1+eOut+sOut).W))
    })
    val c = Module(new RecFNToRecFN(eIn, sIn, eOut, sOut))
    c.io.in := io.in
    c.io.roundingMode := 0.U
    c.io.detectTininess := hardfloat.consts.tininess_afterRounding
    io.out := c.io.out
  }

  // helpers
  def laneIEEE(x: UInt, i: Int): UInt   = x(64*(i+1)-1, 64*i)
  def laneIEEE32(x: UInt, i: Int): UInt = x(32*(i+1)-1, 32*i)
  def laneRec(x: UInt, i: Int): UInt    = FP64U.IEEE2REC(laneIEEE(x, i))

  def laneF32_to_recD(x: UInt, i: Int): UInt = {
    val recS = FP32U.IEEE2REC(laneIEEE32(x, i))
    val cast = Module(new RecCast(FP32U.F.exp, FP32U.F.sig, FP64U.F.exp, FP64U.F.sig))
    cast.io.in := recS
    cast.io.out
  }
  def recD_to_IEEE32(x: UInt): UInt = {
    val cast = Module(new RecCast(FP64U.F.exp, FP64U.F.sig, FP32U.F.exp, FP32U.F.sig))
    cast.io.in := x
    FP32U.REC2IEEE(cast.io.out)
  }
  def f32_to_f64_ieee(x32: UInt): UInt = {
    val recS = FP32U.IEEE2REC(x32(31,0))
    val cast = Module(new RecCast(FP32U.F.exp, FP32U.F.sig, FP64U.F.exp, FP64U.F.sig))
    cast.io.in := recS
    FP64U.REC2IEEE(cast.io.out)
  }
}
