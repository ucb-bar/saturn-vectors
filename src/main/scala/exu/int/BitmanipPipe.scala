package saturn.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import saturn.common._
import saturn.insns._

case object BitmanipPipeFactory extends FunctionalUnitFactory {
  def depth = 2
  def insns = Seq(
    BREV8.VV, BREV.VV, REV8.VV, CLZ.VV, CTZ.VV, CPOP.VV
  ).map(_.pipelined(depth))
  def generate(implicit p: Parameters) = new BitmanipPipe(depth)(p)
}

class BitmanipPipe(depth: Int)(implicit p: Parameters) extends PipelinedFunctionalUnit(depth)(p) {
  val supported_insns = BitmanipPipeFactory.insns

  val ctrl = new VectorDecoder(
    io.pipe(0).bits,
    supported_insns,
    Seq(UsesCountZeros))

  val in1_bytes = io.pipe(0).bits.rvs1_data.asTypeOf(Vec(dLenB, UInt(8.W)))
  val in2_bytes = io.pipe(0).bits.rvs2_data.asTypeOf(Vec(dLenB, UInt(8.W)))

  val rvs1_eew = io.pipe(0).bits.rvs1_eew
  val rvs2_eew = io.pipe(0).bits.rvs2_eew
  val vd_eew   = io.pipe(0).bits.vd_eew

  val brev_bytes = VecInit(in2_bytes.map(b => Reverse(b))).asUInt
  val brev_elements = VecInit((0 until 4).map { eew =>
    VecInit(in2_bytes.asTypeOf(Vec(dLenB >> eew, UInt((8 << eew).W))).map(b => Reverse(b))).asUInt
  })(vd_eew)
  val rev8_elements = VecInit((0 until 4).map { eew =>
    VecInit(in2_bytes.asTypeOf(Vec(dLenB >> eew, Vec(1 << eew, UInt(8.W)))).map(b => VecInit(b.reverse))).asUInt
  })(vd_eew)
  val swap_out = Mux1H(Seq(
    (io.pipe(0).bits.rs1(1,0) === 0.U) -> brev_bytes,
    (io.pipe(0).bits.rs1(1,0) === 1.U) -> rev8_elements,
    (io.pipe(0).bits.rs1(1,0) === 2.U) -> brev_elements
  ))

  val tz_in = Mux(io.pipe(0).bits.rs1(0), in2_bytes, brev_elements.asTypeOf(Vec(dLenB, UInt(8.W))))
  val tz_8b = tz_in.map(b => (b === 0.U, (PriorityEncoderOH(1.U ## b) - 1.U)(7,0)))
  val tz_16b = tz_8b.grouped(2).toSeq.map(t =>
    (t.map(_._1).andR, Mux(t(0)._1, t(1)._2 ## ~(0.U(8.W)), t(0)._2))
  )
  val tz_32b = tz_16b.grouped(2).toSeq.map(t =>
    (t.map(_._1).andR, Mux(t(0)._1, t(1)._2 ## ~(0.U(16.W)), t(0)._2))
  )
  val tz_64b = tz_32b.grouped(2).toSeq.map(t =>
    (t.map(_._1).andR, Mux(t(0)._1, t(1)._2 ## ~(0.U(32.W)), t(0)._2))
  )
  val tz_out = WireInit(VecInit(
    VecInit(tz_8b.map(_._2)).asUInt,
    VecInit(tz_16b.map(_._2)).asUInt,
    VecInit(tz_32b.map(_._2)).asUInt,
    VecInit(tz_64b.map(_._2)).asUInt
  )(vd_eew).asTypeOf(Vec(dLenB, UInt(8.W))))

  val cpop_in = Mux(io.pipe(0).bits.rs1(1), in2_bytes, tz_out)
  val cpop_8b = cpop_in.map(b => PopCount(b))
  val cpop_16b = cpop_8b.grouped(2).toSeq.map(_.reduce(_ +& _))
  val cpop_32b = cpop_16b.grouped(2).toSeq.map(_.reduce(_ +& _))
  val cpop_64b = cpop_32b.grouped(2).toSeq.map(_.reduce(_ +& _))
  val cpops = Seq(cpop_8b, cpop_16b, cpop_32b, cpop_64b)
  val count_out = WireInit(VecInit((0 until 4).map { eew =>
    val out = Wire(Vec(dLenB >> eew, UInt((8 << eew).W)))
    out := VecInit(cpops(eew))
    out.asUInt
  })(vd_eew))

  val out = Mux(ctrl.bool(UsesCountZeros), count_out, swap_out)

  val pipe_out = Pipe(io.pipe(0).valid, out, depth-1).bits

  io.write.valid       := io.pipe(depth-1).valid
  io.write.bits.eg     := io.pipe(depth-1).bits.wvd_eg
  io.write.bits.mask   := FillInterleaved(8, io.pipe(depth-1).bits.wmask)
  io.write.bits.data   := pipe_out

  io.stall := false.B
  io.set_vxsat := false.B
  io.set_fflags.valid := false.B
  io.set_fflags.bits := DontCare

  io.scalar_write.valid := false.B
  io.scalar_write.bits := DontCare
}
