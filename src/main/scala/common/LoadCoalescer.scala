package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

class LoadCoalescer(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val lrq = Flipped(Decoupled(UInt(dLen.W)))
    val laq = Flipped(Decoupled(new LSAQEntry))

    val out = Decoupled(UInt(dLen.W))
    val maq_clear = Valid(UInt(vmaqSz.W))
  })

  val rot_reg = Reg(UInt(dLen.W))

  val in_data = Mux(io.laq.bits.masked, 0.U, io.lrq.bits)
  val in_sramt = io.laq.bits.addr(dLenOffBits-1,0)
  val in_slamt = Mux(io.laq.bits.iterative,
    (io.laq.bits.eidx << io.laq.bits.inst.mem_size)(log2Ceil(dLenB)-1,0),
    (dLen/8).U - in_sramt)
  val in_data_sr = in_data >> (in_sramt << 3)
  val in_data_sl = in_data << (in_slamt << 3)
  val in_lower_mask = (1.U << in_slamt) - 1.U
  val in_lower_mask_bytes = FillInterleaved(8, in_lower_mask)
  val in_combined_data = (in_lower_mask_bytes & rot_reg) | (~in_lower_mask_bytes & in_data_sl)


  io.out.valid := false.B
  io.laq.ready := false.B
  io.lrq.ready := false.B

  when (io.laq.bits.masked) {
    io.out.valid := io.laq.valid
    io.laq.ready := io.out.ready
  } .elsewhen (io.laq.bits.iterative || in_sramt === 0.U) {
    io.out.valid := io.laq.valid && io.lrq.valid
    io.laq.ready := io.out.ready && io.lrq.valid
    io.lrq.ready := io.out.ready
  } .otherwise {
    io.out.valid := !io.laq.bits.head && io.laq.valid && io.lrq.valid
    io.laq.ready := io.lrq.valid && (io.out.ready || !io.laq.bits.head)
    io.lrq.ready := io.laq.bits.head || io.out.ready
  }
  io.out.bits := Mux(io.laq.bits.iterative, in_data_sl,
    Mux(in_sramt === 0.U, in_data, in_combined_data))

  when (io.laq.fire) { rot_reg := in_data_sr }

  assert(!(io.lrq.valid && !io.laq.valid))

  io.maq_clear.valid := io.laq.fire && io.laq.bits.tail
  io.maq_clear.bits := io.laq.bits.maq_idx
}
