package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

class StoreCoalescer(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val saq = Flipped(Decoupled(new LSAQEntry))
    val stdata = Flipped(Decoupled(new StoreData))

    val req = Decoupled(new MemRequest)
    val ack = Input(Bool())
    val maq_clear = Valid(UInt(vmaqSz.W))
  })

  val rot_reg = Reg(UInt(dLen.W))
  val mask_rot_reg = Reg(UInt(dLenB.W))
  val in_data = io.stdata.bits.data
  val in_mask = io.stdata.bits.mask
  val in_slamt = io.saq.bits.addr(dLenOffBits-1,0)
  val in_sramt = (dLen/8).U - in_slamt
  val in_data_sr = in_data >> (in_sramt << 3)
  val in_data_sl = in_data << (in_slamt << 3)
  val in_mask_sr = in_mask >> in_sramt
  val in_mask_sl = in_mask << in_slamt
  val in_lower_mask = (1.U << in_slamt) - 1.U
  val in_lower_mask_bytes = FillInterleaved(8, in_lower_mask)
  val in_combined_data = (in_lower_mask_bytes & rot_reg) | (~in_lower_mask_bytes & in_data_sl)
  val in_combined_mask = (in_lower_mask & mask_rot_reg) | (~in_lower_mask & in_mask_sl)


  val soq = Module(new DCEQueue(new LSAQEntry, vParams.vsoqEntries, flow=true))

  io.saq.ready := false.B
  io.stdata.ready := false.B
  io.req.valid := false.B
  soq.io.enq.valid := false.B

  io.req.bits.addr := io.saq.bits.addr
  io.req.bits.size := Mux(io.saq.bits.iterative, io.saq.bits.inst.mem_size, log2Ceil(dLenB).U)
  io.req.bits.data := Mux(io.saq.bits.iterative || in_slamt === 0.U, in_data, in_combined_data)
  io.req.bits.mask := Mux(io.saq.bits.iterative || in_slamt === 0.U, in_mask, in_combined_mask)
  soq.io.enq.bits := io.saq.bits


  when (io.saq.valid && (io.saq.bits.prestart || io.saq.bits.masked)) {
    io.saq.ready := io.stdata.valid
    io.stdata.ready := true.B
  } .elsewhen (io.saq.bits.iterative || in_slamt === 0.U) {
    io.req.valid := io.saq.valid && io.stdata.valid && soq.io.enq.ready
    io.saq.ready := io.stdata.valid && io.req.ready && soq.io.enq.ready
    io.stdata.ready := io.saq.valid && io.req.ready && soq.io.enq.ready
    soq.io.enq.valid := io.saq.valid && io.stdata.valid && io.req.ready
  } .otherwise {
    io.req.valid := io.saq.valid && (io.stdata.valid || io.saq.bits.tail) && soq.io.enq.ready
    io.saq.ready := io.req.ready && (io.stdata.valid || io.saq.bits.tail) && soq.io.enq.ready
    io.stdata.ready := io.req.ready && io.saq.valid && !io.saq.bits.tail && soq.io.enq.ready
    soq.io.enq.valid := io.saq.valid && io.req.valid && (io.stdata.valid || io.saq.bits.tail)
  }

  when (io.stdata.fire) {
    rot_reg := in_data_sr
    mask_rot_reg := in_mask_sr
  }

  soq.io.deq.ready := io.ack
  io.maq_clear.valid := soq.io.deq.fire && soq.io.deq.bits.tail
  io.maq_clear.bits := soq.io.deq.bits.maq_idx
}
