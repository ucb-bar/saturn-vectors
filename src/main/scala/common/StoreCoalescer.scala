package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

class StoreCoalescer(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val status = Input(new MStatus)
    val saq = Flipped(Decoupled(new LSAQEntry))
    val stdata = Flipped(Decoupled(new StoreData))

    val req = Decoupled(new HellaCacheReq)
    val resp = Flipped(Valid(new HellaCacheResp))
    val busy = Output(Bool())
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
  val store_tags = RegInit(VecInit.fill(4)(false.B))
  val tag = PriorityEncoder(~(store_tags.asUInt))
  val tag_available = !store_tags(tag)

  io.saq.ready := false.B
  io.stdata.ready := false.B
  io.req.valid := false.B

  io.req.bits.addr := io.saq.bits.addr
  io.req.bits.tag := tag
  io.req.bits.cmd := M_XWR
  io.req.bits.size := Mux(io.saq.bits.iterative, io.saq.bits.inst.mem_size, log2Ceil(dLenB).U)
  io.req.bits.signed := false.B
  io.req.bits.dprv := io.status.prv
  io.req.bits.dv := io.status.v
  io.req.bits.data := Mux(io.saq.bits.iterative || in_slamt === 0.U, in_data, in_combined_data)
  io.req.bits.mask := Mux(io.saq.bits.iterative || in_slamt === 0.U, in_mask, in_combined_mask)
  io.req.bits.phys := false.B
  io.req.bits.no_alloc := false.B
  io.req.bits.no_xcpt := true.B

  when (io.saq.bits.prestart || io.saq.bits.masked) {
    io.saq.ready := io.stdata.valid
    io.stdata.ready := true.B
  } .elsewhen (io.saq.bits.iterative || in_slamt === 0.U) {
    io.req.valid := io.saq.valid && io.stdata.valid && tag_available
    io.saq.ready := io.stdata.valid && io.req.ready && tag_available
    io.stdata.ready := io.saq.valid && io.req.ready && tag_available
  } .otherwise {
    io.req.valid := io.saq.valid && (io.stdata.valid || io.saq.bits.tail) && tag_available
    io.saq.ready := io.req.ready && (io.stdata.valid || io.saq.bits.tail) && tag_available
    io.stdata.ready := io.req.ready && io.saq.valid && !io.saq.bits.tail && tag_available
  }

  when (io.req.fire) { store_tags(tag) := true.B }
  when (io.resp.fire) { store_tags(io.resp.bits.tag) := false.B }

  when (io.stdata.fire) {
    rot_reg := in_data_sr
    mask_rot_reg := in_mask_sr
  }

  io.busy := store_tags.orR
}


