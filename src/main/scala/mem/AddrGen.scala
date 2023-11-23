package vector.mem

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import vector.common._

class AddrGen(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val done = Output(Bool())
    val op = Input(new VectorMemMacroOp)
    val maskindex = Flipped(Decoupled(new MaskIndex))
    val req = Decoupled(new MemRequest)

    val out = Decoupled(new IFQEntry)
  })

  def min(a: UInt, b: UInt) = Mux(a > b, b, a)

  def getElems(off: UInt, eew: UInt): UInt = {
    (dLenB.U - off(dLenOffBits-1,0)) >> eew
  }

  val r_eaddr = Reg(UInt(paddrBits.W))
  val r_saddr = Reg(UInt(paddrBits.W))
  val r_eidx = Reg(UInt((1+log2Ceil(8*maxVLMax)).W))
  val r_sidx = Reg(UInt(3.W))
  val r_head = RegInit(true.B)

  val fast_segmented = io.op.mop === mopUnit
  val eidx = Mux(r_head,
    io.op.vstart * (Mux(fast_segmented, io.op.seg_nf, 0.U) +& 1.U),
    r_eidx)
  val sidx = Mux(r_head, 0.U             , r_sidx)
  val eaddr = Mux(r_head, io.op.base_addr, r_eaddr) + Mux(io.op.mop(0),
    io.maskindex.bits.index & eewBitMask(io.op.idx_size), 0.U)
  val saddr = Mux(io.op.nf =/= 0.U, Mux(r_head, eaddr, r_saddr), eaddr)


  val mem_size = io.op.elem_size
  val max_eidx = Mux(fast_segmented,
    io.op.vl * (io.op.seg_nf +& 1.U),
    io.op.vl)
  val stride = Mux(io.op.mop === mopStrided, io.op.stride,
    Mux(io.op.mop === mopUnit, dLenB.U, 0.U))

  val next_max_elems = getElems(saddr, mem_size)
  val next_contig_elems = Mux(fast_segmented,
    max_eidx - eidx,
    io.op.nf +& 1.U - sidx)
  val next_act_elems = min(next_contig_elems, next_max_elems)(dLenOffBits,0)
  val next_act_bytes = next_act_elems << mem_size

  val next_sidx = sidx +& next_act_elems
  val next_eidx = eidx +& Mux(fast_segmented, next_act_elems, 1.U)

  val next_eaddr = eaddr + Mux(io.op.mop === mopUnit, next_act_bytes, Mux(io.op.mop === mopStrided, io.op.stride, 0.U))
  val next_saddr = saddr + next_act_bytes

  val needs_mask = !io.op.vm && io.op.mop =/= mopUnit
  val needs_index = io.op.mop(0)
  val block_maskindex = (needs_mask || needs_index) && !io.maskindex.valid

  val masked = needs_mask && !io.maskindex.bits.mask
  val may_clear = (fast_segmented || next_sidx > io.op.nf) && next_eidx >= max_eidx


  io.done := false.B
  io.maskindex.ready := false.B
  io.out.valid := io.valid && !block_maskindex && (masked || io.req.ready)
  io.out.bits.head := saddr
  io.out.bits.tail := saddr + next_act_bytes
  io.out.bits.masked := masked
  io.out.bits.last := may_clear

  io.req.valid := io.valid && io.out.ready && !block_maskindex && !masked
  io.req.bits.addr := (saddr >> dLenOffBits) << dLenOffBits
  io.req.bits.data := DontCare
  io.req.bits.mask := ((1.U << next_act_bytes) - 1.U) << saddr(dLenOffBits-1,0)
  io.req.bits.phys := io.op.phys

  when (io.out.fire) {
    when (next_sidx > io.op.nf || fast_segmented) {
      r_eaddr := next_eaddr
      r_saddr := next_eaddr
      r_eidx := next_eidx
      r_sidx := 0.U
      io.maskindex.ready := needs_mask || needs_index
    } .otherwise {
      r_eaddr := io.op.base_addr
      r_saddr := next_saddr
      r_eidx := io.op.vstart
      r_sidx := next_sidx
    }
    r_head := false.B
    when (may_clear) {
      io.done := true.B
      r_head := true.B
    }
  }

}
