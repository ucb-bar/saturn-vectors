package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

class AddrGen(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val done = Output(Bool())
    val inst = Input(new VectorIssueInst)
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


  val eidx = Mux(r_head, io.inst.vstart  , r_eidx)
  val sidx = Mux(r_head, 0.U             , r_sidx)
  val eaddr = Mux(r_head, io.inst.rs1_data, r_eaddr) + Mux(io.inst.mop(0),
    io.maskindex.bits.index & eewBitMask(io.inst.mem_size), 0.U)
  val saddr = Mux(io.inst.nf =/= 0.U, Mux(r_head, eaddr, r_saddr), eaddr)

  val mem_size = Mux(io.inst.mop(0), io.inst.vconfig.vtype.vsew, io.inst.mem_size)
  val max_eidx = Mux(io.inst.mop === mopUnit,
    io.inst.vconfig.vl * (io.inst.nf +& 1.U),
    io.inst.vconfig.vl)
  val stride = Mux(io.inst.mop === mopStrided, io.inst.rs2_data,
    Mux(io.inst.mop === mopUnit, dLenB.U, 0.U))

  val next_max_elems = getElems(saddr, mem_size)
  val next_contig_elems = Mux(io.inst.mop === mopUnit,
    max_eidx - eidx,
    io.inst.nf +& 1.U - sidx)
  val next_act_elems = min(next_contig_elems, next_max_elems)(dLenOffBits,0)
  val next_act_bytes = next_act_elems << mem_size

  val next_sidx = sidx +& next_act_elems
  val next_eidx = eidx +& Mux(io.inst.nf =/= 0.U, 1.U, next_act_elems)

  val next_eaddr = eaddr + Mux(io.inst.mop === mopUnit, next_act_bytes, Mux(io.inst.mop === mopStrided, io.inst.rs2_data, 0.U))
  val next_saddr = saddr + next_act_bytes

  val needs_mask = !io.inst.vm
  val needs_index = io.inst.mop(0)
  val block_maskindex = (needs_mask || needs_index) && !io.maskindex.valid

  val masked = needs_mask && !io.maskindex.bits.mask
  val may_clear = next_sidx > io.inst.nf && next_eidx === max_eidx


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
  io.req.bits.phys := io.inst.phys

  when (io.out.fire) {
    when (next_sidx > io.inst.nf || io.inst.mop === mopUnit) {
      r_eaddr := next_eaddr
      r_saddr := next_eaddr
      r_eidx := next_eidx
      r_sidx := 0.U
      io.maskindex.ready := true.B
    } .otherwise {
      r_eaddr := io.inst.rs1_data
      r_saddr := next_saddr
      r_eidx := io.inst.vstart
      r_sidx := next_sidx
    }
    r_head := false.B
    when (may_clear) {
      io.done := true.B
      r_head := true.B
    }
  }

}
