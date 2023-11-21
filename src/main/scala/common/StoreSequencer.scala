package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

class StoreSequencer(implicit p: Parameters) extends PipeSequencer(0)(p) {
  val valid    = RegInit(false.B)
  val inst     = Reg(new VectorIssueInst)
  val eidx     = Reg(UInt(log2Ceil(maxVLMax).W))
  val sidx     = Reg(UInt(3.W))
  val rvd_mask = Reg(UInt(egsTotal.W))
  val sub_dlen = Reg(UInt(2.W))

  val renvm     = !inst.vm && inst.mop === mopUnit
  val next_eidx = get_next_eidx(inst.vconfig.vl, eidx, inst.mem_elem_size, sub_dlen)
  val last      = next_eidx === inst.vconfig.vl && sidx === inst.seg_nf

  val active = io.dis.inst.vmu && io.dis.inst.opcode(5)
  io.dis.ready := !active || !valid || (last && io.iss.fire)

  when (io.dis.fire && active) {
    valid := true.B
    inst  := io.dis.inst
    eidx  := io.dis.inst.vstart
    sidx  := 0.U

    val rvd_arch_mask = Wire(Vec(32, Bool()))
    for (i <- 0 until 32) {
      val group = i.U >> io.dis.inst.pos_lmul
      val rd_group = io.dis.inst.rd >> io.dis.inst.pos_lmul
      rvd_arch_mask(i) := group >= rd_group && group <= (rd_group + io.dis.inst.nf)
    }
    rvd_mask := FillInterleaved(egsPerVReg, rvd_arch_mask.asUInt)

    sub_dlen := Mux(io.dis.inst.seg_nf =/= 0.U && (dLenOffBits.U > (3.U +& io.dis.inst.mem_elem_size)),
      dLenOffBits.U - 3.U - io.dis.inst.mem_elem_size,
      0.U)
  } .elsewhen (last && io.iss.fire) {
    valid := false.B
  }

  io.seq_hazards.valid := valid
  io.seq_hazards.rintent := rvd_mask | Mux(renvm, ~(0.U(egsPerVReg.W)), 0.U)
  io.seq_hazards.wintent := 0.U
  io.seq_hazards.vat := inst.vat

  val vd_read_oh = UIntToOH(io.rvd.req.bits)
  val vm_read_oh = Mux(renvm, UIntToOH(io.rvm.req.bits), 0.U)

  val raw_hazard = ((vm_read_oh | vd_read_oh) & io.seq_hazards.writes) =/= 0.U
  val data_hazard = raw_hazard

  io.rvd.req.valid := valid
  io.rvd.req.bits := getEgId(inst.rd + (sidx << inst.pos_lmul), eidx, inst.mem_elem_size)
  io.rvm.req.valid := valid && renvm
  io.rvm.req.bits := getEgId(0.U, eidx >> 3, 0.U)

  io.iss.valid := valid && !data_hazard && (!renvm || io.rvm.req.ready) && io.rvd.req.ready
  io.iss.bits.wvd := false.B
  io.iss.bits.rvs1_data := DontCare
  io.iss.bits.rvs2_data := DontCare
  io.iss.bits.rvd_data  := io.rvd.resp
  io.iss.bits.rvs1_eew  := DontCare
  io.iss.bits.rvs2_eew  := DontCare
  io.iss.bits.rvd_eew   := inst.mem_elem_size
  io.iss.bits.vd_eew    := DontCare
  io.iss.bits.eidx      := eidx
  io.iss.bits.wvd_eg    := DontCare
  io.iss.bits.wvd_widen2 := false.B
  io.iss.bits.wlat       := 0.U
  io.iss.bits.rs1        := inst.rs1
  io.iss.bits.funct3     := DontCare
  io.iss.bits.funct6     := DontCare

  val head_mask = get_head_mask(~(0.U(dLenB.W)), eidx     , inst.mem_elem_size)
  val tail_mask = get_tail_mask(~(0.U(dLenB.W)), next_eidx, inst.mem_elem_size)
  val vm_mask   = Mux(!renvm, ~(0.U(dLenB.W)), get_vm_mask(io.rvm.resp, eidx, inst.mem_elem_size))
  io.iss.bits.wmask := head_mask & tail_mask & vm_mask
  io.iss.bits.rmask := vm_mask

  when (io.iss.fire && !last) {
    when (next_is_new_eg(eidx, next_eidx, inst.mem_elem_size)) {
      rvd_mask := rvd_mask & ~vd_read_oh
    }
    when (sidx === inst.seg_nf) {
      sidx := 0.U
      eidx := next_eidx
    } .otherwise {
      sidx := sidx + 1.U
    }
  }

  io.vat_release.valid := false.B
  io.vat_release.bits := DontCare

  io.busy := valid
}
