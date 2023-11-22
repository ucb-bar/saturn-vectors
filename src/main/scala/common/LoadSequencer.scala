package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

class LoadSequencer(implicit p: Parameters) extends PipeSequencer(0)(p) {
  val valid = RegInit(false.B)
  val inst  = Reg(new VectorIssueInst)
  val eidx  = Reg(UInt(log2Ceil(maxVLMax).W))
  val sidx  = Reg(UInt(3.W))
  val wvd_mask = Reg(UInt(egsTotal.W))
  val rvm_mask = Reg(UInt(egsPerVReg.W))

  val renvm     = !inst.vm
  val next_eidx = get_next_eidx(inst.vconfig.vl, eidx, inst.mem_elem_size, 0.U)
  val last      = next_eidx === inst.vconfig.vl && sidx === inst.seg_nf

  val active = io.dis.inst.vmu && !io.dis.inst.opcode(5)
  io.dis.ready := !active || !valid || (last && io.iss.fire)

  when (io.dis.fire && active) {
    valid := true.B
    inst  := io.dis.inst
    eidx  := io.dis.inst.vstart
    sidx  := 0.U

    val wvd_arch_mask = Wire(Vec(32, Bool()))
    for (i <- 0 until 32) {
      val group = i.U >> io.dis.inst.pos_lmul
      val rd_group = io.dis.inst.rd >> io.dis.inst.pos_lmul
      wvd_arch_mask(i) := group >= rd_group && group <= (rd_group + io.dis.inst.nf)
    }
    wvd_mask := FillInterleaved(egsPerVReg, wvd_arch_mask.asUInt)
    rvm_mask := Mux(!io.dis.inst.vm, ~(0.U(egsPerVReg.W)), 0.U)
  } .elsewhen (last && io.iss.fire) {
    valid := false.B
  }

  io.seq_hazards.valid := valid
  io.seq_hazards.rintent := rvm_mask
  io.seq_hazards.wintent := wvd_mask
  io.seq_hazards.vat     := inst.vat

  val vm_read_oh  = Mux(renvm, UIntToOH(io.rvm.req.bits), 0.U)
  val vd_write_oh = UIntToOH(io.iss.bits.wvd_eg)

  val raw_hazard = (vm_read_oh & io.seq_hazards.writes) =/= 0.U
  val waw_hazard = (vd_write_oh & io.seq_hazards.writes) =/= 0.U
  val war_hazard = (vd_write_oh & io.seq_hazards.reads) =/= 0.U
  val data_hazard = raw_hazard || waw_hazard || war_hazard

  io.rvm.req.valid := valid && renvm
  io.rvm.req.bits := getEgId(0.U, eidx >> 3, 0.U)

  io.iss.valid := valid && !data_hazard && (!renvm || io.rvm.req.ready)
  io.iss.bits.wvd       := true.B
  io.iss.bits.rvs1_data := DontCare
  io.iss.bits.rvs2_data := DontCare
  io.iss.bits.rvd_data  := DontCare
  io.iss.bits.rvs1_eew  := DontCare
  io.iss.bits.rvs2_eew  := DontCare
  io.iss.bits.rvd_eew   := DontCare
  io.iss.bits.vd_eew    := inst.mem_elem_size
  io.iss.bits.eidx      := eidx
  io.iss.bits.wvd_eg    := getEgId(inst.rd + (sidx << inst.pos_lmul), eidx, inst.mem_elem_size)
  io.iss.bits.wvd_widen2 := false.B
  io.iss.bits.wlat       := 0.U
  io.iss.bits.rs1        := inst.rs1
  io.iss.bits.funct3     := DontCare
  io.iss.bits.funct6     := DontCare
  io.iss.bits.last       := last
  io.iss.bits.vat        := inst.vat
  io.iss.bits.vm         := inst.vm

  val head_mask = get_head_mask(~(0.U(dLenB.W)), eidx     , inst.mem_elem_size)
  val tail_mask = get_tail_mask(~(0.U(dLenB.W)), next_eidx, inst.mem_elem_size)
  val vm_mask   = Mux(!renvm, ~(0.U(dLenB.W)), get_vm_mask(io.rvm.resp, eidx, inst.mem_elem_size))
  io.iss.bits.wmask := head_mask & tail_mask & vm_mask
  io.iss.bits.rmask := DontCare

  when (io.iss.fire && !last) {
    when (next_is_new_eg(eidx, next_eidx, inst.mem_elem_size)) {
      wvd_mask := wvd_mask & ~vd_write_oh
    }
    when (next_mask_is_new_eg(eidx, next_eidx)) {
      rvm_mask := rvm_mask & ~UIntToOH(io.rvm.req.bits)
    }
    when (sidx === inst.seg_nf) {
      sidx := 0.U
      eidx := next_eidx
    } .otherwise {
      sidx := sidx + 1.U
    }
  }

  io.busy := valid
}
