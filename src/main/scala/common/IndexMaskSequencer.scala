package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

class IndexMaskSequencer(implicit p: Parameters) extends PipeSequencer(0)(p) {
  val valid = RegInit(false.B)
  val inst  = Reg(new VectorIssueInst)
  val eidx  = Reg(UInt(log2Ceil(maxVLMax).W))
  val rvs2_mask = Reg(UInt(egsTotal.W))
  val rvm_mask = Reg(UInt(egsPerVReg.W))

  val renvm = !inst.vm && inst.mop =/= mopUnit
  val renv2 = inst.mop(0)
  val next_eidx = eidx +& 1.U
  val last = next_eidx === inst.vconfig.vl

  val active = io.dis.inst.vmu && ((!io.dis.inst.vm && io.dis.inst.mop =/= mopUnit) || io.dis.inst.mop(0))
  io.dis.ready := !active || !valid || (last && io.iss.fire)

  when (io.dis.fire && active) {
    valid := true.B
    inst  := io.dis.inst
    eidx  := io.dis.inst.vstart

    val group_mask = get_group_mask(io.dis.inst.pos_lmul, 3)
    val renv2_arch_mask = get_arch_mask(io.dis.inst.rs2, group_mask)

    rvs2_mask := FillInterleaved(egsPerVReg, renv2_arch_mask)
    rvm_mask := Mux(!inst.vm && inst.mop =/= mopUnit, ~(0.U(egsPerVReg.W)), 0.U)
  } .elsewhen (last && io.iss.fire) {
    valid := false.B
  }

  io.seq_hazards.valid := valid
  io.seq_hazards.rintent := rvs2_mask | rvm_mask
  io.seq_hazards.wintent := false.B
  io.seq_hazards.vat := inst.vat

  val vs2_read_oh = Mux(renv2, UIntToOH(io.rvs2.req.bits), 0.U)
  val vm_read_oh  = Mux(renvm, UIntToOH(io.rvm.req.bits), 0.U)

  val raw_hazard = ((vm_read_oh | vs2_read_oh) & io.seq_hazards.writes) =/= 0.U
  val data_hazard = raw_hazard

  io.rvs2.req.valid := valid && renv2
  io.rvs2.req.bits := getEgId(inst.rs2, eidx, inst.mem_idx_size)
  io.rvm.req.valid := valid && renvm
  io.rvm.req.bits := getEgId(0.U, eidx >> 3, 0.U)

  io.iss.valid := valid && !data_hazard && (!renvm || io.rvm.req.ready) && (!renv2 || io.rvs2.req.ready)
  io.iss.bits.wvd := false.B
  io.iss.bits.rvs1_data := DontCare
  io.iss.bits.rvs2_data := io.rvs2.resp
  io.iss.bits.rvd_data  := DontCare
  io.iss.bits.rvs1_eew  := DontCare
  io.iss.bits.rvs2_eew  := inst.mem_idx_size
  io.iss.bits.rvd_eew   := DontCare
  io.iss.bits.vd_eew    := DontCare
  io.iss.bits.eidx      := eidx
  io.iss.bits.wvd_eg    := DontCare
  io.iss.bits.wvd_widen2 := false.B
  io.iss.bits.wlat       := 0.U
  io.iss.bits.rs1        := inst.rs1
  io.iss.bits.funct3     := DontCare
  io.iss.bits.funct6     := DontCare
  io.iss.bits.last       := last
  io.iss.bits.vat        := inst.vat
  io.iss.bits.vm         := inst.vm

  val tail_mask = get_tail_mask(~(0.U(dLenB.W)), next_eidx, inst.mem_elem_size)
  val vm_mask = Mux(!renvm, ~(0.U(dLenB.W)), get_vm_mask(io.rvm.resp, eidx, inst.mem_elem_size))
  io.iss.bits.wmask      := vm_mask
  io.iss.bits.rmask      := vm_mask

  when (io.iss.fire && !last) {
    when (next_is_new_eg(eidx, next_eidx, inst.mem_idx_size)) {
      rvs2_mask := rvs2_mask & ~vs2_read_oh
    }
    when (next_mask_is_new_eg(eidx, next_eidx)) {
      rvm_mask := rvm_mask & ~UIntToOH(io.rvm.req.bits)
    }
    eidx := next_eidx
  }

  io.busy := valid
}
