package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

class ExecuteSequencer(implicit p: Parameters) extends PipeSequencer(3)(p) {
  val valid = RegInit(false.B)
  val inst  = Reg(new VectorIssueInst)
  val wvd_mask  = Reg(UInt(egsTotal.W))
  val rvs1_mask = Reg(UInt(egsTotal.W))
  val rvs2_mask = Reg(UInt(egsTotal.W))
  val rvd_mask  = Reg(UInt(egsTotal.W))
  val rvm_mask  = Reg(UInt(egsPerVReg.W))

  val renv1 = inst.funct3.isOneOf(OPIVI, OPFVV, OPMVV)
  val renv2 = true.B
  val renvd = false.B
  val renvm = !inst.vm

  val widen2 = inst.opmf6.isOneOf(OPMFunct6.waddu, OPMFunct6.wadd, OPMFunct6.wsub, OPMFunct6.wsubu)
  val writes_mask = inst.opif6.isOneOf(OPIFunct6.madc, OPIFunct6.msbc)
  val use_wmask = !inst.vm && !inst.opif6.isOneOf(OPIFunct6.adc, OPIFunct6.madc, OPIFunct6.sbc, OPIFunct6.msbc)

  val wide_in = inst.opmf6.isOneOf(OPMFunct6.wadduw, OPMFunct6.waddw, OPMFunct6.wsubuw, OPMFunct6.wsubw)
  val vs1_eew = inst.vconfig.vtype.vsew
  val vs2_eew = inst.vconfig.vtype.vsew + wide_in - Mux(inst.opmf6 === OPMFunct6.xunary0,
    ~inst.rs1(2,1) + 1.U, 0.U)
  val vs3_eew = inst.vconfig.vtype.vsew
  val vd_eew  = inst.vconfig.vtype.vsew + wide_in
  val incr_eew = inst.vconfig.vtype.vsew + wide_in


  val eidx      = Reg(UInt(log2Ceil(maxVLMax).W))
  val next_eidx = get_next_eidx(inst.vconfig.vl, eidx, incr_eew, 0.U)
  val last      = next_eidx === inst.vconfig.vl

  val active    = !io.dis.inst.vmu
  io.dis.ready := !active || !valid || (last && io.iss.fire)

  when (io.dis.fire && active) {
    valid := true.B
    inst := io.dis.inst
    eidx := io.dis.inst.vstart
    val dis_widen2 = io.dis.inst.opmf6.isOneOf(OPMFunct6.waddu, OPMFunct6.wadd, OPMFunct6.wsub, OPMFunct6.wsubu)
    val dis_wide_in = io.dis.inst.opmf6.isOneOf(OPMFunct6.wadduw, OPMFunct6.waddw, OPMFunct6.wsubuw, OPMFunct6.wsubw)

    val vd_group_mask  = get_group_mask(io.dis.inst.pos_lmul +& dis_widen2 +& dis_wide_in, 4)
    val vs2_group_mask = get_group_mask(io.dis.inst.pos_lmul               +& dis_wide_in, 4)
    val group_mask     = get_group_mask(io.dis.inst.pos_lmul                             , 3)

    val vd_arch_mask  = get_arch_mask(io.dis.inst.rd , vd_group_mask)
    val vs1_arch_mask = get_arch_mask(io.dis.inst.rs1, group_mask)
    val vs2_arch_mask = get_arch_mask(io.dis.inst.rs2, vs2_group_mask)

    val dis_renv1 = io.dis.inst.funct3.isOneOf(OPIVI, OPFVV, OPMVV)
    val dis_renv2 = true.B
    val dis_renvd = false.B
    val dis_renvm = !inst.vm
    wvd_mask      := FillInterleaved(egsPerVReg, vd_arch_mask)
    rvs1_mask := Mux(dis_renv1, FillInterleaved(egsPerVReg, vs1_arch_mask), 0.U)
    rvs2_mask := Mux(dis_renv2, FillInterleaved(egsPerVReg, vs2_arch_mask), 0.U)
    rvd_mask  := Mux(dis_renvd, FillInterleaved(egsPerVReg, vd_arch_mask), 0.U)
    rvm_mask  := Mux(dis_renvm, ~(0.U(egsPerVReg.W)), 0.U)
  } .elsewhen (last && io.iss.fire) {
    valid := false.B
  }

  io.seq_hazards.valid := valid
  io.seq_hazards.rintent := rvs1_mask | rvs2_mask | rvd_mask | rvm_mask
  io.seq_hazards.wintent := wvd_mask
  io.seq_hazards.vat := inst.vat

  // val pipe_writes = io.pipe_hazards.map(h => Mux(h.valid && h.bits.hazard, h.bits.eg_oh, 0.U)).reduce(_|_)

  val vs1_read_oh = Mux(renv1, UIntToOH(io.rvs1.req.bits), 0.U)
  val vs2_read_oh = Mux(renv2, UIntToOH(io.rvs2.req.bits), 0.U)
  val vd_read_oh  = Mux(renvd, UIntToOH(io.rvd.req.bits ), 0.U)
  val vm_read_oh  = Mux(renvm, UIntToOH(io.rvm.req.bits ), 0.U)
  val vd_write_oh = UIntToOH(io.iss.bits.wvd_eg)

  val raw_hazard = ((vs1_read_oh | vs2_read_oh | vd_read_oh | vm_read_oh) & io.seq_hazards.writes) =/= 0.U
  val waw_hazard = (vd_write_oh & io.seq_hazards.writes) =/= 0.U
  val war_hazard = (vd_write_oh & io.seq_hazards.reads) =/= 0.U
  val data_hazard = raw_hazard || waw_hazard || war_hazard


  io.rvs1.req.bits := getEgId(inst.rs1, eidx     , vs1_eew)
  io.rvs2.req.bits := getEgId(inst.rs2, eidx     , vs2_eew)
  io.rvd.req.bits  := getEgId(inst.rd , eidx     , vs3_eew)
  io.rvm.req.bits  := getEgId(0.U     , eidx >> 3, 0.U)

  io.rvs1.req.valid := valid && renv1
  io.rvs2.req.valid := valid && renv2
  io.rvd.req.valid  := valid && renvd
  io.rvm.req.valid  := valid && renvm

  io.iss.valid := (valid &&
    !data_hazard &&
    !(renv1 && !io.rvs1.req.ready) &&
    !(renv2 && !io.rvs2.req.ready) &&
    !(renvd && !io.rvd.req.ready) &&
    !(renvm && !io.rvm.req.ready)
  )
  val lat = 1.U

  io.iss.bits.wvd   := true.B

  io.iss.bits.rvs1_data := io.rvs1.resp
  when (inst.funct3.isOneOf(OPIVI, OPIVX, OPMVX) && !inst.vmu) {
    val rs1_data = Mux(inst.funct3 === OPIVI, Cat(Fill(59, inst.imm5(4)), inst.imm5), inst.rs1_data)
    io.iss.bits.rvs1_data := dLenSplat(rs1_data, vs1_eew)
  }
  io.iss.bits.rvs2_data := io.rvs2.resp
  io.iss.bits.rvd_data  := io.rvd.resp
  io.iss.bits.rvs1_eew  := vs1_eew
  io.iss.bits.rvs2_eew  := vs2_eew
  io.iss.bits.rvd_eew   := vs3_eew
  io.iss.bits.vd_eew    := vd_eew
  io.iss.bits.eidx      := eidx
  io.iss.bits.wvd_eg    := getEgId(inst.rd, Mux(writes_mask, eidx >> 3, eidx), Mux(writes_mask, 0.U, vd_eew + widen2))
  io.iss.bits.wvd_widen2 := widen2
  io.iss.bits.wlat      := lat
  io.iss.bits.rs1       := inst.rs1
  io.iss.bits.funct3    := inst.funct3
  io.iss.bits.funct6    := inst.funct6
  io.iss.bits.last       := last
  io.iss.bits.vat        := inst.vat
  io.iss.bits.vm         := inst.vm

  val dlen_mask = ~(0.U(dLenB.W))
  val head_mask = dlen_mask << (eidx << vd_eew)(dLenOffBits-1,0)
  val tail_mask = dlen_mask >> (0.U(dLenOffBits.W) - (next_eidx << vd_eew)(dLenOffBits-1,0))
  val vm_off    = ((1 << dLenOffBits) - 1).U(log2Ceil(dLen).W)
  val vm_eidx   = (eidx & ~(vm_off >> vd_eew))(log2Ceil(dLen)-1,0)
  val vm_resp   = (io.rvm.resp >> vm_eidx)
  val vm_mask   = Mux(use_wmask, Mux1H(UIntToOH(vd_eew), (0 until 4).map { sew =>
    FillInterleaved(1 << sew, vm_resp)
  }), ~(0.U(dLenB.W)))
  io.iss.bits.wmask := head_mask & tail_mask & vm_mask
  io.iss.bits.rmask := Mux(renvm, vm_resp, ~(0.U(dLenB.W)))

  when (io.iss.fire && !last) {
    when (Mux(writes_mask, next_mask_is_new_eg(eidx, next_eidx), next_is_new_eg(eidx, next_eidx, vd_eew))) {
      val wvd_clr_mask = Mux(widen2, FillInterleaved(2, UIntToOH(io.iss.bits.wvd_eg >> 1)), UIntToOH(io.iss.bits.wvd_eg))
      wvd_mask  := wvd_mask  & ~wvd_clr_mask
    }
    when (next_is_new_eg(eidx, next_eidx, vs2_eew)) {
      rvs2_mask := rvs2_mask & ~UIntToOH(io.rvs2.req.bits)
    }
    when (next_is_new_eg(eidx, next_eidx, vs1_eew)) {
      rvs1_mask := rvs1_mask & ~UIntToOH(io.rvs1.req.bits)
    }
    when (next_is_new_eg(eidx, next_eidx, vs3_eew)) {
      rvd_mask  := rvd_mask  & ~UIntToOH(io.rvd.req.bits)
    }
    when (next_mask_is_new_eg(eidx, next_eidx)) {
      rvm_mask  := rvm_mask  & ~UIntToOH(io.rvm.req.bits)
    }
    eidx := next_eidx
  }

  io.busy := valid
}
