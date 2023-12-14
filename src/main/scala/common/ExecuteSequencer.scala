package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

class ExecuteSequencer(implicit p: Parameters) extends PipeSequencer()(p) {
  val valid = RegInit(false.B)
  val inst  = Reg(new VectorIssueInst)
  val wvd_mask  = Reg(UInt(egsTotal.W))
  val rvs1_mask = Reg(UInt(egsTotal.W))
  val rvs2_mask = Reg(UInt(egsTotal.W))
  val rvd_mask  = Reg(UInt(egsTotal.W))
  val rvm_mask  = Reg(UInt(egsPerVReg.W))
  val wide_vd     = Reg(Bool()) // vd reads/writes at 2xSEW
  val wide_vs2    = Reg(Bool()) // vs2 reads at 2xSEW
  val writes_mask = Reg(Bool()) // writes dest as a mask
  val widen2      = Reg(Bool()) // writes to 2x RF banks/cycle

  val vs1_eew  = inst.vconfig.vtype.vsew
  val vs2_eew  = inst.vconfig.vtype.vsew + wide_vs2 - Mux(inst.opmf6 === OPMFunct6.xunary0,
    ~inst.rs1(2,1) + 1.U, 0.U)
  val vs3_eew  = inst.vconfig.vtype.vsew + wide_vd
  val vd_eew   = inst.vconfig.vtype.vsew + wide_vd
  val incr_eew = inst.vconfig.vtype.vsew + (wide_vs2 || wide_vd)

  val renv1 = Reg(Bool())
  val renv2 = Reg(Bool())
  val renvd = Reg(Bool())
  val renvm = Reg(Bool())

  val use_wmask = !inst.vm && !inst.opif6.isOneOf(OPIFunct6.adc, OPIFunct6.madc, OPIFunct6.sbc, OPIFunct6.msbc, OPIFunct6.merge)

  val eidx      = Reg(UInt(log2Ceil(maxVLMax).W))
  val next_eidx = get_next_eidx(inst.vconfig.vl, eidx, incr_eew, io.sub_dlen)
  val last      = next_eidx === inst.vconfig.vl

  val active    = !io.dis.inst.vmu
  io.dis.ready := !active || !valid || (last && io.iss.fire)

  when (io.dis.fire && active) {
    valid := true.B
    inst := io.dis.inst
    eidx := io.dis.inst.vstart

    val dis_wide_vd :: dis_wide_vs2 :: dis_writes_mask :: dis_widen2 :: Nil = VecDecode.applyBools(
      io.dis.inst.funct3, io.dis.inst.funct6,
      Seq.fill(4)(false.B), Seq(
        (OPMFunct6.waddu    , Seq(Y,N,N,Y)),
        (OPMFunct6.wadd     , Seq(Y,N,N,Y)),
        (OPMFunct6.wsubu    , Seq(Y,N,N,Y)),
        (OPMFunct6.wsub     , Seq(Y,N,N,Y)),
        (OPMFunct6.wadduw   , Seq(Y,Y,N,N)),
        (OPMFunct6.waddw    , Seq(Y,Y,N,N)),
        (OPMFunct6.wsubuw   , Seq(Y,Y,N,N)),
        (OPMFunct6.wsubw    , Seq(Y,Y,N,N)),
        (OPIFunct6.nsra     , Seq(N,Y,N,N)),
        (OPIFunct6.nsrl     , Seq(N,Y,N,N)),
        (OPIFunct6.madc     , Seq(N,N,Y,N)),
        (OPIFunct6.msbc     , Seq(N,N,Y,N)),
        (OPIFunct6.mseq     , Seq(N,N,Y,N)),
        (OPIFunct6.msne     , Seq(N,N,Y,N)),
        (OPIFunct6.msltu    , Seq(N,N,Y,N)),
        (OPIFunct6.mslt     , Seq(N,N,Y,N)),
        (OPIFunct6.msleu    , Seq(N,N,Y,N)),
        (OPIFunct6.msle     , Seq(N,N,Y,N)),
        (OPIFunct6.msgtu    , Seq(N,N,Y,N)),
        (OPIFunct6.msgt     , Seq(N,N,Y,N)),
        (OPMFunct6.wmul     , Seq(Y,N,N,N)),
        (OPMFunct6.wmulu    , Seq(Y,N,N,N)),
        (OPMFunct6.wmulsu   , Seq(Y,N,N,N)),
        (OPMFunct6.wmaccu   , Seq(Y,N,N,N)),
        (OPMFunct6.wmacc    , Seq(Y,N,N,N)),
        (OPMFunct6.wmaccsu  , Seq(Y,N,N,N)),
        (OPMFunct6.wmaccus  , Seq(Y,N,N,N)),
        (OPFFunct6.vfwadd   , Seq(Y,N,N,N)),
        (OPFFunct6.vfwsub   , Seq(Y,N,N,N)),
        (OPFFunct6.vfwaddw  , Seq(Y,Y,N,N)),
        (OPFFunct6.vfwsubw  , Seq(Y,Y,N,N)),
        (OPFFunct6.vfwmul   , Seq(Y,N,N,N)),
        (OPFFunct6.vfwmacc  , Seq(Y,N,N,N)),
        (OPFFunct6.vfwnmacc , Seq(Y,N,N,N)),
        (OPFFunct6.vfwmsac  , Seq(Y,N,N,N)),
        (OPFFunct6.vfwnmsac , Seq(Y,N,N,N)),
        (OPFFunct6.vmfeq    , Seq(N,N,Y,N)),
        (OPFFunct6.vmfne    , Seq(N,N,Y,N)),
        (OPFFunct6.vmflt    , Seq(N,N,Y,N)),
        (OPFFunct6.vmfle    , Seq(N,N,Y,N)),
        (OPFFunct6.vmfgt    , Seq(N,N,Y,N)),
        (OPFFunct6.vmfge    , Seq(N,N,Y,N)),
      )
    )

    val vd_group_mask  = get_group_mask(io.dis.inst.pos_lmul +& dis_wide_vd , 4)
    val vs2_group_mask = get_group_mask(io.dis.inst.pos_lmul +& dis_wide_vs2, 4)
    val group_mask     = get_group_mask(io.dis.inst.pos_lmul                , 3)

    val vd_arch_mask  = get_arch_mask(io.dis.inst.rd , vd_group_mask)
    val vs1_arch_mask = get_arch_mask(io.dis.inst.rs1, group_mask)
    val vs2_arch_mask = get_arch_mask(io.dis.inst.rs2, vs2_group_mask)

    val dis_renv1 = io.dis.inst.funct3.isOneOf(OPIVV, OPFVV, OPMVV)
    val dis_renv2 = !(io.dis.inst.opif6 === OPIFunct6.merge && io.dis.inst.vm)
    val dis_renvd = io.dis.inst.opmf6.isOneOf(
      OPMFunct6.macc, OPMFunct6.nmsac, OPMFunct6.madd, OPMFunct6.nmsub,
      OPMFunct6.wmaccu, OPMFunct6.wmacc, OPMFunct6.wmaccsu, OPMFunct6.wmaccus) ||
      (io.dis.inst.funct3.isOneOf(OPFVV, OPFVF) && io.dis.inst.opff6.isOneOf(
      OPFFunct6.vfmacc, OPFFunct6.vfnmacc, OPFFunct6.vfmsac, OPFFunct6.vfnmsac,
      OPFFunct6.vfmadd, OPFFunct6.vfnmadd, OPFFunct6.vfmsub, OPFFunct6.vfnmsub,
      OPFFunct6.vfwmacc, OPFFunct6.vfwnmacc, OPFFunct6.vfwmsac, OPFFunct6.vfwnmsac))
    val dis_renvm = !inst.vm || io.dis.inst.opif6 === OPIFunct6.merge
    wvd_mask      := FillInterleaved(egsPerVReg, vd_arch_mask)
    rvs1_mask := Mux(dis_renv1, FillInterleaved(egsPerVReg, vs1_arch_mask), 0.U)
    rvs2_mask := Mux(dis_renv2, FillInterleaved(egsPerVReg, vs2_arch_mask), 0.U)
    rvd_mask  := Mux(dis_renvd, FillInterleaved(egsPerVReg, vd_arch_mask), 0.U)
    rvm_mask  := Mux(dis_renvm, ~(0.U(egsPerVReg.W)), 0.U)
    wide_vd     := dis_wide_vd
    wide_vs2    := dis_wide_vs2
    writes_mask := dis_writes_mask
    widen2      := dis_widen2
    renv1       := dis_renv1
    renv2       := dis_renv2
    renvd       := dis_renvd
    renvm       := dis_renvm
  } .elsewhen (last && io.iss.fire) {
    valid := false.B
  }

  io.seq_hazards.valid := valid
  io.seq_hazards.rintent := rvs1_mask | rvs2_mask | rvd_mask | rvm_mask
  io.seq_hazards.wintent := wvd_mask
  io.seq_hazards.vat := inst.vat

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
  io.iss.bits.wvd_eg    := getEgId(inst.rd, Mux(writes_mask, eidx >> 3, eidx), Mux(writes_mask, 0.U, vd_eew))
  io.iss.bits.wvd_widen2 := widen2
  io.iss.bits.rs1       := inst.rs1
  io.iss.bits.funct3    := inst.funct3
  io.iss.bits.funct6    := inst.funct6
  io.iss.bits.last      := last
  io.iss.bits.vat       := inst.vat
  io.iss.bits.vm        := inst.vm
  io.iss.bits.vxrm      := inst.vxrm
  io.iss.bits.frm       := inst.frm
  io.iss.bits.frs1_data := inst.frs1_data

  val dlen_mask = ~(0.U(dLenB.W))
  val head_mask = dlen_mask << (eidx << (vd_eew - widen2))(dLenOffBits-1,0)
  val tail_mask = dlen_mask >> (0.U(dLenOffBits.W) - (next_eidx << (vd_eew - widen2))(dLenOffBits-1,0))
  val vm_off    = ((1 << dLenOffBits) - 1).U(log2Ceil(dLen).W)
  val vm_eidx   = (eidx & ~(vm_off >> (vd_eew - widen2)))(log2Ceil(dLen)-1,0)
  val vm_resp   = (io.rvm.resp >> vm_eidx)
  val vm_mask   = Mux(use_wmask, VecInit.tabulate(4)({ sew =>
    FillInterleaved(1 << sew, vm_resp)
  })(vd_eew - widen2), ~(0.U(dLenB.W)))
  io.iss.bits.wmask := head_mask & tail_mask & vm_mask
  io.iss.bits.rmask := Mux(inst.vm, ~(0.U(dLenB.W)), vm_resp)

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
