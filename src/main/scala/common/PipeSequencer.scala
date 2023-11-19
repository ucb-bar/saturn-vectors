package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

abstract class PipeSequencer(val depth: Int)(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val dis = new Bundle {
      val fire = Input(Bool())
      val ready = Output(Bool())
      val inst = Input(new VectorIssueInst)

      // TODO remove
      val renv1 = Input(Bool())
      val renv2 = Input(Bool())
      val renvd = Input(Bool())
      val renvm = Input(Bool())

      val vs1_eew = Input(UInt(2.W))
      val vs2_eew = Input(UInt(2.W))
      val vs3_eew = Input(UInt(2.W))
      val vd_eew = Input(UInt(2.W))
      val vd_widen2 = Input(Bool())
      val incr_eew = Input(UInt(2.W))
      val pipe_lat = Input(UInt((log2Ceil(depth+1)).W))
      val use_wmask = Input(Bool())
    }

    val seq_hazards = new Bundle {
      val valid = Output(Bool())
      val rintent = Output(UInt(egsTotal.W))
      val wintent = Output(UInt(egsTotal.W))
      val vat = Output(UInt(vParams.vatSz.W))

      val writes = Input(UInt(egsTotal.W))
      val reads = Input(UInt(egsTotal.W))
    }

    val busy = Output(Bool())
    val pipe_hazards = Vec(depth, Valid(new PipeHazard(depth)))
    val vat_release = Valid(UInt(vParams.vatSz.W))


    val rvs1 = new VectorReadIO
    val rvs2 = new VectorReadIO
    val rvd  = new VectorReadIO
    val rvm  = new VectorReadIO

    val iss = Decoupled(new VectorMicroOp(depth))
  })
  def min(a: UInt, b: UInt) = Mux(a > b, b, a)
  def get_head_mask(bit_mask: UInt, eidx: UInt, eew: UInt) = bit_mask << (eidx << eew)(dLenOffBits-1,0)
  def get_tail_mask(bit_mask: UInt, eidx: UInt, eew: UInt) = bit_mask >> (0.U(dLenOffBits.W) - (eidx << eew)(dLenOffBits-1,0))
  def get_vm_mask(mask_resp: UInt, eidx: UInt, eew: UInt) = {
    val vm_off  = ((1 << dLenOffBits) - 1).U(log2Ceil(dLen).W)
    val vm_eidx = eidx & ~(vm_off >> eew)(log2Ceil(dLen)-1,0)
    val vm_resp = mask_resp >> vm_eidx
    Mux1H(UIntToOH(eew), (0 until 4).map { w => FillInterleaved(1 << w, vm_resp) })
  }
  def get_next_eidx(vl: UInt, eidx: UInt, eew: UInt, sub_dlen: UInt) = min(vl,
    (((eidx >> (dLenOffBits.U - eew - sub_dlen)) + 1.U) << (dLenOffBits.U - eew - sub_dlen))(log2Ceil(maxVLMax)+1,0)
  )

  io.rvs1.req.valid := false.B
  io.rvs1.req.bits := DontCare
  io.rvs2.req.valid := false.B
  io.rvs2.req.bits := DontCare
  io.rvd.req.valid := false.B
  io.rvd.req.bits := DontCare
  io.rvm.req.valid := false.B
  io.rvm.req.bits := DontCare

}

class OldPipeSequencer(depth: Int, sel: VectorIssueInst => Bool,
  writeVD: Boolean, readVS1: Boolean, readVS2: Boolean, readVD: Boolean,
  segmented: Boolean
)(implicit p: Parameters) extends PipeSequencer(depth)(p) {
  val valid   = RegInit(false.B)
  val inst    = Reg(new VectorIssueInst)
  val wvd_oh  = Reg(UInt(egsTotal.W))
  val rvs1_oh = Reg(UInt(egsTotal.W))
  val rvs2_oh = Reg(UInt(egsTotal.W))
  val rvd_oh  = Reg(UInt(egsTotal.W))
  val renv1   = Reg(Bool())
  val renv2   = Reg(Bool())
  val renvd   = Reg(Bool())
  val renvm   = Reg(Bool())
  val vs1_eew = Reg(UInt(2.W))
  val vs2_eew = Reg(UInt(2.W))
  val vs3_eew = Reg(UInt(2.W))
  val vd_eew  = Reg(UInt(2.W))
  val vd_widen2 = Reg(Bool())
  val incr_eew = Reg(UInt(2.W))
  val eidx    = Reg(UInt(log2Ceil(maxVLMax).W))
  val lat     = Reg(UInt(log2Ceil(depth+1).W))
  val use_wmask = Reg(Bool())
  val next_eidx = min(
    inst.vconfig.vl,
    ((eidx << incr_eew) + dLenB.U) >> incr_eew)
  val last      = next_eidx === inst.vconfig.vl
  val eewmask   = eewByteMask(vd_eew)



  io.dis.ready := !sel(io.dis.inst) || !valid || (last && io.iss.fire)

  val dis_fire = io.dis.fire && sel(io.dis.inst)
  when (dis_fire) {
    valid := true.B
    inst := io.dis.inst
    eidx := io.dis.inst.vstart
    def mul(eew: UInt) = (io.dis.inst.vconfig.vl >> (log2Ceil(vLen/8).U - eew))(3,0)
    def mul_mask(eew: UInt) = ((1.U << (mul(eew) + 1.U)) - 1.U)(7,0)

    val wvd_arch_oh = Mux(writeVD.B,
      mul_mask(io.dis.vd_eew + io.dis.vd_widen2) << io.dis.inst.rd, 0.U)
    val rvs1_arch_oh = Mux(readVS1.B && io.dis.renv1,
      mul_mask(io.dis.vs1_eew) << io.dis.inst.rs1, 0.U)
    val rvs2_arch_oh = Mux(readVS2.B && io.dis.renv2,
      mul_mask(io.dis.vs2_eew) << io.dis.inst.rs2, 0.U)
    val rvd_arch_oh  = Mux(readVD.B && io.dis.renvd,
      mul_mask(io.dis.vs3_eew) << io.dis.inst.rd, 0.U)
    wvd_oh := FillInterleaved(egsPerVReg, wvd_arch_oh)
    rvs1_oh := FillInterleaved(egsPerVReg, rvs1_arch_oh)
    rvs2_oh := FillInterleaved(egsPerVReg, rvs2_arch_oh)
    rvd_oh := FillInterleaved(egsPerVReg, rvd_arch_oh)
    renv1 := io.dis.renv1 && readVS1.B
    renv2 := io.dis.renv2 && readVS2.B
    renvd := io.dis.renvd && readVD.B
    renvm := io.dis.renvm
    vs1_eew := io.dis.vs1_eew
    vs2_eew := io.dis.vs2_eew
    vs3_eew := io.dis.vs3_eew
    vd_eew := io.dis.vd_eew
    vd_widen2 := io.dis.vd_widen2
    incr_eew := io.dis.incr_eew
    lat := io.dis.pipe_lat
    use_wmask := io.dis.use_wmask
  } .elsewhen (last && io.iss.fire) {
    valid := false.B
  }

  io.seq_hazards.valid := valid
  io.seq_hazards.rintent := rvs1_oh | rvs2_oh | rvd_oh | Mux(renvm, ~(0.U(egsPerVReg.W)), 0.U)
  io.seq_hazards.wintent := wvd_oh
  io.seq_hazards.vat := inst.vat

  val pipe_writes = (io.pipe_hazards.map(h => Mux(h.valid && h.bits.hazard, h.bits.eg_oh, 0.U)) ++ Seq(0.U)).reduce(_|_)

  val vs1_read_oh = Mux(renv1, UIntToOH(io.rvs1.req.bits), 0.U)
  val vs2_read_oh = Mux(renv2, UIntToOH(io.rvs2.req.bits), 0.U)
  val vd_read_oh  = Mux(renvd, UIntToOH(io.rvd.req.bits ) , 0.U)
  val vd_write_oh = UIntToOH(io.iss.bits.wvd_eg)

  val raw_hazard = ((vs1_read_oh | vs2_read_oh | vd_read_oh) & (pipe_writes | io.seq_hazards.writes)) =/= 0.U
  val waw_hazard = (vd_write_oh & (pipe_writes | io.seq_hazards.writes)) =/= 0.U
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
    io.iss.bits.rvs1_data := dLenSplat(inst.rs1_data, vs1_eew)
  }
  io.iss.bits.rvs2_data := io.rvs2.resp
  io.iss.bits.rvd_data  := io.rvd.resp
  io.iss.bits.rvs1_eew  := vs1_eew
  io.iss.bits.rvs2_eew  := vs2_eew
  io.iss.bits.rvd_eew   := vs3_eew
  io.iss.bits.vd_eew    := vd_eew
  io.iss.bits.eidx      := eidx
  io.iss.bits.wvd_eg    := getEgId(inst.rd, eidx, vd_eew + vd_widen2)
  io.iss.bits.wvd_widen2 := vd_widen2
  io.iss.bits.wlat      := lat
  io.iss.bits.rs1       := inst.rs1
  io.iss.bits.funct3    := inst.funct3
  io.iss.bits.funct6    := inst.funct6
  io.iss.bits.load      := inst.opcode(5)

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

  val pipe_valids = Seq.fill(depth) { RegInit(false.B) }
  val pipe_hazards = Seq.fill(depth) { Reg(new PipeHazard(depth)) }

  when (io.iss.fire && !last) {
    when (tail_mask(dLenB-1)) {
      val wvd_clr_mask = Mux(vd_widen2, FillInterleaved(2, UIntToOH(io.iss.bits.wvd_eg >> 1)), UIntToOH(io.iss.bits.wvd_eg))
      wvd_oh  := wvd_oh  & ~wvd_clr_mask
      rvs1_oh := rvs1_oh & ~UIntToOH(io.rvs1.req.bits)
      rvs2_oh := rvs2_oh & ~UIntToOH(io.rvs2.req.bits)
      rvd_oh  := rvd_oh  & ~UIntToOH(io.rvd.req.bits)
    }
    eidx := next_eidx
  }

  for (i <- 0 until depth) {
    io.pipe_hazards(i).valid := pipe_valids(i)
    io.pipe_hazards(i).bits := pipe_hazards(i)
  }

  if (depth > 0) {
    when (io.iss.fire) {
      pipe_valids.head := true.B
      pipe_hazards.head.eg := io.iss.bits.wvd_eg
      pipe_hazards.head.vat := inst.vat
      pipe_hazards.head.clear_vat := last
      pipe_hazards.head.hazard_oh := (1.U << lat) - 1.U
      pipe_hazards.head.wvd_widen2 := vd_widen2
    } .otherwise {
      pipe_valids.head := false.B
    }
    for (i <- 1 until depth) {
      pipe_valids(i) := pipe_valids(i-1)
      when (pipe_valids(i-1)) {
        pipe_hazards(i) := pipe_hazards(i-1)
        pipe_hazards(i).hazard_oh := pipe_hazards(i-1).hazard_oh >> 1
      }
    }
    io.vat_release.valid := pipe_valids.last && pipe_hazards.last.clear_vat
    io.vat_release.bits := pipe_hazards.last.vat
  } else {
    io.vat_release.valid := io.iss.fire && last
    io.vat_release.bits := inst.vat
  }

  io.busy := (pipe_valids :+ valid).reduce(_||_)
}
