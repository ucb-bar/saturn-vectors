package vector.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

class VectorIssueBeat(pipe_depth: Int)(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val inst = new VectorIssueInst
  val wvd = Bool()

  val eidx = UInt(log2Ceil(maxVLMax).W)

  val rvs1_data = UInt(dLen.W)
  val rvs2_data = UInt(dLen.W)
  val rvd_data  = UInt(dLen.W)

  val rvs1_eew = UInt(2.W)
  val rvs2_eew = UInt(2.W)
  val rvd_eew = UInt(2.W)

  val wvd_byte   = UInt(log2Ceil(egsTotal*dLenB).W)
  def wvd_eg     = wvd_byte >> log2Ceil(dLenB)
  val wmask   = UInt(dLenB.W)

  val wlat = UInt(log2Ceil(pipe_depth+1).W)
}

class PipeHazard(depth: Int)(implicit p: Parameters) extends CoreBundle()(p) with HasVectorParams {
  val eg = UInt(log2Ceil(egsTotal).W)
  val vat = UInt(vParams.vatSz.W)
  val hazard_oh = UInt(depth.W)
  def hazard = hazard_oh(0)
  val clear_vat = Bool()
}


class PipeSequencer(val depth: Int, sel: VectorIssueInst => Bool,
  writeVD: Boolean, readVS1: Boolean, readVS2: Boolean, readVD: Boolean,
)(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val dis = new Bundle {
      val fire = Input(Bool())
      val ready = Output(Bool())
      val inst = Input(new VectorIssueInst)
      val clear_vat = Input(Bool())

      val wvd = Input(Bool())
      val renv1 = Input(Bool())
      val renv2 = Input(Bool())
      val renvd = Input(Bool())
      val renvm = Input(Bool())
      val seg_nf = Input(UInt(3.W))

      val execmode = Input(UInt(2.W))

      val vs1_eew = Input(UInt(2.W))
      val vs2_eew = Input(UInt(2.W))
      val vs3_eew = Input(UInt(2.W))
      val vd_eew = Input(UInt(2.W))
      val incr_eew = Input(UInt(2.W))
      val sub_dlen = Input(UInt(2.W))
      val pipe_lat = Input(UInt((log2Ceil(depth+1)).W))
    }

    val valid = Output(Bool())
    val busy = Output(Bool())
    val iss = Decoupled(new VectorIssueBeat(depth))
    val rvs1 = new VectorReadIO
    val rvs2 = new VectorReadIO
    val rvd  = new VectorReadIO
    val seq_hazards = new Bundle {
      val valid = Output(Bool())
      val rintent = Output(UInt(egsTotal.W))
      val wintent = Output(UInt(egsTotal.W))
      val vat = Output(UInt(vParams.vatSz.W))

      val writes = Input(UInt(egsTotal.W))
      val reads = Input(UInt(egsTotal.W))
    }
    val pipe_hazards = Vec(depth, Valid(new PipeHazard(depth)))

    val vat_release = Valid(UInt(vParams.vatSz.W))
  })

  def min(a: UInt, b: UInt) = Mux(a > b, b, a)

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
  val wvd     = Reg(Bool())
  val vs1_eew = Reg(UInt(2.W))
  val vs2_eew = Reg(UInt(2.W))
  val vs3_eew = Reg(UInt(2.W))
  val vd_eew  = Reg(UInt(2.W))
  val incr_eew = Reg(UInt(2.W))
  val seg_nf  = Reg(UInt(3.W))
  val eidx    = Reg(UInt(log2Ceil(maxVLMax).W))
  val sidx    = Reg(UInt(3.W))
  val mode    = Reg(UInt(2.W))
  val clear_vat = Reg(Bool())
  val sub_dlen = Reg(UInt(2.W))
  val lat     = Reg(UInt(log2Ceil(depth+1).W))
  val next_eidx = min(
    Mux(mode =/= execRegular, eidx +& 1.U, inst.vconfig.vl),
    ((eidx << incr_eew) + (dLenB.U >> sub_dlen)) >> incr_eew)
  val last      = next_eidx === inst.vconfig.vl && sidx === seg_nf
  val eewmask   = eewByteMask(vd_eew)



  io.dis.ready := !sel(io.dis.inst) || !valid || (last && io.iss.fire)

  val dis_fire = io.dis.fire && sel(io.dis.inst)
  when (dis_fire) {
    valid := true.B
    inst := io.dis.inst
    eidx := io.dis.inst.vstart
    sidx := 0.U
    val lmul_mask = ((1.U << ((1.U << io.dis.inst.pos_lmul) +& 1.U + io.dis.seg_nf)) - 1.U)(31,0)
    val wvd_arch_oh = Mux(writeVD.B && io.dis.wvd,
      lmul_mask << io.dis.inst.rd, 0.U)
    val rvs1_arch_oh = Mux(readVS1.B && io.dis.renv1,
      lmul_mask << io.dis.inst.rs1, 0.U)
    val rvs2_arch_oh = Mux(readVS2.B && io.dis.renv2,
      lmul_mask << io.dis.inst.rs2, 0.U)
    val rvd_arch_oh  = Mux(readVD.B && io.dis.renvd,
      lmul_mask << io.dis.inst.rd, 0.U)
    wvd_oh := FillInterleaved(egsPerVReg, wvd_arch_oh)
    rvs1_oh := FillInterleaved(egsPerVReg, rvs1_arch_oh)
    rvs2_oh := FillInterleaved(egsPerVReg, rvs2_arch_oh)
    rvd_oh := FillInterleaved(egsPerVReg, rvd_arch_oh)
    renv1 := io.dis.renv1 && readVS1.B
    renv2 := io.dis.renv2 && readVS2.B
    renvd := io.dis.renvd && readVD.B
    renvm := io.dis.renvm
    wvd := io.dis.wvd && writeVD.B
    mode := io.dis.execmode
    vs1_eew := io.dis.vs1_eew
    vs2_eew := io.dis.vs2_eew
    vs3_eew := io.dis.vs3_eew
    vd_eew := io.dis.vd_eew
    incr_eew := io.dis.incr_eew
    seg_nf := io.dis.seg_nf
    sub_dlen := io.dis.sub_dlen
    lat := io.dis.pipe_lat
    clear_vat := io.dis.clear_vat
  } .elsewhen (last && io.iss.fire) {
    valid := false.B
  }

  io.seq_hazards.valid := valid
  io.seq_hazards.rintent := rvs1_oh | rvs2_oh | rvd_oh | Mux(renvm, ~(0.U(egsPerVReg.W)), 0.U)
  io.seq_hazards.wintent := wvd_oh
  io.seq_hazards.vat := inst.vat

  val pipe_writes = (io.pipe_hazards.map(h => Mux(h.valid, UIntToOH(h.bits.eg), 0.U)) ++ Seq(0.U)).reduce(_|_)

  val vs1_read_oh = Mux(renv1, UIntToOH(io.rvs1.req.bits), 0.U)
  val vs2_read_oh = Mux(renv2, UIntToOH(io.rvs2.req.bits), 0.U)
  val vd_read_oh  = Mux(renvd, UIntToOH(io.rvd.req.bits ) , 0.U)
  val vd_write_oh = Mux(wvd  , UIntToOH(io.iss.bits.wvd_eg) , 0.U)

  val raw_hazard = ((vs1_read_oh | vs2_read_oh | vd_read_oh) & (pipe_writes | io.seq_hazards.writes)) =/= 0.U
  val waw_hazard = (vd_write_oh & (pipe_writes | io.seq_hazards.writes)) =/= 0.U
  val war_hazard = (vd_write_oh & io.seq_hazards.reads) =/= 0.U
  val data_hazard = raw_hazard || waw_hazard || war_hazard


  io.valid := valid

  io.rvs1.req.bits := getEgId(inst.rs1 + (sidx << inst.pos_lmul), eidx, vs1_eew)
  io.rvs2.req.bits := getEgId(inst.rs2 + (sidx << inst.pos_lmul), eidx, vs2_eew)
  io.rvd.req.bits  := getEgId(inst.rd  + (sidx << inst.pos_lmul), eidx, vs3_eew)

  io.rvs1.req.valid := valid && renv1 && !data_hazard
  io.rvs2.req.valid := valid && renv2 && !data_hazard
  io.rvd.req.valid  := valid && renvd && !data_hazard

  io.iss.valid := (valid &&
    !data_hazard &&
    !(renv1 && !io.rvs1.req.ready) &&
    !(renv2 && !io.rvs2.req.ready) &&
    !(renvd && !io.rvd.req.ready)
  )

  io.iss.bits.inst := inst
  io.iss.bits.wvd   := wvd

  io.iss.bits.rvs1_data := io.rvs1.resp
  io.iss.bits.rvs2_data := io.rvs2.resp
  io.iss.bits.rvd_data  := io.rvd.resp
  io.iss.bits.rvs1_eew  := vs1_eew
  io.iss.bits.rvs2_eew  := vs2_eew
  io.iss.bits.rvd_eew   := vs3_eew
  io.iss.bits.eidx      := eidx
  io.iss.bits.wvd_byte  := getByteId(inst.rd  + (sidx << inst.pos_lmul), eidx, vd_eew)
  io.iss.bits.wlat := lat

  val dlen_mask = (1.U << (dLenB.U >> sub_dlen)) - 1.U
  val head_mask = dlen_mask << (eidx << vd_eew)(dLenOffBits-1,0)
  val tail_mask = dlen_mask >> (0.U(dLenOffBits.W) - (next_eidx << vd_eew)(dLenOffBits-1,0))
  io.iss.bits.wmask := head_mask & tail_mask

  val pipe_valids = Seq.fill(depth) { RegInit(false.B) }
  val pipe_hazards = Seq.fill(depth) { Reg(new PipeHazard(depth)) }

  when (io.iss.fire && !last) {
    when (mode === execRegular && sub_dlen === 0.U) {
      wvd_oh  := wvd_oh  & ~UIntToOH(io.iss.bits.wvd_eg)
      rvs1_oh := rvs1_oh & ~UIntToOH(io.rvs1.req.bits)
      rvs2_oh := rvs2_oh & ~UIntToOH(io.rvs2.req.bits)
      rvd_oh  := rvd_oh  & ~UIntToOH(io.rvd.req.bits)
    }
    when (sidx === seg_nf) {
      sidx := 0.U
      eidx := next_eidx
    } .otherwise {
      sidx := sidx + 1.U
    }
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
      pipe_hazards.head.clear_vat := last && clear_vat
      pipe_hazards.head.hazard_oh := (1.U << lat) - 1.U
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
    io.vat_release.valid := io.iss.fire && last && clear_vat
    io.vat_release.bits := inst.vat
  }

  io.busy := (pipe_valids :+ valid).reduce(_||_)
}
