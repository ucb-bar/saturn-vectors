package vector.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import vector.common._

class IntegerReductionPipe(implicit p: Parameters) extends IterativeFunctionalUnit()(p) {
  io.iss.sub_dlen := log2Ceil(dLenB).U - io.iss.op.rvs1_eew
  io.set_vxsat := false.B

  lazy val ctrl_table = Seq(
    (OPMFunct6.redsum     , Seq(Y,N,N)),
    (OPMFunct6.redmaxu    , Seq(N,Y,N)),
    (OPMFunct6.redmax     , Seq(N,Y,N)),
    (OPMFunct6.redminu    , Seq(N,Y,N)),
    (OPMFunct6.redmin     , Seq(N,Y,N)),
    (OPMFunct6.redand     , Seq(N,N,Y)),
    (OPMFunct6.redor      , Seq(N,N,Y)),
    (OPMFunct6.redxor     , Seq(N,N,Y)),
  )

  override def accepts(f3: UInt, f6: UInt): Bool = VecDecode(f3, f6, ctrl_table.map(_._1))
  val ctrl_sum :: ctrl_cmp :: ctrl_bw :: Nil = VecDecode.applyBools(
    io.iss.op.funct3, io.iss.op.funct6,
    Seq.fill(3)(X), ctrl_table)

  val signed = io.iss.op.funct6(0)
  val minmax = io.iss.op.funct6(1)
  val is_and = io.iss.op.funct6(0)
  val is_or  = io.iss.op.funct6(1)

  val vs1_0 = Wire(UInt(64.W))

  val in_eew = io.iss.op.rvs2_eew
  val out_eew = io.iss.op.vd_eew
  val eidx = io.iss.op.eidx

  val vs2_i = extract(io.iss.op.rvs2_data, false.B, in_eew, eidx)(63,0)
  val vs2_i_mask = io.iss.op.wmask((eidx << in_eew) % dLenB.U)
  val masked = !(io.iss.op.vm || vs2_i_mask)

  val active = RegInit(false.B)
  when (active && io.write.fire()) {
    active := false.B
  } .elsewhen (!active && io.iss.valid) {
    active := true.B
  }
  
  io.hazard.valid := active
  io.hazard.bits.vat := op.vat
  io.hazard.bits.eg := op.wvd_eg
  io.hazard.bits.widen2 := false.B

  vs1_0 := extract(io.iss.op.rvs1_data, false.B, in_eew, 0.U)(63,0)

  val red_accum = RegInit(0.U(64.W))

  val gt_u = vs2_i > red_accum 
  val gt = VecInit.tabulate(4){eew => vs2_i((8 << eew) - 1, 0).asSInt > red_accum((8 << eew) - 1, 0).asSInt}(in_eew)
  val equal = vs2_i === red_accum

  val gt_u_init = vs2_i > vs1_0
  val gt_init = VecInit.tabulate(4){eew => vs2_i((8 << eew) - 1, 0).asSInt > vs1_0((8 << eew) - 1, 0).asSInt}(in_eew)
  val equal_init = vs2_i === vs1_0

  val switch_values = !masked && !equal && ((signed && ((!minmax && !gt) || (minmax && gt))) ||
                                (!signed && ((!minmax && !gt_u) || (minmax && gt_u))))

  val select_init_value = !equal_init && ((signed && ((!minmax && !gt_init) || (minmax && gt_init))) ||
                                         (!signed && ((!minmax && !gt_u_init) || (minmax && gt_u_init))))

  when(io.iss.valid && active && !masked) {
    when (ctrl_sum) {
      red_accum := (red_accum.asSInt + vs2_i.asSInt).asUInt
    } .elsewhen (ctrl_cmp && switch_values) {
      red_accum := vs2_i        
    } .elsewhen (ctrl_bw) {
      when (is_and && is_or) {
        red_accum := red_accum ^ vs2_i
      } .elsewhen (is_and) {
        red_accum := red_accum & vs2_i
      } .otherwise {
        red_accum := red_accum | vs2_i
      }
    }
  } .elsewhen (io.iss.valid && !active) {
    when (masked) {
      red_accum := vs1_0
    } .elsewhen (ctrl_sum) {
      red_accum := (vs2_i.asSInt + vs1_0.asSInt).asUInt
    } .elsewhen (ctrl_cmp) {
      when (select_init_value) {
        red_accum := vs2_i
      } .otherwise {
        red_accum := vs1_0
      }
    } .elsewhen (ctrl_bw) {
      when (is_and && is_or) {
        red_accum := vs2_i ^ vs1_0
      } .elsewhen (is_and) {
        red_accum := vs2_i & vs1_0
      } .otherwise {
        red_accum := vs2_i | vs1_0
      }
    }
  }

  val mask_select = Seq(FillInterleaved(8, "b00000001".U),
                        FillInterleaved(8, "b00000011".U),
                        FillInterleaved(8, "b00001111".U),
                        FillInterleaved(8, "b11111111".U))

  val wdata = Wire(UInt(64.W)) 
  wdata := red_accum

  io.write.valid := active && op.last
  io.write.bits.eg := op.wvd_eg
  io.write.bits.data := red_accum
  io.write.bits.mask := mask_select(out_eew) 

  last := true.B

  io.iss.ready := accepts(io.iss.op.funct3, io.iss.op.funct6) && !(active && op.last)
  io.vat.valid := io.write.fire()
}
