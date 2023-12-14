package vector.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import vector.common._

class FPCompPipe(implicit p: Parameters) extends PipelinedFunctionalUnit(1, true)(p) with HasFPUParameters {
  io.iss.sub_dlen := 0.U
  io.set_vxsat := false.B

  lazy val ctrl_table = Seq(
    (OPFFunct6.vfmin    ,   Seq(Y,Y,N,N,N,N,X,X)),
    (OPFFunct6.vfmax    ,   Seq(N,Y,N,N,N,N,X,X)),
    (OPFFunct6.vfsgnj   ,   Seq(N,N,N,N,N,Y,N,N)), 
    (OPFFunct6.vfsgnjn  ,   Seq(N,N,N,N,N,Y,Y,N)), 
    (OPFFunct6.vfsgnjx  ,   Seq(N,N,N,N,N,Y,N,Y)), 
    (OPFFunct6.vmfeq    ,   Seq(N,N,N,Y,Y,N,X,X)),
    (OPFFunct6.vmfne    ,   Seq(N,N,N,Y,Y,N,X,X)),
    (OPFFunct6.vmflt    ,   Seq(N,N,Y,Y,Y,N,X,X)),
    (OPFFunct6.vmfle    ,   Seq(N,N,Y,Y,Y,N,X,X)),
    (OPFFunct6.vmfgt    ,   Seq(N,N,N,Y,Y,N,X,X)),
    (OPFFunct6.vmfge    ,   Seq(N,N,N,Y,Y,N,X,X)),
  )
  override def accepts(f3: UInt, f6: UInt): Bool = VecDecode(f3, f6, ctrl_table.map(_._1))

  val ctrl_min :: ctrl_cmp :: ctrl_cmp_less :: ctrl_rev12 :: ctrl_mask_write :: ctrl_sgnj :: ctrl_sgnjn :: ctrl_sgnjx :: Nil = VecDecode.applyBools(
    io.pipe(0).bits.funct3, io.pipe(0).bits.funct6,
    Seq.fill(8)(X), ctrl_table
  ) 
  
  val is_opfvf = io.pipe(0).bits.funct3.isOneOf(OPFVF)

  val rvs1_vals = io.pipe(0).bits.rvs1_data.asTypeOf(Vec(fmaCount, UInt(64.W)))
  val rvs2_vals = io.pipe(0).bits.rvs2_data.asTypeOf(Vec(fmaCount, UInt(64.W)))

  val comparisons = rvs2_vals.zip(rvs1_vals).map{ case(rvs2, rvs1) =>
    val cmp_d = Module(new hardfloat.CompareRecFN(11, 53))
    val cmp_s0 = Module(new hardfloat.CompareRecFN(8, 24))
    val cmp_s1 = Module(new hardfloat.CompareRecFN(8, 24))

    cmp_d.io.signaling := true.B
    cmp_s0.io.signaling := true.B
    cmp_s1.io.signaling := true.B

    val rvs2_d = FType.D.recode(rvs2)
    val rvs1_d = FType.D.recode(Mux(is_opfvf, io.pipe(0).bits.frs1_data, rvs1))
    val rvs2_s0 = FType.S.recode(rvs2(31,0))
    val rvs1_s0 = FType.S.recode(Mux(is_opfvf, io.pipe(0).bits.frs1_data(31,0), rvs1(31,0)))
    val rvs2_s1 = FType.S.recode(rvs2(63,32))
    val rvs1_s1 = FType.S.recode(Mux(is_opfvf, io.pipe(0).bits.frs1_data(31,0), rvs1(63,32)))

    val NaNs = Seq(FType.D.isNaN(rvs2_d), FType.D.isNaN(rvs1_d), FType.S.isNaN(rvs2_s0), FType.S.isNaN(rvs1_s0), FType.S.isNaN(rvs2_s1), FType.S.isNaN(rvs1_s1))
    
    cmp_d.io.a := rvs2_d
    cmp_d.io.b := rvs1_d
    cmp_s0.io.a := rvs2_s0 
    cmp_s0.io.b := rvs1_s0
    cmp_s1.io.a := rvs2_s1
    cmp_s1.io.b := rvs1_s1

    val cmp_out = Wire(Vec(2, UInt(32.W)))
    when (io.pipe(0).bits.vd_eew === 3.U) {
      when (NaNs(0) && NaNs(1)) {
        cmp_out(0) := 0.U
        cmp_out(1) := "h7ff80000".U
      } .elsewhen (NaNs(0)) {
        cmp_out(0) := Mux(is_opfvf, io.pipe(0).bits.frs1_data(31,0), rvs1(31,0))
        cmp_out(1) := Mux(is_opfvf, io.pipe(0).bits.frs1_data(63,32), rvs1(63,32))
      } .elsewhen (NaNs(1)) {
        cmp_out(0) := rvs2(31,0)
        cmp_out(1) := rvs2(63,32)
      } .otherwise {
        cmp_out(0) := Mux((!ctrl_min && cmp_d.io.gt) || (ctrl_min && cmp_d.io.lt), rvs2(31,0), Mux(is_opfvf, io.pipe(0).bits.frs1_data(31,0), rvs1(31,0)))
        cmp_out(1) := Mux((!ctrl_min && cmp_d.io.gt) || (ctrl_min && cmp_d.io.lt), rvs2(63,32), Mux(is_opfvf, io.pipe(0).bits.frs1_data(63,32), rvs1(63,32)))
      }
    } .otherwise {
      when (NaNs(2) && NaNs(3)) {
        cmp_out(0) := "h7fc00000".U
      } .elsewhen (NaNs(2)) {
        cmp_out(0) := Mux(is_opfvf, io.pipe(0).bits.frs1_data(31,0), rvs1(31,0))
      } .elsewhen (NaNs(3)) {
        cmp_out(0) := rvs2(31,0)
      } .otherwise {
        cmp_out(0) := Mux((!ctrl_min && cmp_s0.io.gt) || (ctrl_min && cmp_s0.io.lt), rvs2(31,0), Mux(is_opfvf, io.pipe(0).bits.frs1_data(31,0), rvs1(31,0))) 
      }
      when (NaNs(4) && NaNs(5)) {
        cmp_out(1) := "h7fc00000".U
      } .elsewhen (NaNs(4)) {
        cmp_out(1) := Mux(is_opfvf, io.pipe(0).bits.frs1_data(31,0), rvs1(63,32))
      } .elsewhen (NaNs(5)) {
        cmp_out(1) := rvs2(63,32)
      } .otherwise {
        cmp_out(1) := Mux((!ctrl_min && cmp_s1.io.gt) || (ctrl_min && cmp_s1.io.lt), rvs2(63,32), Mux(is_opfvf, io.pipe(0).bits.frs1_data(31,0), rvs1(63,32)))
      }
    } 
    cmp_out.asTypeOf(UInt(64.W))
  }

  val sgnj = rvs2_vals.zip(rvs1_vals).map{ case(rvs2, rvs1) =>
    val rs1 = Mux(is_opfvf, io.pipe(0).bits.frs1_data, rvs1)
    val d_bit = Wire(Bool())
    val s_bit = Wire(Bool())

    when (ctrl_sgnjn) {
      d_bit := !rs1(63)
      s_bit := !rs1(31)
    } .elsewhen (ctrl_sgnjx) {
      d_bit := rs1(63) ^ rvs2(63)
      s_bit := rs1(31) ^ rvs2(31)
    } .otherwise {
      d_bit := !rs1(63)
      s_bit := !rs1(31)
    }
    Cat(d_bit,Cat(rvs1(62,32),Cat(Mux(io.pipe(0).bits.vd_eew === 3.U, rs1(31), s_bit), rvs1(30,0))))
  }

  val out = Wire(UInt(dLen.W))
  when (ctrl_cmp) {
    out := comparisons.asUInt
  } .otherwise {
    out := sgnj.asUInt 
  }

  io.write.valid := io.pipe(0).valid
  io.write.bits.eg := io.pipe(0).bits.wvd_eg >> 1
  io.write.bits.mask := Fill(2, FillInterleaved(8, io.pipe(0).bits.wmask))
  io.write.bits.data := Fill(2, out)
}
