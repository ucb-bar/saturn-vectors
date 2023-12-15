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
    (OPFFunct6.vfmin    ,   Seq(Y,Y, N,N,N, N,X,X)),
    (OPFFunct6.vfmax    ,   Seq(Y,N, N,N,N, N,X,X)),
    (OPFFunct6.vfsgnj   ,   Seq(N,X, N,N,N, Y,N,N)), 
    (OPFFunct6.vfsgnjn  ,   Seq(N,X, N,N,N, Y,Y,N)), 
    (OPFFunct6.vfsgnjx  ,   Seq(N,X, N,N,N, Y,N,Y)), 
    (OPFFunct6.vmfeq    ,   Seq(N,X, Y,Y,N, N,X,X)),
    (OPFFunct6.vmfne    ,   Seq(N,X, Y,N,N, N,X,X)),
    (OPFFunct6.vmflt    ,   Seq(N,X, Y,N,Y, N,X,X)),
    (OPFFunct6.vmfle    ,   Seq(N,X, Y,Y,Y, N,X,X)),
    (OPFFunct6.vmfgt    ,   Seq(N,X, Y,N,N, N,X,X)),
    (OPFFunct6.vmfge    ,   Seq(N,X, Y,Y,N, N,X,X)),
    (OPFFunct6.vfmerge  ,   Seq(N,X, N,X,X, N,X,X)),
  )
  override def accepts(f3: UInt, f6: UInt): Bool = VecDecode(f3, f6, ctrl_table.map(_._1))

  val ctrl_cmp :: ctrl_min :: ctrl_mask_write :: ctrl_eq :: ctrl_cmp_less :: ctrl_sgnj :: ctrl_sgnjn :: ctrl_sgnjx :: Nil = VecDecode.applyBools(
    io.pipe(0).bits.funct3, io.pipe(0).bits.funct6,
    Seq.fill(8)(X), ctrl_table
  ) 
  
  val is_opfvf = io.pipe(0).bits.funct3.isOneOf(OPFVF)

  val rvs1_eew = io.pipe(0).bits.rvs1_eew
  val rvs2_eew = io.pipe(0).bits.rvs2_eew
  val rvd_eew  = io.pipe(0).bits.rvd_eew
  val rvs2_data = io.pipe(0).bits.rvs2_data
  val rvs1_data = io.pipe(0).bits.rvs1_data
  val frs1_data = io.pipe(0).bits.frs1_data

  val ctrl_merge = io.pipe(0).bits.opff6 === OPFFunct6.vfmerge

  val fTypes = Seq(FType.S, FType.D)
  val minmax_results = Wire(Vec(2, UInt(dLen.W)))       // results for vfmin/vfmax
  val comp_results = Wire(Vec(2, UInt((dLen / 32).W)))  // results for comparisons

  for (eew <- 2 until 4) {
    val fType = fTypes(eew-2)
    val num_chunks = dLen / fType.ieeeWidth
    val compare_modules = Seq.fill(num_chunks)(Module(new hardfloat.CompareRecFN(fType.exp, fType.sig))) 

    val zip_compares = rvs2_data.asTypeOf(Vec(num_chunks, UInt(fType.ieeeWidth.W))).zip(rvs1_data.asTypeOf(Vec(num_chunks, UInt(fType.ieeeWidth.W)))).zip(compare_modules)

    val gen_compares = zip_compares.map { case((rvs2, rvs1), comp) =>
      val rs1 = Mux(is_opfvf, frs1_data(fType.ieeeWidth-1,0), rvs1)

      val rvs2_rec = fType.recode(rvs2)
      val rs1_rec = fType.recode(rs1)
      val rvs2_nan = fType.isNaN(rvs2_rec) 
      val rs1_nan = fType.isNaN(rs1_rec)

      comp.io.signaling := true.B
      comp.io.a := rvs2_rec
      comp.io.b := rs1_rec
      (comp.io, rvs2, rvs2_nan, rs1, rs1_nan)
    }

    val minmax = gen_compares.map{ case(comp, rvs2, rvs2_nan, rs1, rs1_nan) => 
      val minmax_out = Wire(UInt(fType.ieeeWidth.W))
      when(rvs2_nan && rs1_nan) {
        minmax_out := fType.ieeeQNaN
      } .elsewhen (rvs2_nan) { 
        minmax_out := rs1
      } .elsewhen (rs1_nan) {
        minmax_out := rvs2
      } .otherwise {
        minmax_out := Mux((!ctrl_min && comp.gt) || (ctrl_min && comp.lt), rvs2, rs1)
      }
      minmax_out
    }    
    minmax_results(eew - 2) := minmax.asUInt

    val comparisons = gen_compares.map{ case(comp, rvs2, rvs2_man, rs1, rs1_nan) =>
      val comparison_out = Wire(UInt(1.W))
      when (ctrl_eq) {
        comparison_out := comp.eq
      } .elsewhen (ctrl_cmp_less) {
        comparison_out := comp.lt
      } .otherwise {   
        comparison_out := comp.gt
      }
      comparison_out
    } 
    comp_results(eew-2) := Fill(eew + 1, comparisons.asUInt)
  }

  val rvs1_vals = io.pipe(0).bits.rvs1_data.asTypeOf(Vec(fmaCount, UInt(64.W)))
  val rvs2_vals = io.pipe(0).bits.rvs2_data.asTypeOf(Vec(fmaCount, UInt(64.W)))

  // Sign-Injection Instructions
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
      d_bit := rs1(63)
      s_bit := rs1(31)
    }
    Cat(d_bit,Cat(rvs2(62,32),Cat(Mux(io.pipe(0).bits.rvd_eew === 3.U, rvs2(31), s_bit), rvs2(30,0))))
  }

  // FP Merge Instruction
  val rvs2_elems = io.pipe(0).bits.rvs2_data.asTypeOf(Vec(dLenB/4, UInt(32.W)))
  val frs1_elems = dLenSplat(Mux(rvs2_eew === 3.U, frs1_data, Fill(2, frs1_data(31,0))), 3.U).asTypeOf(Vec(dLenB/4, UInt(32.W)))
  //val frs1_elems = dLenSplat(frs1_data, rvs2_eew)
  //val fs1_elems = Mux(vs2_eew === 3.U, dLenSplat(frs1_data, 3.U), )

  //val merge_mask = VecInit.tabulate(2)({eew => FillInterleaved(1 << (eew+2), io.pipe(0).bits.rmask(((dLenB >> (eew+2))-1,0)))})(rvs2_eew - 2.U)
  val merge_mask = VecInit.tabulate(4)({eew => FillInterleaved(1 << eew, io.pipe(0).bits.rmask((dLenB >> eew)-1,0))})(rvs2_eew)
  val merge_out = VecInit((0 until (dLenB / 4)).map {i => Mux(merge_mask(i), frs1_elems(i), rvs2_elems(i))}).asUInt

  val out = Wire(UInt(dLen.W))
  when (ctrl_merge) {
    out := merge_out
  } .elsewhen (ctrl_cmp) {
    out := Mux(rvs1_eew === 3.U, minmax_results(1), minmax_results(0)) 
  } .elsewhen (ctrl_mask_write) {
    out := Fill(64, Mux(rvs1_eew === 3.U, comp_results(1), comp_results(0)))
  } .otherwise {
    out := sgnj.asUInt 
  }

  // Mask writing
  val mask_write_offset = VecInit.tabulate(4)({ eew =>
    Cat(io.pipe(0).bits.eidx(log2Ceil(dLen)-1, dLenOffBits-eew), 0.U((dLenOffBits-eew).W))
  })(rvs1_eew)
  val mask_write_mask = (VecInit.tabulate(4)({ eew =>
    VecInit(io.pipe(0).bits.wmask.asBools.grouped(1 << eew).map(_.head).toSeq).asUInt
  })(rvs1_eew) << mask_write_offset)(dLen-1,0)

  io.write.valid := io.pipe(0).valid
  io.write.bits.eg := io.pipe(0).bits.wvd_eg >> 1
  io.write.bits.mask := Fill(2, Mux(ctrl_mask_write, mask_write_mask, FillInterleaved(8, io.pipe(0).bits.wmask)))
  io.write.bits.data := Fill(2, out)
}
