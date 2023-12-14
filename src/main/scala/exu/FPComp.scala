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
    (OPFFunct6.vfmax    ,   Seq(N,Y, N,N,N, N,X,X)),
    (OPFFunct6.vfsgnj   ,   Seq(X,X, N,N,N, Y,N,N)), 
    (OPFFunct6.vfsgnjn  ,   Seq(X,X, N,N,N, Y,Y,N)), 
    (OPFFunct6.vfsgnjx  ,   Seq(X,X, N,N,N, Y,N,Y)), 
    (OPFFunct6.vmfeq    ,   Seq(X,X, Y,Y,N, N,X,X)),
    (OPFFunct6.vmfne    ,   Seq(X,X, Y,N,N, N,X,X)),
    (OPFFunct6.vmflt    ,   Seq(X,X, Y,N,Y, N,X,X)),
    (OPFFunct6.vmfle    ,   Seq(X,X, Y,Y,Y, N,X,X)),
    (OPFFunct6.vmfgt    ,   Seq(X,X, Y,N,N, N,X,X)),
    (OPFFunct6.vmfge    ,   Seq(X,X, Y,Y,N, N,X,X)),
  )
  override def accepts(f3: UInt, f6: UInt): Bool = VecDecode(f3, f6, ctrl_table.map(_._1))

  val ctrl_min :: ctrl_cmp :: ctrl_mask_write :: ctrl_eq :: ctrl_cmp_less :: ctrl_sgnj :: ctrl_sgnjn :: ctrl_sgnjx :: Nil = VecDecode.applyBools(
    io.pipe(0).bits.funct3, io.pipe(0).bits.funct6,
    Seq.fill(8)(X), ctrl_table
  ) 
  
  val is_opfvf = io.pipe(0).bits.funct3.isOneOf(OPFVF)

  // Helpers
  val vs1_eew = io.pipe(0).bits.rvs1_eew
  val rvs2_data = io.pipe(0).bits.rvs2_data
  val rvs1_data = io.pipe(0).bits.rvs1_data
  val frs1_data = io.pipe(0).bits.frs1_data


  // New implementation experiment

  val fTypes = Seq(FType.S, FType.D)
  val minmax_results = Wire(Vec(2, UInt(dLen.W))) // results for vfmin/vfmax
  //val comp_results = Wire(Vec(2, Bool()))         // results for the comparison mask instructions

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
      // NaN checking, etc.
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
      //when(fType.isNaN(rvs2) && fType.isNaN(rs1)) {
      //  minmax_out := fType.ieeeQNaN
      //} .elsewhen (fType.isNaN(rvs2)) { 
      //  minmax_out := rs1
      //} .elsewhen (fType.isNaN(rs1)) {
      //  minmax_out := rvs2
      //} .otherwise {
      //  minmax_out := Mux((!ctrl_min && comp.gt) || (ctrl_min && comp.lt), rvs2, rs1)
      //}
      minmax_out
    }    

    // the issue is that this doesn't account for a full dLen width
    // I need to collect all the results from the eew chunks and assign them
    //minmax_results(eew.U - 2.U) := minmax.asUInt
    minmax_results(eew - 2) := minmax.asUInt
  }

  //val results = comp_results(vs1_eew - 2.U) 
  //val comparisons = VecInit.tabulate(2)({ eew => })


  ////////////////////////////////


  // Old implementation
  val rvs1_vals = io.pipe(0).bits.rvs1_data.asTypeOf(Vec(fmaCount, UInt(64.W)))
  val rvs2_vals = io.pipe(0).bits.rvs2_data.asTypeOf(Vec(fmaCount, UInt(64.W)))

  //val compare_mask_bits_s = Wire(Vec(dLenB/4, Bool()))
  //val compare_mask_bits_d = Wire(Vec(dLenB/8, Bool()))

  //val comparisons = rvs2_vals.zip(rvs1_vals).zipWithIndex.map{ case((rvs2, rvs1), i) =>
  //  val cmp_d = Module(new hardfloat.CompareRecFN(11, 53))
  //  val cmp_s0 = Module(new hardfloat.CompareRecFN(8, 24))
  //  val cmp_s1 = Module(new hardfloat.CompareRecFN(8, 24))

  //  cmp_d.io.signaling := true.B
  //  cmp_s0.io.signaling := true.B
  //  cmp_s1.io.signaling := true.B

  //  val rvs2_d = FType.D.recode(rvs2)
  //  val rvs1_d = FType.D.recode(Mux(is_opfvf, io.pipe(0).bits.frs1_data, rvs1))
  //  val rvs2_s0 = FType.S.recode(rvs2(31,0))
  //  val rvs1_s0 = FType.S.recode(Mux(is_opfvf, io.pipe(0).bits.frs1_data(31,0), rvs1(31,0)))
  //  val rvs2_s1 = FType.S.recode(rvs2(63,32))
  //  val rvs1_s1 = FType.S.recode(Mux(is_opfvf, io.pipe(0).bits.frs1_data(31,0), rvs1(63,32)))

  //  val NaNs = Seq(FType.D.isNaN(rvs2_d), FType.D.isNaN(rvs1_d), FType.S.isNaN(rvs2_s0), FType.S.isNaN(rvs1_s0), FType.S.isNaN(rvs2_s1), FType.S.isNaN(rvs1_s1))
  //  
  //  cmp_d.io.a := rvs2_d
  //  cmp_d.io.b := rvs1_d
  //  cmp_s0.io.a := rvs2_s0 
  //  cmp_s0.io.b := rvs1_s0
  //  cmp_s1.io.a := rvs2_s1
  //  cmp_s1.io.b := rvs1_s1

  //  val cmp_out = Wire(Vec(2, UInt(32.W)))
  //  when (io.pipe(0).bits.vd_eew === 3.U) {
  //    when (NaNs(0) && NaNs(1)) {
  //      cmp_out(0) := 0.U
  //      cmp_out(1) := "h7ff80000".U
  //    } .elsewhen (NaNs(0)) {
  //      cmp_out(0) := Mux(is_opfvf, io.pipe(0).bits.frs1_data(31,0), rvs1(31,0))
  //      cmp_out(1) := Mux(is_opfvf, io.pipe(0).bits.frs1_data(63,32), rvs1(63,32))
  //    } .elsewhen (NaNs(1)) {
  //      cmp_out(0) := rvs2(31,0)
  //      cmp_out(1) := rvs2(63,32)
  //    } .otherwise {
  //      cmp_out(0) := Mux((!ctrl_min && cmp_d.io.gt) || (ctrl_min && cmp_d.io.lt), rvs2(31,0), Mux(is_opfvf, io.pipe(0).bits.frs1_data(31,0), rvs1(31,0)))
  //      cmp_out(1) := Mux((!ctrl_min && cmp_d.io.gt) || (ctrl_min && cmp_d.io.lt), rvs2(63,32), Mux(is_opfvf, io.pipe(0).bits.frs1_data(63,32), rvs1(63,32)))
  //    }
  //  } .otherwise {
  //    when (NaNs(2) && NaNs(3)) {
  //      cmp_out(0) := "h7fc00000".U
  //    } .elsewhen (NaNs(2)) {
  //      cmp_out(0) := Mux(is_opfvf, io.pipe(0).bits.frs1_data(31,0), rvs1(31,0))
  //    } .elsewhen (NaNs(3)) {
  //      cmp_out(0) := rvs2(31,0)
  //    } .otherwise {
  //      cmp_out(0) := Mux((!ctrl_min && cmp_s0.io.gt) || (ctrl_min && cmp_s0.io.lt), rvs2(31,0), Mux(is_opfvf, io.pipe(0).bits.frs1_data(31,0), rvs1(31,0))) 
  //    }
  //    when (NaNs(4) && NaNs(5)) {
  //      cmp_out(1) := "h7fc00000".U
  //    } .elsewhen (NaNs(4)) {
  //      cmp_out(1) := Mux(is_opfvf, io.pipe(0).bits.frs1_data(31,0), rvs1(63,32))
  //    } .elsewhen (NaNs(5)) {
  //      cmp_out(1) := rvs2(63,32)
  //    } .otherwise {
  //      cmp_out(1) := Mux((!ctrl_min && cmp_s1.io.gt) || (ctrl_min && cmp_s1.io.lt), rvs2(63,32), Mux(is_opfvf, io.pipe(0).bits.frs1_data(31,0), rvs1(63,32)))
  //    }
  //  } 

  //  val d_mask = Wire(Bool())
  //  val s_mask = Wire(UInt(2.W))
  //  // Mask generating instructions
  //  when (ctrl_eq) {
  //    d_mask := cmp_d.io.eq
  //    s_mask := Cat(cmp_s1.io.eq, cmp_s0.io.eq)
  //  } .elsewhen (ctrl_cmp_less) {
  //    d_mask := cmp_d.io.lt
  //    s_mask := Cat(cmp_s1.io.lt, cmp_s0.io.lt)
  //  } .otherwise {
  //    d_mask := cmp_d.io.gt
  //    s_mask := Cat(cmp_s1.io.gt, cmp_s0.io.gt)
  //  }

  //  compare_mask_bits_d(i) := d_mask
  //  compare_mask_bits_s(2*i) := s_mask(0)
  //  compare_mask_bits_s((2*i)+1) := s_mask(1)
  //  
  //  cmp_out.asTypeOf(UInt(64.W))
  //}

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
    Cat(d_bit,Cat(rvs2(62,32),Cat(Mux(io.pipe(0).bits.vd_eew === 3.U, rvs2(31), s_bit), rvs2(30,0))))
  }

  val out = Wire(UInt(dLen.W))
  when (ctrl_cmp) {
    out := Mux(vs1_eew === 3.U, minmax_results(1), minmax_results(0)) 
  } .otherwise {
    out := sgnj.asUInt 
  }

  //val out = Wire(UInt(dLen.W))
  //when (ctrl_cmp) {
  //  out := comparisons.map(_(0)).asUInt
  //} .elsewhen (ctrl_mask_write) {
  //  //out := comparisons.map(_(1)).asUInt
  //  out := Mux(io.pipe(0).bits.vd_eew === 3.U, Fill(64, compare_mask_bits_d.asUInt), Fill(32, compare_mask_bits_s.asUInt))
  //} .otherwise {
  //  out := sgnj.asUInt 
  //}

  // Mask writing
  //val mask_write_offset = VecInit.tabulate(4)({ eew =>
  //  Cat(io.pipe(0).bits.eidx(log2Ceil(dLen)-1, dLenOffBits-eew), 0.U((dLenOffBits-eew).W))
  //})(io.pipe(0).bits.rvs1_eew)
  //val mask_write_mask = (VecInit.tabulate(4)({ eew =>
  //  VecInit(io.pipe(0).bits.wmask.asBools.grouped(1 << eew).map(_.head).toSeq).asUInt
  //})(io.pipe(0).bits.rvs1_eew) << mask_write_offset)(dLen-1,0)

  io.write.valid := io.pipe(0).valid
  io.write.bits.eg := io.pipe(0).bits.wvd_eg >> 1
  io.write.bits.mask := Fill(2, FillInterleaved(8, io.pipe(0).bits.wmask))
  //io.write.bits.mask := Fill(2, Mux(ctrl_mask_write, mask_write_mask, FillInterleaved(8, io.pipe(0).bits.wmask)))
  io.write.bits.data := Fill(2, out)
}
