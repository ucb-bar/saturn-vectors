package vector.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import vector.common._

class FPCompPipe(implicit p: Parameters) extends PipelinedFunctionalUnit(1)(p) with HasFPUParameters {
  io.iss.sub_dlen := 0.U
  io.set_vxsat := false.B

  lazy val ctrl_table = Seq(
    (OPFFunct6.fmin     ,   Seq(Y,Y, N,X,X,X,X, N,X,X)),
    (OPFFunct6.fmax     ,   Seq(Y,N, N,X,X,X,X, N,X,X)),
    (OPFFunct6.fsgnj    ,   Seq(N,X, N,X,X,X,X, Y,N,N)),
    (OPFFunct6.fsgnjn   ,   Seq(N,X, N,X,X,X,X, Y,Y,N)),
    (OPFFunct6.fsgnjx   ,   Seq(N,X, N,X,X,X,X, Y,N,Y)),
    (OPFFunct6.mfeq     ,   Seq(N,X, Y,Y,N,N,N, N,X,X)),
    (OPFFunct6.mfne     ,   Seq(N,X, Y,N,Y,N,N, N,X,X)),
    (OPFFunct6.mflt     ,   Seq(N,X, Y,N,N,Y,N, N,X,X)),
    (OPFFunct6.mfle     ,   Seq(N,X, Y,Y,N,Y,N, N,X,X)),
    (OPFFunct6.mfgt     ,   Seq(N,X, Y,N,N,N,Y, N,X,X)),
    (OPFFunct6.mfge     ,   Seq(N,X, Y,Y,N,N,Y, N,X,X)),
    (OPFFunct6.fredmin  ,   Seq(Y,Y, N,X,X,X,X, N,X,X)),
    (OPFFunct6.fredmax  ,   Seq(Y,N, N,X,X,X,X, N,X,X)),
  )
  def accepts(f3: UInt, f6: UInt): Bool = VecDecode(f3, f6, ctrl_table.map(_._1))
  io.iss.ready := accepts(io.iss.op.funct3, io.iss.op.funct6)

  val ctrl_cmp :: ctrl_min :: ctrl_mask_write :: ctrl_eq :: ctrl_ne :: ctrl_lt :: ctrl_gt :: ctrl_sgnj :: ctrl_sgnjn :: ctrl_sgnjx :: Nil = VecDecode.applyBools(
    io.pipe(0).bits.funct3, io.pipe(0).bits.funct6,
    Seq.fill(10)(X), ctrl_table
  )

  val rvs1_eew = io.pipe(0).bits.rvs1_eew
  val rvs2_eew = io.pipe(0).bits.rvs2_eew
  val rvd_eew  = io.pipe(0).bits.rvd_eew
  val rvs2_data = io.pipe(0).bits.rvs2_data
  val rvs1_data = io.pipe(0).bits.rvs1_data

  val fTypes = Seq(FType.S, FType.D)
  val minmax_results = Wire(Vec(2, UInt(dLen.W)))       // results for vfmin/vfmax
  val comp_results_eew = Seq.tabulate(4)({sew => WireInit(0.U((dLenB >> sew).W))})
  val comp_results = (0 until 4).map({sew => Mux(rvd_eew === sew.U, Fill(1 << sew, comp_results_eew(sew)), 0.U(dLenB.W))}).reduce(_|_)
  val exceptions = Wire(Vec(2, UInt(5.W)))

  for (eew <- 2 until 4) {
    val fType = fTypes(eew-2)
    val num_chunks = dLen / fType.ieeeWidth
    val compare_modules = Seq.fill(num_chunks)(Module(new hardfloat.CompareRecFN(fType.exp, fType.sig)))

    val zip_compares = rvs2_data.asTypeOf(Vec(num_chunks, UInt(fType.ieeeWidth.W))).zip(rvs1_data.asTypeOf(Vec(num_chunks, UInt(fType.ieeeWidth.W)))).zip(compare_modules)

    val gen_compares = zip_compares.map { case((rvs2, rvs1), comp) =>
      val rvs2_rec = fType.recode(rvs2)
      val rvs1_rec = fType.recode(rvs1)
      val rvs2_nan = fType.isNaN(rvs2_rec)
      val rvs1_nan = fType.isNaN(rvs1_rec)

      comp.io.signaling := true.B
      comp.io.a := rvs2_rec
      comp.io.b := rvs1_rec
      (comp.io, rvs2, rvs2_nan, rvs1, rvs1_nan)
    }

    val minmax = gen_compares.map{ case(comp, rvs2, rvs2_nan, rvs1, rvs1_nan) =>
      val minmax_out = Wire(UInt(fType.ieeeWidth.W))
      when(rvs2_nan && rvs1_nan) {
        minmax_out := fType.ieeeQNaN
      } .elsewhen (rvs2_nan) {
        minmax_out := rvs1
      } .elsewhen (rvs1_nan) {
        minmax_out := rvs2
      } .otherwise {
        minmax_out := Mux((!ctrl_min && comp.gt) || (ctrl_min && comp.lt), rvs2, rvs1)
      }
      minmax_out
    }
    minmax_results(eew - 2) := minmax.asUInt

    val comparisons = gen_compares.map{ case(comp, rvs2, rvs2_nan, rvs1, rvs1_nan) =>
      val comparison_out = Wire(UInt(1.W))
      when (ctrl_ne) {
        when (rvs2_nan || rvs1_nan) {
          comparison_out := 1.U
        } .otherwise {
          comparison_out := !comp.eq
        }
      } .elsewhen (rvs2_nan || rvs1_nan) {
        comparison_out := 0.U
      } .otherwise {
        comparison_out := (comp.eq && ctrl_eq) || (comp.lt && ctrl_lt) || (comp.gt && ctrl_gt)
      }
      comparison_out
    }
    comp_results_eew(eew) := comparisons.asUInt

    exceptions(eew - 2) := gen_compares.map {case(comp, rvs2, rvs2_nan, rvs1, rvs1_nan) => comp.exceptionFlags}.reduce(_ | _)
  }


  val rvs1_vals = io.pipe(0).bits.rvs1_data.asTypeOf(Vec(dLenB / 8, UInt(64.W)))
  val rvs2_vals = io.pipe(0).bits.rvs2_data.asTypeOf(Vec(dLenB / 8, UInt(64.W)))

  // Sign-Injection Instructions
  val sgnj = rvs2_vals.zip(rvs1_vals).map{ case(rvs2, rvs1) =>
    val d_bit = Wire(Bool())
    val s_bit = Wire(Bool())

    when (ctrl_sgnjn) {
      d_bit := !rvs1(63)
      s_bit := !rvs1(31)
    } .elsewhen (ctrl_sgnjx) {
      d_bit := rvs1(63) ^ rvs2(63)
      s_bit := rvs1(31) ^ rvs2(31)
    } .otherwise {
      d_bit := rvs1(63)
      s_bit := rvs1(31)
    }
    Cat(d_bit,Cat(rvs2(62,32),Cat(Mux(io.pipe(0).bits.rvd_eew === 3.U, rvs2(31), s_bit), rvs2(30,0))))
  }

  val out = Wire(UInt(dLen.W))
  when (ctrl_cmp) {
    out := Mux(rvs1_eew === 3.U, minmax_results(1), minmax_results(0))
  } .elsewhen (ctrl_mask_write) {
    out := Fill(8, comp_results)
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
  io.write.bits.eg := io.pipe(0).bits.wvd_eg
  io.write.bits.mask := Mux(ctrl_mask_write, mask_write_mask, FillInterleaved(8, io.pipe(0).bits.wmask))
  io.write.bits.data := out

  io.set_fflags.valid := io.write.valid
  io.set_fflags.bits := Mux(rvd_eew === 3.U, exceptions(1), exceptions(0))
  io.scalar_write.valid := false.B
  io.scalar_write.bits := DontCare
}
