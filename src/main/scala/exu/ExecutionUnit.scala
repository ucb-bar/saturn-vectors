package saturn.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import saturn.common._

class ExecutionUnit(genFUs: Seq[FunctionalUnitFactory], desc: String)(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  override def desiredName = s"ExecutionUnit$desc"

  val fus = genFUs.map(gen => Module(gen.generate(p)))
  val nFUs = fus.size

  val pipe_fus: Seq[(PipelinedFunctionalUnit, Int)] = fus.zipWithIndex.collect { case (f: PipelinedFunctionalUnit, i: Int) => (f, i) }
  val iter_fus: Seq[(IterativeFunctionalUnit, Int)] = fus.zipWithIndex.collect { case (f: IterativeFunctionalUnit, i: Int) => (f, i) }

  val maxPipeDepth = (pipe_fus.map(_._1.depth) :+ 0).max

  val io = IO(new Bundle {
    val iss = Flipped(Decoupled(new ExecuteMicroOpWithData(nFUs)))
    val iter_hazards = Output(Vec(iter_fus.size, Valid(new PipeHazard(maxPipeDepth))))
    val iter_write = Decoupled(new VectorWrite(dLen))
    val pipe_write = Output(Valid(new VectorWrite(dLen)))
    val acc_write = Output(Valid(new VectorWrite(dLen)))
    val scalar_write = Decoupled(new ScalarWrite)

    val pipe_hazards = Output(Vec(maxPipeDepth, Valid(new PipeHazard(maxPipeDepth))))

    val shared_fp_req = Decoupled(new FPInput())
    val shared_fp_resp = Flipped(Valid(new FPResult()))

    val set_vxsat = Output(Bool())
    val set_fflags = Output(Valid(UInt(5.W)))
    val busy = Output(Bool())
  })

  // Outer Product Unit
  val add_ope = desc == "int"
  val ope_write = Wire(new VectorWrite(dLen))
  val cnt = RegInit(0.U(32.W))

  ope_write := DontCare

  if (add_ope) {
    val ope_params = OPUParameters(8, 8, 32, vLen, dLen, 0.U, false.B)
    val opu = Module(new OuterProductUnit(ope_params))
    opu.io.en     := (cnt > (20*50).U) && (cnt % 101.U) === 0.U
    opu.io.acc    := true.B
    opu.io.msel   := 1.U
    opu.io.cfg_en := cnt < (16*50).U
    opu.io.in0    := io.iss.bits.rvs1_data
    opu.io.in1    := io.iss.bits.rvs2_data 

    opu.io.load   := (cnt > (20*50).U) && (cnt % 100.U) === 0.U
    opu.io.rd_out := (cnt > (20*50).U) && (cnt % 150.U) === 0.U
    
    ope_write.data := opu.io.out.asUInt
    ope_write.mask := 0.U
    ope_write.eg   := 3.U

    cnt := cnt+1.U
  }

  val sharedFPUnits = fus.collect { case fp: HasSharedFPUIO => fp }
  val hasSharedFPUnits = sharedFPUnits.size > 0

  io.shared_fp_req.valid := false.B
  io.shared_fp_req.bits := DontCare
  if (sharedFPUnits.size > 0) {
    val pipe_fp_units = sharedFPUnits.collect { case p: PipelinedFunctionalUnit with HasSharedFPUIO => p }
    val iter_fp_units = sharedFPUnits.collect { case i: IterativeFunctionalUnit with HasSharedFPUIO => i }
    require(pipe_fp_units.size <= 1 && iter_fp_units.size <= 1)
    val pu = pipe_fp_units.headOption
    val iu = iter_fp_units.headOption

    if (pu.isEmpty) {
      iu.foreach { iu => io.shared_fp_req <> iu.io_fp_req }
    } else {
      pu.map { pu =>
        io.shared_fp_req <> pu.io_fp_req
        iu.foreach { iu =>
          val use_iu = !pu.io.pipe.map(_.valid).orR && iu.io.busy
          val use_pu = !iu.io.busy
          io.shared_fp_req.valid := (use_iu && iu.io_fp_req.valid) || (use_pu && pu.io_fp_req.valid)
          io.shared_fp_req.bits := Mux1H(Seq(
            use_iu -> iu.io_fp_req.bits,
            use_pu -> pu.io_fp_req.bits
          ))
          iu.io_fp_req.ready := use_iu && io.shared_fp_req.ready
          pu.io_fp_req.ready := use_pu && io.shared_fp_req.ready
        }
      }
    }
    sharedFPUnits.foreach(_.io_fp_resp := io.shared_fp_resp)
  }

  val stalls = fus.map(_.io.stall)
  when (io.iss.valid) { assert(PopCount(io.iss.bits.fu_sel) <= 1.U) }
  io.iss.ready := !Mux1H(io.iss.bits.fu_sel, stalls)
  fus.zipWithIndex.foreach { case (fu,i) =>
    fu.io.iss.op := io.iss.bits
    fu.io.iss.valid := io.iss.valid && io.iss.bits.fu_sel(i) && !fu.io.stall
  }

  val pipe_write = WireInit(false.B)

  io.pipe_write.valid := false.B
  io.pipe_write.bits := DontCare
  io.iter_write.valid := false.B
  io.iter_write.bits := DontCare
  io.acc_write.valid := false.B
  io.acc_write.bits := DontCare
  io.busy := false.B
  io.set_vxsat := fus.map(_.io.set_vxsat).orR
  io.set_fflags.valid := fus.map(_.io.set_fflags.valid).orR
  io.set_fflags.bits := fus.map(f => Mux(f.io.set_fflags.valid, f.io.set_fflags.bits, 0.U)).reduce(_|_)

  val scalar_write_arb = Module(new Arbiter(new ScalarWrite, fus.size))
  scalar_write_arb.io.in.zip(fus.map(_.io.scalar_write)).foreach { case (l, r) => l <> r }
  io.scalar_write <> scalar_write_arb.io.out

  if (pipe_fus.size > 0) {
    val pipe_valids    = Seq.fill(maxPipeDepth)(RegInit(false.B))
    val pipe_bits      = Seq.fill(maxPipeDepth)(Reg(new ExecuteMicroOpWithData(nFUs)))

    val pipe_iss = io.iss.fire && pipe_fus.map(t => io.iss.bits.fu_sel(t._2)).orR
    pipe_valids.head := pipe_iss
    when (pipe_iss) {
      pipe_bits.head      := io.iss.bits
    }

    for (i <- 1 until maxPipeDepth) {
      val fire = pipe_valids(i-1)
      pipe_valids(i) := fire && pipe_bits(i-1).pipe_depth =/= (i-1).U
      when (fire) {
        pipe_bits(i)      := pipe_bits(i-1)
      }
    }
    for ((fu, j) <- pipe_fus) {
      for (i <- 0 until fu.depth) {
        fu.io.pipe(i).valid := pipe_valids(i) && pipe_bits(i).fu_sel(j)
        fu.io.pipe(i).bits  := Mux(pipe_valids(i) && pipe_bits(i).fu_sel(j),
          pipe_bits(i), 0.U.asTypeOf(new ExecuteMicroOpWithData(nFUs)))
      }
    }

    // Selects which pipe register has the write
    val write_pipe_sel = pipe_valids.zip(pipe_bits).zipWithIndex.map { case ((v,b),i) =>
      v && b.pipe_depth === i.U
    }
    // Selects which of all FUs has the write
    val write_fu_sel = pipe_fus.map { case (_, j) => Mux1H(write_pipe_sel, pipe_bits.map(_.fu_sel))(j) }
    assert(PopCount(write_pipe_sel) <= 1.U)
    pipe_write := write_pipe_sel.orR
    when (write_pipe_sel.orR) {
      assert(PopCount(write_fu_sel) <= 1.U)
      val acc = Mux1H(write_pipe_sel, pipe_bits.map(_.acc))
      val tail = Mux1H(write_pipe_sel, pipe_bits.map(_.tail))
      io.pipe_write.valid := Mux1H(write_fu_sel, pipe_fus.map(_._1.io.write.valid)) && (!acc || tail)
      io.pipe_write.bits := Mux1H(write_fu_sel, pipe_fus.map(_._1.io.write.bits))
      if (add_ope) {
        io.pipe_write.bits := Mux(cnt === 3.U, ope_write, Mux1H(write_fu_sel, pipe_fus.map(_._1.io.write.bits))) // KA Added
      } else {
        io.pipe_write.bits := Mux1H(write_fu_sel, pipe_fus.map(_._1.io.write.bits))
      }
      io.acc_write.valid := acc && !tail
      io.acc_write.bits := Mux1H(write_fu_sel, pipe_fus.map(_._1.io.write.bits))
    }

    when (pipe_valids.orR) { io.busy := true.B }
    for (i <- 0 until maxPipeDepth) {
      io.pipe_hazards(i).valid       := pipe_valids(i)
      io.pipe_hazards(i).bits.vat    := pipe_bits(i).vat
      io.pipe_hazards(i).bits.eg     := pipe_bits(i).wvd_eg
    }

    // Compress instructions need special handling
    val permFU = pipe_fus.map(_._1).collect { case f: PermuteUnit => f }.headOption.foreach { fu =>
      when (fu.io.pipe(0).valid) {
        io.pipe_hazards(0).bits.eg := fu.io.write.bits.eg
      }
    }
  }

  if (iter_fus.size > 0) {
    val iter_write_arb = Module(new Arbiter(new VectorWrite(dLen), iter_fus.size))
    iter_write_arb.io.in.zip(iter_fus.map(_._1.io.write)).foreach { case (l,r) => l <> r }
    iter_write_arb.io.out.ready := !pipe_write && io.iter_write.ready

    val acc = Mux1H(iter_write_arb.io.in.map(_.fire), iter_fus.map(_._1.io.acc))
    val tail = Mux1H(iter_write_arb.io.in.map(_.fire), iter_fus.map(_._1.io.tail))
    io.iter_write.valid     := iter_write_arb.io.out.valid && (!acc || tail) && !pipe_write
    io.iter_write.bits.eg   := iter_write_arb.io.out.bits.eg
    io.iter_write.bits.mask := iter_write_arb.io.out.bits.mask
    io.iter_write.bits.data := iter_write_arb.io.out.bits.data
    when (!pipe_write) {
      io.acc_write.valid := iter_write_arb.io.out.valid && acc
      io.acc_write.bits.eg   := Mux1H(iter_write_arb.io.in.map(_.fire), iter_fus.map(_._1.io.write.bits.eg))
      io.acc_write.bits.data := Mux1H(iter_write_arb.io.in.map(_.fire), iter_fus.map(_._1.io.write.bits.data))
      io.acc_write.bits.mask := Mux1H(iter_write_arb.io.in.map(_.fire), iter_fus.map(_._1.io.write.bits.mask))
    }
    when (iter_fus.map(_._1.io.busy).orR) { io.busy := true.B }
    for (i <- 0 until iter_fus.size) {
      io.iter_hazards(i) := iter_fus(i)._1.io.hazard
    }
  }


}
