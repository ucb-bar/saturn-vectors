package saturn.frontend

import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tile._
import freechips.rocketchip.util._

import saturn.common._
import saturn.insns._

class VectorDispatcher(implicit p: Parameters) extends CoreModule()(p) with HasVectorParams {
  val io = IO(new Bundle {
    val issue = Flipped(Decoupled(new VectorIssueInst))

    val mem = Decoupled(new VectorMemMacroOp)
    val dis = Decoupled(new VectorIssueInst)

    val scalar_resp = Decoupled(new ScalarWrite)

    val vat_release = Input(Vec(nRelease, Valid(UInt(vParams.vatSz.W))))
    val vat_head = Output(UInt(vParams.vatSz.W))
    val vat_tail = Output(UInt(vParams.vatSz.W))
  })

  val debug_id_ctr = RegInit(0.U(debugIdSz.W))
  val vat_valids = RegInit(VecInit.fill(1 << vParams.vatSz)(false.B))
  val vat_tail = RegInit(0.U(vParams.vatSz.W))
  val vat_head = RegInit(0.U(vParams.vatSz.W))
  def vatOlder(i0: UInt, i1: UInt) = cqOlder(i0, i1, vat_tail)
  val vat_available = !vat_valids(vat_tail)
  val vat_available_count = PopCount(~vat_valids.asUInt)
  val vat_head_incr = WireInit(false.B)

  when (vat_head_incr) {
    vat_head := vat_head + 1.U
  }

  when (io.dis.fire) {
    assert(!vat_valids(vat_tail))
    vat_valids(vat_tail) := true.B
    vat_tail := vat_tail + 1.U
    debug_id_ctr := debug_id_ctr + 1.U
  }

  when (vat_tail =/= vat_head && !vat_valids(vat_head)) {
    vat_head_incr := true.B
  }

  val issue_inst = WireInit(io.issue.bits)
  issue_inst.vat := vat_tail
  issue_inst.debug_id := debug_id_ctr

  val hwacha_limiter = vParams.hwachaLimiter.map(n => Module(new HwachaLimiter(n)))
  hwacha_limiter.foreach { h =>
    h.io.inst := issue_inst
    h.io.fire := io.issue.fire
    h.io.vat_release.foreach(_ := false.B)
  }
  val hwacha_block = hwacha_limiter.map(_.io.block).getOrElse(false.B)


  io.issue.ready := vat_available && io.dis.ready   && (!issue_inst.vmu || io.mem.ready) && !hwacha_block
  io.dis.valid   := vat_available && io.issue.valid && (!issue_inst.vmu || io.mem.ready) && !hwacha_block
  io.mem.valid   := vat_available && io.issue.valid && io.dis.ready && issue_inst.vmu    && !hwacha_block

  io.vat_tail := vat_tail
  io.vat_head := vat_head

  val move_to_scalar = (
    (io.issue.bits.funct3 === OPMVV && io.issue.bits.opmf6 === OPMFunct6.wrxunary0 && io.issue.bits.rs1 === 0.U) ||
    (io.issue.bits.funct3 === OPFVV && io.issue.bits.opff6 === OPFFunct6.wrfunary0 && io.issue.bits.rs1 === 0.U)
  )
  when (move_to_scalar) {
    issue_inst.vconfig.vl := 1.U
    issue_inst.vstart := 0.U
  }

  // This resolves a false critical path from PTC fissioning of VL to the scalar-resp VL check
  when (io.issue.bits.fission_vl.valid) {
    issue_inst.vconfig.vl := io.issue.bits.fission_vl.bits
  }

  // Strided with stride = 1 << eew is just unit-strided
  when (io.issue.bits.mop === mopStrided && io.issue.bits.rs2_data === ((io.issue.bits.nf +& 1.U) << io.issue.bits.mem_elem_size)) {
    issue_inst.mop := mopUnit
  }

  io.scalar_resp.valid := false.B
  io.scalar_resp.bits.fp := false.B
  io.scalar_resp.bits.rd := io.issue.bits.rd
  io.scalar_resp.bits.size := 3.U
  io.scalar_resp.bits.data := Mux(io.issue.bits.rs1(0),
    ~(0.U(xLen.W)), // vfirst
    0.U // vpopc
  )

  when (io.issue.bits.vconfig.vl <= issue_inst.vstart && !(issue_inst.funct3 === OPIVI && issue_inst.opif6 === OPIFunct6.mvnrr) && !move_to_scalar) {
    io.issue.ready := true.B
    io.mem.valid := false.B
    io.dis.valid := false.B
    when (io.issue.bits.funct3 === OPMVV && io.issue.bits.opmf6 === OPMFunct6.wrxunary0) {
      io.issue.ready := io.scalar_resp.ready
      io.scalar_resp.valid := io.issue.valid
    }
  }

  io.dis.bits := issue_inst

  io.mem.bits.base_offset := issue_inst.rs1_data
  io.mem.bits.stride := issue_inst.rs2_data
  io.mem.bits.page := issue_inst.page
  io.mem.bits.vstart := issue_inst.vstart
  io.mem.bits.segstart := issue_inst.segstart
  io.mem.bits.segend := issue_inst.segend
  io.mem.bits.vl := issue_inst.vconfig.vl
  io.mem.bits.mop := issue_inst.mop
  io.mem.bits.vm := issue_inst.vm
  io.mem.bits.nf := issue_inst.nf
  io.mem.bits.idx_size := issue_inst.mem_idx_size
  io.mem.bits.elem_size := issue_inst.mem_elem_size
  io.mem.bits.whole_reg := issue_inst.umop === lumopWhole && issue_inst.orig_mop === mopUnit
  io.mem.bits.store := issue_inst.bits(5)
  io.mem.bits.fast_sg := issue_inst.fast_sg
  io.mem.bits.debug_id := issue_inst.debug_id


  for (r <- io.vat_release) {
    when (r.valid) {
      assert(vat_valids(r.bits))
      when (r.bits === vat_head) { vat_head_incr := true.B }
      vat_valids(r.bits) := false.B
      hwacha_limiter.foreach(_.io.vat_release(r.bits) := true.B)
    }
  }
}
