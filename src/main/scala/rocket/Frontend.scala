package vector.rocket

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._

import vector.common._
import vector.backend.{VectorBackend}
import vector.mem.{ScalarMemOrderCheckIO, MemRequest}


class SaturnRocketUnit(implicit p: Parameters) extends RocketVectorUnit()(p) with HasVectorParams with HasCoreParameters {

  val tl_if = LazyModule(new TLInterface)
  atlNode := tl_if.node

  override lazy val module = new SaturnRocketImpl

  class SaturnRocketImpl extends RocketVectorUnitModuleImp(this) with HasVectorParams with HasCoreParameters {
    require(dLen == vMemDataBits)

    val trap_check = Module(new FrontendTrapCheck)
    trap_check.io.core <> io.core
    trap_check.io.tlb <> io.tlb

    val hella_if = Module(new HellaCacheInterface)

    val vu = Module(new VectorBackend)
    vu.io.issue <> trap_check.io.issue
    trap_check.io.index_access <> vu.io.index_access
    trap_check.io.mask_access  <> vu.io.mask_access
    trap_check.io.scalar_check <> vu.io.mem.scalar_check
    trap_check.io.backend_busy := vu.io.backend_busy
    trap_check.io.vm_busy  := vu.io.vm_busy
    io.core.backend_busy   := vu.io.backend_busy
    io.core.set_vxsat      := vu.io.set_vxsat
    io.core.set_fflags     := vu.io.set_fflags
    io.core.resp           <> vu.io.scalar_resp

    io.dmem <> hella_if.io.dmem
    hella_if.io.vec_busy := vu.io.backend_busy
    tl_if.module.io.vec_busy := vu.io.backend_busy
    hella_if.io.status := io.core.status

    def block[T <: Data](in: DecoupledIO[T], block: Bool): DecoupledIO[T] = {
      val out = Wire(Decoupled(in.bits.cloneType))
      out.bits := in.bits
      out.valid := in.valid && !block
      in.ready := out.ready && !block
      out
    }

    tl_if.module.io.vec.scalar_check := DontCare
    hella_if.io.vec.scalar_check := DontCare

    val load_use_tl_reg = RegInit(true.B)
    val store_use_tl_reg = RegInit(true.B)

    // virtually-addressed requests must go through L1
    val load_use_tl = load_use_tl_reg && vu.io.mem.load_req.bits.phys
    val store_use_tl = store_use_tl_reg && vu.io.mem.store_req.bits.phys

    vu.io.mem.load_resp.valid := tl_if.module.io.vec.load_resp.valid || hella_if.io.vec.load_resp.valid
    vu.io.mem.load_resp.bits := Mux1H(
      Seq(tl_if.module.io.vec.load_resp.valid, hella_if.io.vec.load_resp.valid),
      Seq(tl_if.module.io.vec.load_resp.bits , hella_if.io.vec.load_resp.bits))
    vu.io.mem.store_ack.valid := tl_if.module.io.vec.store_ack.valid || hella_if.io.vec.store_ack.valid
    vu.io.mem.store_ack.bits := Mux1H(
      Seq(tl_if.module.io.vec.store_ack.valid, hella_if.io.vec.store_ack.valid),
      Seq(tl_if.module.io.vec.store_ack.bits , hella_if.io.vec.store_ack.bits))

    when (load_use_tl) {
      tl_if.module.io.vec.load_req <> block(vu.io.mem.load_req, hella_if.io.mem_busy)
      hella_if.io.vec.load_req.valid := false.B
      hella_if.io.vec.load_req.bits := DontCare
    } .otherwise {
      hella_if.io.vec.load_req <> block(vu.io.mem.load_req, tl_if.module.io.mem_busy)
      tl_if.module.io.vec.load_req.valid := false.B
      tl_if.module.io.vec.load_req.bits := DontCare
    }
    when (store_use_tl) {
      tl_if.module.io.vec.store_req <> block(vu.io.mem.store_req, hella_if.io.mem_busy)
      hella_if.io.vec.store_req.valid := false.B
      hella_if.io.vec.store_req.bits := DontCare
    } .otherwise {
      hella_if.io.vec.store_req <> block(vu.io.mem.store_req, tl_if.module.io.mem_busy)
      tl_if.module.io.vec.store_req.valid := false.B
      tl_if.module.io.vec.store_req.bits := DontCare
    }
  }
}

