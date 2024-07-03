package saturn.shuttle

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._

import saturn.common._
import saturn.backend.{VectorBackend}
import saturn.mem.{ScalarMemOrderCheckIO, MemRequest, TLSplitInterface, SGTLInterface}
import saturn.frontend.{EarlyTrapCheck, IterativeTrapCheck}
import shuttle.common._


class SaturnShuttleUnit(implicit p: Parameters) extends ShuttleVectorUnit()(p) with HasVectorParams with HasCoreParameters {
  assert(!vParams.useScalarFPFMA && !vParams.useScalarFPMisc)
  if (vParams.useScalarFPFMA) {
    require(coreParams.fpu.get.dfmaLatency == vParams.fmaPipeDepth - 1)
  }

  val tl_if = LazyModule(new TLSplitInterface)
  atlNode := TLBuffer(vParams.tlBuffer) := TLWidthWidget(dLenB) := tl_if.node

  val sg_if = sgNode.map { n =>
    val sg_if = LazyModule(new SGTLInterface)
    n :=* sg_if.node
    sg_if
  }

  override lazy val module = new SaturnShuttleImpl
  class SaturnShuttleImpl extends ShuttleVectorUnitModuleImp(this) with HasVectorParams with HasCoreParameters {

    val vfu = Module(new SaturnShuttleFrontend(sgSize, tl_if.edge))
    val vu = Module(new VectorBackend(sgSize))

    sg_if.foreach { sg =>
      sg.module.io.vec <> vu.io.sgmem.get
    }

    vfu.io.core <> io
    vfu.io.sg_base := io_sg_base
    vu.io.issue <> vfu.io.issue
    vu.io.index_access <> vfu.io.index_access
    vu.io.mask_access <> vfu.io.mask_access
    vu.io.scalar_check <> vfu.io.scalar_check

    io.backend_busy   := vu.io.backend_busy || tl_if.module.io.mem_busy || sg_if.map(_.module.io.mem_busy).getOrElse(false.B)
    io.set_vxsat      := vu.io.set_vxsat
    io.set_fflags     := vu.io.set_fflags
    io.resp           <> vu.io.scalar_resp

    tl_if.module.io.vec <> vu.io.dmem

    vu.io.fp_req.ready := false.B
    vu.io.fp_resp.valid := false.B
    vu.io.fp_resp.bits := DontCare
  }
}
