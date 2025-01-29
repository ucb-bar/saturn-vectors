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
import saturn.mem.{TLSplitInterface, SGTLInterface, VectorMemUnit}
import saturn.frontend.{VectorDispatcher}
import shuttle.common._


class SaturnShuttleUnit(implicit p: Parameters) extends ShuttleVectorUnit()(p) with HasVectorParams with HasCoreParameters {
  assert(!vParams.useScalarFPFMA)
  if (vParams.useScalarFPFMA) {
    require(coreParams.fpu.get.dfmaLatency == vParams.fmaPipeDepth - 1)
  }

  val tl_if = LazyModule(new TLSplitInterface)
  atlNode := TLBuffer(vParams.tlBuffer) := TLWidthWidget(mLenB) := tl_if.node

  val sg_if = sgNode.map { n =>
    val sg_if = LazyModule(new SGTLInterface)
    n :=* sg_if.node
    sg_if
  }

  override lazy val module = new SaturnShuttleImpl
  class SaturnShuttleImpl extends ShuttleVectorUnitModuleImp(this) with HasVectorParams with HasCoreParameters {

    val dis = Module(new VectorDispatcher)
    val scalar_arb = Module(new Arbiter(new ScalarWrite, 2))
    val vfu = Module(new SaturnShuttleFrontend(sgSize, tl_if.edge))
    val vu = Module(new VectorBackend)
    val vmu = Module(new VectorMemUnit(sgSize))

    sg_if.foreach { sg =>
      sg.module.io.vec <> vmu.io.sgmem.get
    }

    dis.io.issue <> vfu.io.issue
    vfu.io.core <> io
    vfu.io.sg_base := io_sg_base

    vu.io.index_access <> vfu.io.index_access
    vu.io.mask_access <> vfu.io.mask_access
    vu.io.vmu <> vmu.io.vu
    vu.io.vat_tail := dis.io.vat_tail
    vu.io.vat_head := dis.io.vat_head
    vu.io.dis <> dis.io.dis
    dis.io.vat_release := vu.io.vat_release
    vmu.io.enq <> dis.io.mem

    vmu.io.scalar_check <> vfu.io.scalar_check

    io.backend_busy   := vu.io.busy || tl_if.module.io.mem_busy || sg_if.map(_.module.io.mem_busy).getOrElse(false.B) || vmu.io.busy
    io.set_vxsat      := vu.io.set_vxsat
    io.set_fflags     := vu.io.set_fflags


    scalar_arb.io.in(0) <> vu.io.scalar_resp
    scalar_arb.io.in(1) <> dis.io.scalar_resp
    io.resp <> Queue(scalar_arb.io.out)

    tl_if.module.io.vec <> vmu.io.dmem

    vu.io.fp_req.ready := false.B
    vu.io.fp_resp.valid := false.B
    vu.io.fp_resp.bits := DontCare
  }
}
