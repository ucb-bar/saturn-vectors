package saturn.rocket

import chisel3._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._
import freechips.rocketchip.diplomacy._
import saturn.common._
import saturn.frontend.{EarlyVectorDecode}

class WithRocketVectorUnit(
  vLen: Int = 128,
  dLen: Int = 64,
  params: VectorParams = VectorParams(),
  cores: Option[Seq[Int]] = None,
  useL1DCache: Boolean = true) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: RocketTileAttachParams => {
      val buildVector = cores.map(_.contains(tp.tileParams.tileId)).getOrElse(true)
      if (buildVector) tp.copy(tileParams = tp.tileParams.copy(
        core = tp.tileParams.core.copy(
          vector = Some(RocketCoreVectorParams(
            build = ((p: Parameters) => new SaturnRocketUnit()(p.alterPartial {
              case VectorParamsKey => params.copy(dLen=dLen)
            })),
            vLen = vLen,
            vfLen = 64,
            vfh = true,
            eLen = 64,
            vMemDataBits = if (useL1DCache) dLen else 0,
            decoder = ((p: Parameters) => {
              val decoder = Module(new EarlyVectorDecode(params.supported_ex_insns)(p))
              decoder
            }),
            useDCache = true,
            issueVConfig = false,
            vExts = Seq("zvbb")
          )),
          fpu = (if (params.useScalarFPFMA) { tp.tileParams.core.fpu.map(_.copy(
            sfmaLatency = params.fmaPipeDepth - 1,
            dfmaLatency = params.fmaPipeDepth - 1,
            ifpuLatency = params.fmaPipeDepth - 1,
            fpmuLatency = params.fmaPipeDepth - 1,
          )) } else { tp.tileParams.core.fpu }).map(_.copy(minFLen = 16))
        ),
        dcache = if (useL1DCache) tp.tileParams.dcache.map(_.copy(rowBits = dLen)) else tp.tileParams.dcache
      )) else tp
    }
    case other => other
  }
})

