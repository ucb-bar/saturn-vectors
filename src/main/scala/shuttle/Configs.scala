package saturn.shuttle

import chisel3._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._
import freechips.rocketchip.diplomacy._
import saturn.common._
import saturn.frontend.{EarlyVectorDecode}
import shuttle.common._

class WithShuttleVectorUnit(
  vLen: Int = 128,
  dLen: Int = 64,
  params: VectorParams = VectorParams(),
  cores: Option[Seq[Int]] = None,
  location: HierarchicalLocation = InSubsystem,
  mLen: Option[Int] = None
) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: ShuttleTileAttachParams => {
      val buildVector = cores.map(_.contains(tp.tileParams.tileId)).getOrElse(true)
      val vParams = params.copy(
        dLen=dLen,
        mLen=mLen.getOrElse(dLen),
        useScalarFPFMA = false,
      )
      if (buildVector) tp.copy(tileParams = tp.tileParams.copy(
        core = tp.tileParams.core.copy(
          vector = Some(ShuttleCoreVectorParams(
            build = ((p: Parameters) => new SaturnShuttleUnit()(p.alterPartial {
              case VectorParamsKey => vParams
            })),
            vfLen = 64,
            vfh = true,
            vLen = vLen,
            decoder = ((p: Parameters) => {
              val decoder = Module(new EarlyVectorDecode(vParams.supported_ex_insns)(p))
              decoder
            }),
            issueVConfig = false,
            vExts = Seq("zvbb")
          )),
        )
      )) else tp
    }
    case other => other
  }
})

