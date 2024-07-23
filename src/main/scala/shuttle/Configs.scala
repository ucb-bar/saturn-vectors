package saturn.shuttle

import chisel3._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._
import freechips.rocketchip.diplomacy._
import saturn.common._
import shuttle.common._

class WithShuttleVectorUnit(
  vLen: Int = 128,
  dLen: Int = 64,
  params: VectorParams = VectorParams(),
  cores: Option[Seq[Int]] = None,
  location: HierarchicalLocation = InSubsystem
) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: ShuttleTileAttachParams => {
      val buildVector = cores.map(_.contains(tp.tileParams.tileId)).getOrElse(true)
      if (buildVector) tp.copy(tileParams = tp.tileParams.copy(
        core = tp.tileParams.core.copy(
          vector = Some(ShuttleCoreVectorParams(
            build = ((p: Parameters) => new SaturnShuttleUnit()(p.alterPartial {
              case VectorParamsKey => params.copy(
                dLen=dLen,
                useScalarFPFMA = false,
                useScalarFPMisc = false
              )
            })),
            vfLen = 64,
            vfh = true,
            vLen = vLen,
            decoder = ((p: Parameters) => {
              val decoder = Module(new EarlyVectorDecode()(p))
              decoder
            }),
            issueVConfig = false
          )),
        )
      )) else tp
    }
    case other => other
  }
})

