package vector.core.rocket

import chisel3._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._
import vector.common._

class WithRocketVectorUnit(vLen: Int = 128, dLen: Int = 64, params: VectorParams = VectorParams()) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: RocketTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(
      core = tp.tileParams.core.copy(vector = Some(RocketCoreVectorParams(
        build = ((p: Parameters) => {
          val vec = Module(new VectorUnit()(p.alterPartial {
            case VectorParamsKey => params
          }))
          vec
        }),
        vLen = vLen,
        vMemDataBits = dLen,
        decoder = ((p: Parameters) => {
          val decoder = Module(new EarlyVectorDecode()(p))
          decoder
        })
      ))),
      dcache = tp.tileParams.dcache.map(_.copy(rowBits = dLen))))
    case other => other
  }
})

