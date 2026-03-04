package chipyard

import org.chipsalliance.cde.config.{Config}
import saturn.common.{VectorParams}

class OPUV128D64ShuttleConfig extends Config(
  new saturn.shuttle.WithShuttleVectorUnit(128, 64, VectorParams.opuParams) ++
  new chipyard.config.WithSystemBusWidth(64) ++
  new shuttle.common.WithShuttleTileBeatBytes(8) ++
  new shuttle.common.WithNShuttleCores(1) ++
  new chipyard.config.AbstractConfig)

class OPUV256D128ShuttleConfig extends Config(
  new saturn.shuttle.WithShuttleVectorUnit(256, 128, VectorParams.opuParams) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new shuttle.common.WithShuttleTileBeatBytes(16) ++
  new shuttle.common.WithNShuttleCores(1) ++
  new chipyard.config.AbstractConfig)

class OPUV512D256ShuttleConfig extends Config(
  new saturn.shuttle.WithShuttleVectorUnit(512, 256, VectorParams.opuParams) ++
  new chipyard.config.WithSystemBusWidth(256) ++
  new shuttle.common.WithShuttleTileBeatBytes(32) ++
  new shuttle.common.WithNShuttleCores(1) ++
  new chipyard.config.AbstractConfig)

class OPUV512D256RocketConfig extends Config(
  new saturn.rocket.WithRocketVectorUnit(512, 256, VectorParams.opuParams) ++
  new chipyard.config.WithSystemBusWidth(256) ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.AbstractConfig)
