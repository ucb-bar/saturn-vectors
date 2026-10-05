package chipyard

import org.chipsalliance.cde.config.{Config}
import saturn.common.{VectorParams}

// ------------------------------------------------------------------------
// RISC-V VME subset (Xsfmm v0.6.6 encodings) performance targets:
// half-width datapaths with the int8 + OCP FP8 outer-product unit.
// Tile edge TE = VLEN/8 (Xsfmm16t / Xsfmm32t / Xsfmm64t).

class VMEV128D64MxShuttleConfig extends Config(
  new saturn.shuttle.WithShuttleVectorUnit(128, 64, VectorParams.opuMxParams) ++
  new chipyard.config.WithSystemBusWidth(64) ++
  new shuttle.common.WithShuttleTileBeatBytes(8) ++
  new shuttle.common.WithNShuttleCores(1) ++
  new chipyard.config.AbstractConfig)

class VMEV256D128MxShuttleConfig extends Config(
  new saturn.shuttle.WithShuttleVectorUnit(256, 128, VectorParams.opuMxParams) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new shuttle.common.WithShuttleTileBeatBytes(16) ++
  new shuttle.common.WithNShuttleCores(1) ++
  new chipyard.config.AbstractConfig)

class VMEV256D64MxShuttleConfig extends Config(
  new saturn.shuttle.WithShuttleVectorUnit(256, 64, VectorParams.opuMxParams) ++
  new chipyard.config.WithSystemBusWidth(64) ++
  new shuttle.common.WithShuttleTileBeatBytes(8) ++
  new shuttle.common.WithNShuttleCores(1) ++
  new chipyard.config.AbstractConfig)

class VMEV512D256MxShuttleConfig extends Config(
  new saturn.shuttle.WithShuttleVectorUnit(512, 256, VectorParams.opuMxParams) ++
  new chipyard.config.WithSystemBusWidth(256) ++
  new shuttle.common.WithShuttleTileBeatBytes(32) ++
  new shuttle.common.WithNShuttleCores(1) ++
  new chipyard.config.AbstractConfig)

class VMEV512D128MxShuttleConfig extends Config(
  new saturn.shuttle.WithShuttleVectorUnit(512, 128, VectorParams.opuMxParams) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new shuttle.common.WithShuttleTileBeatBytes(16) ++
  new shuttle.common.WithNShuttleCores(1) ++
  new chipyard.config.AbstractConfig)

class VMEV1024D256MxShuttleConfig extends Config(
  new saturn.shuttle.WithShuttleVectorUnit(1024, 256, VectorParams.opuMxParams) ++
  new chipyard.config.WithSystemBusWidth(256) ++
  new shuttle.common.WithShuttleTileBeatBytes(32) ++
  new shuttle.common.WithNShuttleCores(1) ++
  new chipyard.config.AbstractConfig)

class VMEV128D64ShuttleConfig extends Config(
  new saturn.shuttle.WithShuttleVectorUnit(128, 64, VectorParams.opuParams) ++
  new chipyard.config.WithSystemBusWidth(64) ++
  new shuttle.common.WithShuttleTileBeatBytes(8) ++
  new shuttle.common.WithNShuttleCores(1) ++
  new chipyard.config.AbstractConfig)

class VMEV256D128ShuttleConfig extends Config(
  new saturn.shuttle.WithShuttleVectorUnit(256, 128, VectorParams.opuParams) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new shuttle.common.WithShuttleTileBeatBytes(16) ++
  new shuttle.common.WithNShuttleCores(1) ++
  new chipyard.config.AbstractConfig)

class VMEV256D64ShuttleConfig extends Config(
  new saturn.shuttle.WithShuttleVectorUnit(256, 64, VectorParams.opuParams) ++
  new chipyard.config.WithSystemBusWidth(64) ++
  new shuttle.common.WithShuttleTileBeatBytes(8) ++
  new shuttle.common.WithNShuttleCores(1) ++
  new chipyard.config.AbstractConfig)

class VMEV512D256ShuttleConfig extends Config(
  new saturn.shuttle.WithShuttleVectorUnit(512, 256, VectorParams.opuParams) ++
  new chipyard.config.WithSystemBusWidth(256) ++
  new shuttle.common.WithShuttleTileBeatBytes(32) ++
  new shuttle.common.WithNShuttleCores(1) ++
  new chipyard.config.AbstractConfig)

class VMEV512D128ShuttleConfig extends Config(
  new saturn.shuttle.WithShuttleVectorUnit(512, 128, VectorParams.opuParams) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new shuttle.common.WithShuttleTileBeatBytes(16) ++
  new shuttle.common.WithNShuttleCores(1) ++
  new chipyard.config.AbstractConfig)

class VMEV1024D256ShuttleConfig extends Config(
  new saturn.shuttle.WithShuttleVectorUnit(1024, 256, VectorParams.opuParams) ++
  new chipyard.config.WithSystemBusWidth(256) ++
  new shuttle.common.WithShuttleTileBeatBytes(32) ++
  new shuttle.common.WithNShuttleCores(1) ++
  new chipyard.config.AbstractConfig)

class VMEV512D256RocketConfig extends Config(
  new saturn.rocket.WithRocketVectorUnit(512, 256, VectorParams.opuParams) ++
  new chipyard.config.WithSystemBusWidth(256) ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.AbstractConfig)

class VMEV512D128RocketConfig extends Config(
  new saturn.rocket.WithRocketVectorUnit(512, 128, VectorParams.opuParams) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.AbstractConfig)

class VMEV128D64DualShuttleConfig extends Config(
  new saturn.shuttle.WithShuttleVectorUnit(128, 64, VectorParams.opuParams) ++
  new chipyard.config.WithSystemBusWidth(64) ++
  new shuttle.common.WithTCM(size=128L << 10) ++
  new shuttle.common.WithShuttleTileBeatBytes(8) ++
  new shuttle.common.WithNShuttleCores(2) ++
  new chipyard.config.AbstractConfig)
