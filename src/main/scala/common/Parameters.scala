package saturn.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import freechips.rocketchip.diplomacy.{BufferParams}

object VectorParams {

  // minParams:
  // For a very small area-efficient vector unit with iterative
  // and element-wise functional units
  def minParams = VectorParams()

  // refParams
  // For a standard modestly capable small vector unit with
  // SIMD functional units
  def refParams = minParams.copy(
    vlrobEntries = 4,
    vlissqEntries = 3,
    vsissqEntries = 3,
    vxissqEntries = 3,
    vatSz = 5,
    useSegmentedIMul = true,
    doubleBufferSegments = true,
    useScalarFPFMA = false,
    vrfBanking = 4,
  )

  // dspParams
  // For a wide high-performance vector unit with multi-issue
  def dspParams = refParams.copy(
    issStructure = VectorIssueStructure.Shared
  )

  // genParams:
  // For a vector unit that performs better on less-optimized
  // code sequences
  def genParams = dspParams.copy(
    issStructure = VectorIssueStructure.Split
  )

  // dmaParams:
  // For a vector unit that only does memcpys, and no arithmetic
  def dmaParams = VectorParams(
    vdqEntries = 2,
    vliqEntries = 4,
    vsiqEntries = 4,
    vlifqEntries = 32,
    vlrobEntries = 4,
    vsifqEntries = 32,
    vlissqEntries = 2,
    vsissqEntries = 1,
    vrfBanking = 1,
    useIterativeIMul = true
  )

  // The parameters below are approximations

  // spzParams:
  // For a vector unit which is similar-ish to Spatz
  def spzParams = VectorParams(
    vdqEntries = 1,
    vlifqEntries = 8, // match spatz params
    vsifqEntries = 8,
    vlrobEntries = 8,
    useSegmentedIMul = true,
    useScalarFPMisc = false,
    useScalarFPFMA = false,
    vrfBanking = 4,
    vatSz = 2 // spatz supports up to 4 parallel insns
  )

  // hwaParams
  // For a vector unit with limited sequencer slots akin to Hwacha
  def hwaParams = VectorParams(
    vatSz = 3,
    vlifqEntries = 32, // match VLTEntries
    vsifqEntries = 32,
    vlrobEntries = 32,
    vlissqEntries = 8,
    vsissqEntries = 8,
    vxissqEntries = 8,
    vpissqEntries = 8,
    useSegmentedIMul = true,
    useScalarFPMisc = false,
    useScalarFPFMA = false,
    vrfBanking = 4,
    hwachaLimiter = Some(8), // sequencer slots
    issStructure = VectorIssueStructure.Split
  )

  // lgvParams
  // For a vector unit with very long vector lengths
  def lgvParams = VectorParams(
    vatSz = 5,
    vlifqEntries = 32,
    vsifqEntries = 32,
    vlrobEntries = 32,
    vlissqEntries = 8,
    vsissqEntries = 8,
    vxissqEntries = 8,
    vpissqEntries = 8,
    useSegmentedIMul = true,
    useScalarFPMisc = false,
    useScalarFPFMA = false,
    vrfBanking = 4,
    issStructure = VectorIssueStructure.Split
  )

}

sealed trait VectorIssueStructure
object VectorIssueStructure {
  case object Unified extends VectorIssueStructure
  case object Shared extends VectorIssueStructure
  case object Split extends VectorIssueStructure
}

case class VectorParams(
  // In-order dispatch Queue
  vdqEntries: Int = 4,

  // Load store instruction queues (in VLSU)
  vliqEntries: Int = 4,
  vsiqEntries: Int = 4,

  // Load store in-flight queues (in VLSU)
  vlifqEntries: Int = 8,
  vsifqEntries: Int = 8,
  vlrobEntries: Int = 2,

  // Load/store/execute/permute/maskindex issue queues
  vlissqEntries: Int = 0,
  vsissqEntries: Int = 0,
  vxissqEntries: Int = 0,
  vpissqEntries: Int = 0,

  dLen: Int = 64,
  vatSz: Int = 3,

  useSegmentedIMul: Boolean = false,
  useScalarFPMisc: Boolean = true,       // Use shared scalar FPU for all non-FMA FP instructions
  useScalarFPFMA: Boolean = true,        // Use shared scalar FPU for FMA instructions
  useIterativeIMul: Boolean = false,
  fmaPipeDepth: Int = 4,
  imaPipeDepth: Int = 3,

  // for comparisons only
  hazardingMultiplier: Int = 0,
  hwachaLimiter: Option[Int] = None,
  enableChaining: Boolean = true,
  latencyInject: Boolean = false,

  doubleBufferSegments: Boolean = false,

  vrfBanking: Int = 2,

  issStructure: VectorIssueStructure = VectorIssueStructure.Unified,

  tlBuffer: BufferParams = BufferParams.default,
)

case object VectorParamsKey extends Field[VectorParams]

trait HasVectorParams extends HasVectorConsts { this: HasCoreParameters =>
  implicit val p: Parameters
  def vParams: VectorParams = p(VectorParamsKey)
  def dLen = vParams.dLen
  def dLenB = dLen / 8
  def dLenOffBits = log2Ceil(dLenB)
  def dmemTagBits = log2Ceil(vParams.vlifqEntries.max(vParams.vsifqEntries))
  def egsPerVReg = vLen / dLen
  def egsTotal = (vLen / dLen) * 32
  def vrfBankBits = log2Ceil(vParams.vrfBanking)
  def lsiqIdBits = log2Ceil(vParams.vliqEntries.max(vParams.vsiqEntries))

  def getEgId(vreg: UInt, eidx: UInt, eew: UInt, bitwise: Bool): UInt = {
    val base = vreg << log2Ceil(egsPerVReg)
    val off = eidx >> Mux(bitwise, log2Ceil(dLen).U, (log2Ceil(dLenB).U - eew))
    base +& off
  }
  def getByteId(vreg: UInt, eidx: UInt, eew: UInt): UInt = {
    Cat(getEgId(vreg, eidx, eew, false.B), (eidx << eew)(log2Ceil(dLenB)-1,0))
  }

  def eewByteMask(eew: UInt) = (0 until (1+log2Ceil(eLen/8))).map { e =>
    Mux(e.U === eew, ((1 << (1 << e)) - 1).U, 0.U)
  }.reduce(_|_)((eLen/8)-1,0)
  def eewBitMask(eew: UInt) = FillInterleaved(8, eewByteMask(eew))


  def cqOlder(i0: UInt, i1: UInt, tail: UInt) = (i0 < i1) ^ (i0 < tail) ^ (i1 < tail)
  def dLenSplat(in: UInt, eew: UInt) = {
    val v = Wire(UInt(64.W))
    v := in
    Mux1H(UIntToOH(eew), (0 until 4).map { i => Fill(dLenB >> i, v((8<<i)-1,0)) })
  }

  def sextElem(in: UInt, in_eew: UInt): UInt = VecInit.tabulate(4)( { eew =>
    Cat(in((8 << eew)-1), in((8 << eew)-1,0)).asSInt
  })(in_eew)(64,0)

  def extractElem(in: UInt, in_eew: UInt, eidx: UInt): UInt = {
    val bytes = in.asTypeOf(Vec(dLenB, UInt(8.W)))
    VecInit.tabulate(4) { eew =>
      val elem = if (dLen == 64 && eew == 3) {
        in
      } else {
        VecInit(bytes.grouped(1 << eew).map(g => VecInit(g).asUInt).toSeq)(eidx(log2Ceil(dLenB)-1-eew,0))
      }
      elem((8 << eew)-1,0)
    }(in_eew)
  }

  def maxPosUInt(sew: Int) = Cat(0.U, ~(0.U(((8 << sew)-1).W)))
  def minNegUInt(sew: Int) = Cat(1.U,   0.U(((8 << sew)-1).W))
  def maxPosSInt(sew: Int) = ((1 << ((8 << sew)-1))-1).S
  def minNegSInt(sew: Int) = (-1 << ((8 << sew)-1)).S
  def maxPosFPUInt(sew: Int) = {
    val expBits = Seq(4, 5, 8, 11)(sew)
    val fracBits = (8 << sew) - expBits - 1
    Cat(0.U, ~(0.U(expBits.W)), 0.U(fracBits.W))
  }
  def minNegFPUInt(sew: Int) = {
    val expBits = Seq(4, 5, 8, 11)(sew)
    val fracBits = (8 << sew) - expBits - 1
    Cat(1.U, ~(0.U(expBits.W)), 0.U(fracBits.W))
  }
  def get_arch_mask(reg: UInt, emul: UInt) = VecInit.tabulate(4)({ lmul =>
    FillInterleaved(1 << lmul, UIntToOH(reg >> lmul)((32>>lmul)-1,0))
  })(emul)
  def log2_up(f: UInt, max: Int) = VecInit.tabulate(max)({nf => log2Ceil(nf+1).U})(f)

  def hazardMultiply(mask: UInt): UInt = if (vParams.hazardingMultiplier == 0) { mask } else {
    require((1 << vParams.hazardingMultiplier) <= egsTotal)
    VecInit(mask.asBools.grouped(1 << vParams.hazardingMultiplier).map { g =>
      Fill(1 << vParams.hazardingMultiplier, g.orR)
    }.toSeq).asUInt
  }
}
