package saturn.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import chisel3.util.experimental.decode._
import saturn.common._
import hardfloat._


case class OPEParameters (
  val A_width : Int,
  val B_width : Int,
  val C_width : Int,
  val roundingMode : UInt,
  val detectTininess : Bool
)


class OPEUnit(params : OPEParameters)(implicit p: Parameters) extends CoreModule{
  val io = IO(new Bundle{
    val aa = Input(UInt(params.A_width.W))
    val b = Input(UInt(params.B_width.W))
    val c = Input(UInt(params.C_width.W))
    val out = Output(UInt(params.C_width.W))

    val en = Input(Bool())
    val acc = Input(Bool())
    val msel = Input(UInt(2.W))
    val cfg_en = Input(Bool())
  })


  // Multipliers
  val mult8  = Module(new MulRecFN(4, 3))
  val mult16 = Module(new MulRecFN(5, 10))
  val mult32 = Module(new MulRecFN(8, 23))

  val mults = List(mult8, mult16, mult32)
  val msel_reg = Reg(UInt(2.W))
  val acco_reg = Reg(Bool())
  val acc = RegInit(UInt(params.C_width.W), 0.U)

  // Load configure settings for op
  when (io.cfg_en === true.B) {
    msel_reg := io.msel
    acco_reg := io.acc
  }

  // Connect inputs
  mults.foreach( m => {
    // m.io.op := 0.U
    m.io.a := io.aa(m.io.a.getWidth-1, 0) // Truncate as necessary
    m.io.b := io.b(m.io.b.getWidth-1, 0) // Truncate as necessary
    // m.io.c := io.c(0,m.io.c.getWidth-1) // Truncate as necessary
    m.io.roundingMode   := params.roundingMode
    m.io.detectTininess := params.detectTininess
  })


  val mult_mux = MuxCase(mults(1).io.out,
                        Array((msel_reg === 0.U) -> mults(0).io.out,
                        (msel_reg === 1.U) -> mults(1).io.out,
                        (msel_reg === 2.U) -> mults(2).io.out))

  acc := acc + mult_mux
  io.out := Mux(acco_reg, acc + mult_mux, mult_mux)

}


class OPE(params: OPEParameters)(implicit p : Parameters)  extends CoreModule()(p) with HasVectorParams  {

  val VLEN = 512

  val io = IO(new Bundle{
    val a = Input(UInt(dLen.W))
    val b = Input(UInt(dLen.W))
    val c = Input(UInt(dLen.W))
    val out = Output(UInt(dLen.W))

    val en = Input(Bool())
    val acc = Input(Bool())
    val msel = Input(UInt(2.W))
    val cfg_en = Input(Bool())
  })

  // Input registers
  val numel_A = VLEN/params.A_width
  val numel_B = VLEN/params.B_width
  val a_reg = RegInit(VecInit(Seq.fill(numel_A)(0.U(params.A_width.W))))  // Reg(VecInit[UInt(params.A_width.W)](VLEN/params.A_width, UInt(params.A_width.W)))
  val b_reg = RegInit(VecInit(Seq.fill(numel_B)(0.U(params.B_width.W))))  // Reg(VecInit[UInt(params.A_width.W)](VLEN/params.B_width, UInt(params.B_width.W)))

  val mgrid = Seq.fill(VLEN/params.A_width,VLEN/params.B_width){Module(new OPEUnit(params))}
  // val mgrid = Seq(Seq(Module(new OPEUnit(params))))

  val cnt = RegInit(0.U(32.W))
  cnt := cnt+1.U


  // a_reg.zipWithIndex.foreach({case (x, i) => x := io.a(((i+1)*dLen)%numel_A, (i*dLen)%numel_A)})
  // b_reg.zipWithIndex.foreach({case (x, i) => x := io.b(((i+1)*dLen)%numel_B, (i*dLen)%numel_B)})

  for (i <- (0 until numel_A)) {
    var hi = ((i+1)*params.A_width - 1)%dLen
    var lo = ((i)*params.B_width)%dLen
//    println(s"ORANGES: a($hi, $lo) ")
    a_reg(i) := io.a(hi, lo)
    b_reg(i) := io.b(hi, lo)
  }

  mgrid.foreach(row => {
//    println("APPLES")
//    println(row)
    row.zipWithIndex.foreach({case (mult,i) => {
//      println("ORANGES")
//      println(mult)
      mult.io.aa     := a_reg(i)
      mult.io.b      := b_reg(i)
      mult.io.c      := io.c
      mult.io.en     := io.en // hardcoded
      mult.io.acc    := io.acc // hardcoded
      mult.io.msel   := cnt % 3.U
      mult.io.cfg_en := io.cfg_en // hardcoded
    }
    })
  })


  // val T = A.grouped(dLen/WIDTH).foldRight(Seq.fill(dLen/WIDTH)(0.U(WIDTH.W)))((l0, l1) => (l0,l1).zipped.map(_+_))
  // println(T.reduceRight(_ ## _))

  val folds = VLEN/dLen

  val row_sums = mgrid.map(m => m.foldRight(0.U)((x, y) => x.io.out + y))
  val div = (dLen/params.A_width)
  val output = row_sums.grouped(div).foldRight(Seq.fill(dLen/params.A_width)(0.U(params.A_width.W)))((l0, l1) => (l0,l1).zipped.map(_+_))
  io.out := output.reduceRight(_ ## _)
}


