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
import scala.math._

// Declare supported types (dictates multipler instantiated)
object OPUTypes extends Enumeration {
  val INT32, INT16, INT8 = Value
  // val FP32, FP16, FP8, INT32, INT16, INT8 = Value
}

// Parameters for configured OPU
case class OPUParameters (
  val A_width : Int,
  val B_width : Int,
  val C_width : Int, // Accumulator size
  // val accum_width : Int,

  // val types : Seq[OPUTypes],

  // To support floating point FMAs
  val roundingMode   : UInt,
  val detectTininess : Bool
)

/*
 * A single cell in the Outer Product Unit MACC array
 * Accumulation register alse serve as pipeline registers
 * during read out 
 *
 */
class OuterProductCell(params : OPUParameters)(implicit p: Parameters) extends CoreModule{
  val io = IO(new Bundle{
    val a   = Input(SInt(params.A_width.W))
    val b   = Input(SInt(params.B_width.W))
    val c   = Input(SInt(params.C_width.W))
    val out = Output(SInt(params.C_width.W))

    val read_in     = Input(SInt(params.C_width.W)) // Read out support

    val cell_en     = Input(Bool())   // Turn off cell; Controlled by mask and (?; for power saving features)
    val macc_en     = Input(Bool())
    val cfg_en      = Input(Bool())
    val reg_rst     = Input(Bool())
    val read_out_en = Input(Bool())
  })

  // Constants 
  val prod_width = (params.A_width + params.B_width).W

  val macc_en      = Reg(Bool())       // Latch configuration register
  val prod         = Wire(SInt(32.W))  // Make width dynamic
  val accum        = Wire(SInt(32.W))  // Make width dynamic
  val cell_reg     = RegInit(SInt(params.C_width.W), 0.S)   // Accumulation/Read Out
  // Input padding   
  val a_sint_padded = Wire(SInt(32.W)) // TODO: Switch to dyanmic selection of parameter to max width for datatype
  val b_sint_padded = Wire(SInt(32.W)) // TODO: Switch to dyanmic selection of parameter to max width for datatype
  val c_sint_padded = Wire(SInt(32.W)) // TODO: Switch to dyanmic selection of parameter to max width for datatype
  // Muxes
  val prod_mux     = Wire(SInt(prod_width))
  val out_mux      = Wire(SInt(params.C_width.W))
  val reg_mux      = Wire(SInt(params.C_width.W))

  a_sint_padded := 0.S
  b_sint_padded := 0.S
  c_sint_padded := 0.S

  // Load configure settings for op
  when (io.cfg_en === true.B) {
    cell_reg := io.c
    macc_en  := io.macc_en
  }

  // TODO: Padded inputs to large width of respective datatype (i.e. int8 to int32)
  //       This needs to be dynamic and data type is programmable

  // TODO: Need to check for overflow and saturate to accumulator width
  prod  := io.a*io.b // Save to intermediate to force overflow
  accum := prod

  prod_mux := prod // TODO: if multiple data types support switch on this variable
  out_mux  := MuxCase(prod_mux, Array(macc_en -> cell_reg,  io.read_out_en -> cell_reg))
  reg_mux  := MuxCase(out_mux,  Array(io.reg_rst -> 0.S, io.cfg_en -> io.c, io.read_out_en -> io.read_in))

  io.out := out_mux // Output selected 
}


// class OuterProductCell_FP_PrelimSyn(params : OPEParameters)(implicit p: Parameters) extends CoreModule{
//   val io = IO(new Bundle{
//     val a  = Input(UInt(params.A_width.W))
//     val b   = Input(UInt(params.B_width.W))
//     val c   = Input(UInt(params.C_width.W))
//     val out = Output(UInt(params.C_width.W))

//     val en     = Input(Bool())
//     val acc    = Input(Bool())
//     val msel   = Input(UInt(2.W))
//     val cfg_en = Input(Bool())
//   })

//   // Multipliers
//   val mult8  = Module(new MulAddRecFNPipe(2, 4, 3))
//   val mult16 = Module(new MulAddRecFNPipe(2, 5, 10))
//   val mult32 = Module(new MulAddRecFNPipe(2, 8, 23))

//   val mults = List(mult8, mult16, mult32)
//   val msel_reg = Reg(UInt(2.W))
//   val acco_reg = Reg(Bool())
//   val acc = RegInit(UInt(params.C_width.W), 0.U)

//   // Load configure settings for op
//   when (io.cfg_en === true.B) {
//     msel_reg := io.msel
//     acco_reg := io.acc
//     acc      := io.c(mults(io.msel).io.c.getWidth-1, 0) // Truncate as necessary
//   }

//   // Connect inputs
//   mults.foreach( m => {
//     m.io.validin := True.B
//     m.io.op := 0.U  // Mult-Add
//     m.io.a := io.a(m.io.a.getWidth-1, 0) // Truncate as necessary
//     m.io.b := io.b(m.io.b.getWidth-1, 0) // Truncate as necessary
//     m.io.c := acc
//     m.io.roundingMode   := params.roundingMode
//     m.io.detectTininess := params.detectTininess
//   })


//   val mult_mux = MuxCase(mults(1).io.out,
//                         Array((msel_reg === 0.U) -> mults(0).io.out,
//                         (msel_reg === 1.U) -> mults(1).io.out,
//                         (msel_reg === 2.U) -> mults(2).io.out))

//   // acc := acc + mult_mux
//   // io.out := Mux(acco_reg, acc + mult_mux, mult_mux)
//   io.out := mult_mux

// }


class OuterProductUnit(params: OPUParameters)(implicit p : Parameters)  extends CoreModule()(p) with HasVectorParams  {
  
  val VLEN = 512

  val io = IO(new Bundle{
    val a      = Input(UInt(dLen.W))
    val b      = Input(UInt(dLen.W))
    val c      = Input(UInt(dLen.W))
    val out    = Output(SInt(dLen.W))
    val rd_out = Input(Bool())

    val en     = Input(Bool())
    val acc    = Input(Bool())
    val msel   = Input(UInt(2.W))
    val cfg_en = Input(Bool())
  })

  // Input registers
  val numel_A = dLen/params.A_width
  val numel_B = dLen/params.B_width
  val numel_C = max(numel_A, numel_B)
  val a_wire   = WireInit(VecInit(Seq.fill(numel_A)(0.S(params.A_width.W))))  // Reg(VecInit[UInt(params.A_width.W)](VLEN/params.A_width, UInt(params.A_width.W)))
  val b_wire   = WireInit(VecInit(Seq.fill(numel_B)(0.S(params.B_width.W))))  // Reg(VecInit[UInt(params.A_width.W)](VLEN/params.B_width, UInt(params.B_width.W)))
  val c_wire   = WireInit(VecInit(Seq.fill(numel_B)(0.S(params.C_width.W))))  // Reg(VecInit[UInt(params.A_width.W)](VLEN/params.B_width, UInt(params.B_width.W)))

  val cell_array = Seq.fill(numel_A, numel_B){Module(new OuterProductCell(params))}
  // val read_regs  = Seq.fill(numel_A, numel_B){RegInit(UInt(params.accum_width))}

  for (i <- (0 until numel_A)) {
    var hi = ((i+1)*params.A_width - 1)%dLen
    var lo = ((i)*params.A_width)%dLen
    a_wire(i) := io.a(hi, lo).asSInt
  }

  for (i <- (0 until numel_B)) {
    var hi = ((i+1)*params.B_width - 1)%dLen
    var lo = ((i)*params.B_width)%dLen
    b_wire(i) := io.b(hi, lo).asSInt
  }

  for (i <- (0 until numel_C)) {
    var hi = ((i+1)*params.C_width - 1)%dLen
    var lo = ((i)*params.C_width)%dLen
    c_wire(i) := io.b(hi, lo).asSInt
  }

  // Logic to drive array (just an fake implementation)
  val logic_cnt = RegInit(UInt(16.W), 0.U)
  logic_cnt := logic_cnt + 1.U;
  cell_array.zipWithIndex.foreach({ case(row,j) => {
      row.zipWithIndex.foreach({case (cell,i) => {
        cell.io.reg_rst := (logic_cnt(4) === 0.U)
        cell.io.read_out_en := (logic_cnt(4) === 1.U)
    }})
  }})


  // Connect Cells 
  val prev = None
  cell_array.zipWithIndex.foreach({ case(row,j) => {
    row.zipWithIndex.foreach({case (cell,i) => {
      cell.io.a       := a_wire(i)
      cell.io.b       := b_wire(i)
      cell.io.c       := c_wire(i)
      cell.io.cell_en := io.en // hardcoded
      cell.io.macc_en := io.acc // hardcoded
      cell.io.cfg_en  := io.cfg_en // hardcoded
    }})
  }})

  // Connect for systolic read out
  cell_array(0).foreach(cell => cell.io.read_in := 0.S)
  for (j <- 1 until numel_A) {
    for (i <- 0 until numel_B) {
      cell_array(j)(i).io.read_in := cell_array(j-1)(i).io.out
    }
  }


  // val row_sums = cell_array.map(m => m.foldRight(0.S)((x, y) => x.io.out + y))
  // val div = (dLen/params.A_width)
  // val output = row_sums.grouped(div).foldRight(Seq.fill(dLen/params.A_width)(0.S(params.A_width.W)))((l0, l1) => (l0,l1).zipped.map(_+_))
  // io.out := output.reduceRight(_ ## _)
  val last_row_outputs = cell_array(numel_A-1).map(cell => cell.io.out.asUInt)
  io.out := last_row_outputs.reduceRight(_ ## _).asSInt
}

