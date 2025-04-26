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
  val A_width : Int = 8,
  val B_width : Int = 8,
  val C_width : Int = 32, // Accumulator size

  // val types : Seq[OPUTypes],
  val n_mrf_regs : Int = 1, 

  // To support floating point FMAs
  val vLen : Int,
  val dLen : Int,
  val roundingMode   : UInt,
  val detectTininess : Bool
)

/*
 * A single cell in the Outer Product Unit MACC array
 * Accumulation register alse serve as pipeline registers
 * during read out 
 */
class OuterProductCell(params : OPUParameters)(implicit p: Parameters) extends CoreModule{
  val io = IO(new Bundle{
    val in0 = Input(SInt(params.A_width.W)) // One input for mult
    val in1 = Input(SInt(params.C_width.W)) // One input for mult/value to load accummulator register
    val out = Output(SInt(params.C_width.W))

    // val op          = Input(Bool())   // Asserted with should be operating
    val op_load     = Input(Bool())   // Asserted loads accumulator from in1; deasserted means the cell is in operating mode should never be asserted with op input
    val read_in     = Input(SInt(params.C_width.W)) // Read out support

    val mrf_idx     = Input(UInt(log2Ceil(params.n_mrf_regs).W))

    val cell_en     = Input(Bool())   // Turn off cell; Controlled by mask and (?; for power saving features)
    val macc_en     = Input(Bool())   // Asserted performs MACC, deasserted just multiplication
    val cfg_en      = Input(Bool())   // Asserted loads config registers
    val reg_rst     = Input(Bool())   // Asserted zeros MRF registers
    val read_out_en = Input(Bool())   // Asserted indicates readout in progress (output readout regiseter and load from read_in)
  })

  // Scala Constants 
  val dLen           = params.dLen
  val varch_ratio    = vLen/params.dLen
  val prod_width     = params.A_width + params.B_width
  val regs_per_mrf_reg = scala.math.pow(varch_ratio, 2).toInt

  // Signals
  val sum     = Wire(SInt(params.C_width.W))  // Make width dynamic
  val prod    = Wire(SInt(prod_width.W))      // Make width dynamic
  val macc_en = RegInit(Bool(), false.B)      // Latch configuration register
  // Muxes
  val read_pp_mux   = Wire(SInt(params.C_width.W))
  val mrf_in_mux    = Wire(SInt(params.C_width.W))
  val mrf_out_demux = Wire(SInt(params.C_width.W))
  val mrf_reg_demux = Wire(Vec(params.n_mrf_regs, SInt(params.C_width.W)))   // Mux per MRF reg
  val cell_out_mux  = Wire(SInt(params.C_width.W))
  // Matrix Register + Logic
  val mrf_ind       = io.mrf_idx
  val mregs         = Seq.fill(params.n_mrf_regs)(Reg(Vec(regs_per_mrf_reg, SInt(params.C_width.W))))
  val sub_mreg_ind  = RegInit(UInt(log2Ceil(regs_per_mrf_reg).W), 0.U)
  // Pipeline register for readout 
  val read_pp = Reg(SInt(params.C_width.W))
  // If buffering (Unecessary if using VRF as buffer; true functional unit)
  val vs1 = Reg(Vec(varch_ratio, UInt(params.dLen.W)))
  val vs2 = Reg(Vec(varch_ratio, UInt(params.dLen.W)))

  // // Load configure settings for op
  // when (io.cfg_en === true.B) {
  //   macc_en  := io.macc_en
  // }

  // Sequencing logic if VLEN == DLEN
  when (io.op_load) {
    when (sub_mreg_ind === (regs_per_mrf_reg-1).U) {
      sub_mreg_ind := 0.U
    } otherwise {
      sub_mreg_ind := sub_mreg_ind + 1.U
    }
  }

  // TODO: Padded inputs to large width of respective datatype (i.e. int8 to int32)
  //       This needs to be dynamic and data type is programmable

  // TODO: Need to check for overflow and saturate to accumulator width
  prod := io.in0*io.in1(7, 0) // Save to intermediate to force overflow
  sum  := prod + mrf_out_demux

  // Select correct sub-register from MRF register
  (0 until params.n_mrf_regs).map(mreg => {
    val mux_logic = (0 until regs_per_mrf_reg).map(j => (j.U === sub_mreg_ind) -> mregs(mreg)(j))
    mrf_reg_demux(mreg) :=  MuxCase(0.S, mux_logic)
  })

  // Data going into MRF
  mrf_in_mux := MuxCase(0.S, Array( io.reg_rst  -> 0.S, 
                                    io.op_load  -> io.in1,
                                    macc_en     -> sum,
                                    ~macc_en    -> prod)) 
  // Programmatically create array to construct decoder
  val mux_logic = (0 until params.n_mrf_regs).map(j => (j.U === mrf_ind) -> mrf_reg_demux(j))
  mrf_out_demux := MuxCase(0.S, mux_logic)

  read_pp_mux  := Mux(io.read_out_en, io.read_in, mrf_out_demux)
  cell_out_mux := read_pp // For simplest pipelined readout always read from register // Mux(io.read_out_en, io.read_in, read_pp)
  read_pp := read_pp_mux
  
  // Iterate over Scala structure and make HW assignments
  (0 until params.n_mrf_regs).map(mreg_ind => {
    mregs(mreg_ind).zipWithIndex.foreach({ case (reg, ind) => {
      when (ind.U === sub_mreg_ind && io.cell_en && io.mrf_idx === mreg_ind.U) {
        reg := mrf_in_mux
      }
    }})
  })

  // // Programmatically create array to construct decoder
  // val mux_contents = (0 until regs_per_mrf_reg).map(j => (j.U === sub_mreg_ind) -> sub_mreg(j))
  // regout_mux := MuxCase(0.S, mux_contents)



  // regout_mux := sub_mreg(sub_mreg_ind)
  io.out := cell_out_mux // Output selected 
}


// Can load C using ports of OPU cell
class OuterProductUnit(params: OPUParameters)(implicit p : Parameters) extends CoreModule()(p) with HasVectorParams  {
  val io = IO(new Bundle{
    val in0    = Input(UInt(dLen.W))
    val in1    = Input(UInt(dLen.W))
    val out    = Output(SInt(dLen.W))
    val rd_out = Input(Bool())

    val load   = Input(Bool())
    val en     = Input(Bool())
    val acc    = Input(Bool())
    val cfg_en = Input(Bool())
    val msel   = Input(UInt(2.W))
  })

  // Segment inputs for easier manipulation
  val numel_A    = dLen/params.A_width
  val numel_B    = dLen/params.B_width
  val numel_C    = dLen/params.C_width
  val in0_a_wire = WireInit(VecInit(Seq.fill(numel_A)(0.S(params.A_width.W)))) // Reg(VecInit[UInt(params.A_width.W)](VLEN/params.A_width, UInt(params.A_width.W)))
  val in1_b_wire = WireInit(VecInit(Seq.fill(numel_B)(0.S(params.B_width.W)))) // Reg(VecInit[UInt(params.A_width.W)](VLEN/params.B_width, UInt(params.B_width.W)))
  val in0_c_wire = WireInit(VecInit(Seq.fill(numel_C)(0.S(params.C_width.W)))) // Reg(VecInit[UInt(params.A_width.W)](VLEN/params.B_width, UInt(params.B_width.W)))
  val in1_c_wire = WireInit(VecInit(Seq.fill(numel_C)(0.S(params.C_width.W)))) // Reg(VecInit[UInt(params.A_width.W)](VLEN/params.B_width, UInt(params.B_width.W)))

  val row_read_index = RegInit(UInt(2.W), 0.U)
  val cell_array     = Seq.fill(numel_A, numel_B){Module(new OuterProductCell(params))}

  // Wire inputs as A/B row/cols to cells
  for (i <- (0 until numel_A)) {
    var hi = ((i+1)*params.A_width - 1)%dLen
    var lo = ((i)*params.A_width)%dLen
    in0_a_wire(i) := io.in0(hi, lo).asSInt
    println(s"a_wire($i): [$hi, $lo] $dLen \n")
  }
  for (i <- (0 until numel_B)) {
    var hi = ((i+1)*params.B_width - 1)%dLen
    var lo = ((i)*params.B_width)%dLen
    in1_b_wire(i) := io.in1(hi, lo).asSInt
  }

  // Wire inputs a C value to cells
  for (i <- (0 until numel_C)) {
    var hi = ((i+1)*params.C_width - 1)%dLen
    var lo = ((i)*params.C_width)%dLen
    in0_c_wire(i) := io.in0(hi, lo).asSInt
    in1_c_wire(i) := io.in1(hi, lo).asSInt
  }

  // Logic to drive array (just a fake implementation)
  val state = RegInit(UInt(1.W), 0.U)
  val logic_cnt = RegInit(UInt((256/8).W), 0.U)
  when(state === 0.U && io.en === true.B) {
    state := 1.U
  } otherwise {
    when (logic_cnt === (16*8 - 1).U) {
      state := 0.U
      logic_cnt := 0.U
    } otherwise {
      logic_cnt := logic_cnt + 1.U
    }
  }

  cell_array.zipWithIndex.foreach({ case(row,j) => {
      row.zipWithIndex.foreach({case (cell,i) => {
        cell.io.reg_rst := false.B // (logic_cnt(4) === 0.U)
        cell.io.read_out_en := (logic_cnt(1,0) === 3.U)
    }})
  }})

  // Connect Cells 
  cell_array.zipWithIndex.foreach({ case(row,j) => {
    row.zipWithIndex.foreach({case (cell,i) => {
      cell.io.in0 := in0_a_wire(j)
      // if (j % 2 == 1) { 
      //   cell.io.in1 := Mux(io.load, in0_c_wire(i%numel_C), in1_b_wire(i))
      // } else {
        cell.io.in1 := Mux(io.load, in1_c_wire(i%numel_C), in1_b_wire(i))
        println(s"Index in0_c_wire for cell_${j}_${i}: ${i%numel_C}")
      // }
      cell.io.cell_en := io.en      // hardcoded
      cell.io.macc_en := io.acc     // hardcoded
      cell.io.cfg_en  := io.cfg_en  // hardcoded
      cell.io.op_load := io.load    // hardcoded
      cell.io.mrf_idx := logic_cnt(0)   // Just to initial for compilation
    }})
  }})

  // Connect for systolic read out
  cell_array(0).foreach(cell => cell.io.read_in := 0.S)
  for (j <- 1 until numel_A) {
    for (i <- 0 until numel_B) {
      cell_array(j)(i).io.read_in := cell_array(j-1)(i).io.out
    }
  }

  val num_reads = (numel_C*params.C_width/dLen)
  val read_sub_vector = Wire(Vec(num_reads, UInt(dLen.W)))
  val last_row_outputs = cell_array(numel_A-1).map(cell => cell.io.out.asUInt).reduceRight(_ ## _)
  for (i <- 0 until num_reads) {
    read_sub_vector(i) := last_row_outputs((i+1)*dLen-1, i*dLen)
  }
  row_read_index := logic_cnt(1,0)
  io.out := read_sub_vector(row_read_index).asSInt
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