package saturn.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import chisel3.util.experimental.decode._
import saturn.common._
import saturn.backend._
import hardfloat._
import scala.math._

// Declare supported types (dictates multipler instantiated)
object OPUTypes extends Enumeration { val INT32, INT16, INT8 = Value }

// Parameters for configured OPU
case class OPUParameters (
  val A_width : Int = 8,
  val B_width : Int = 8,
  val C_width : Int = 32, // Accumulator size

  val n_mrf_regs : Int = 1, 
  val nrows : Int = 1,

  // To support floating point FMAs
  val vLen : Int,
  val dLen : Int,
  val roundingMode   : UInt,
  val detectTininess : Bool

  // val types : Seq[OPUTypes],
  // val cluster : Boolean = false,
  // val cluster_xdim : Int = 4,
  // val cluster_ydim : Int = 4,
)


/*
 * A single cell in the Outer Product Unit MACC array
 * Accumulation register alse serve as pipeline registers
 * during read out 
 */
class OuterProductCell(params : OPUParameters)(implicit p: Parameters) extends CoreModule{
  // Scala Constants 
  val dLen           = params.dLen
  val varch_ratio    = vLen/params.dLen
  val prod_width     = params.A_width + params.B_width
  val regs_per_mrf_reg = scala.math.pow(varch_ratio, 2).toInt
  val ntotal_regs     = params.n_mrf_regs*regs_per_mrf_reg

  val io = IO(new Bundle{
    // Data signals
    val in0 = Input(SInt(params.A_width.W)) // One input for mult
    val in1 = Input(SInt(params.C_width.W)) // One input for mult/value to load accummulator register
    val out = Output(SInt(params.C_width.W))
    val read_in = Input(SInt(params.C_width.W)) // Read out support

    // Contol Signals
    val mrf_idx = Input(UInt(log2Ceil(regs_per_mrf_reg*params.n_mrf_regs).W)) // Index for µarch register to write
    val load    = Input(Bool())   // Asserted loads accumulator from in1; deasserted means the cell is in operating mode should never be asserted with op input
    val row_en  = Input(Bool())   // Turn off cell; Controlled by mask and (?; for power saving features)
    val readout = Input(Bool())   // Asserted indicates readout in progress (output readout regiseter and load from read_in)
    val macc_en = Input(Bool())   // Asserted performs MACC, deasserted just multiplication
    val cfg_en  = Input(Bool())   // Asserted loads config registers
    val reg_rst = Input(Bool())   // Asserted zeros MRF registers
  })

  // Signals
  val sum     = Wire(SInt(params.C_width.W))  // Make width dynamic
  val prod    = Wire(SInt(prod_width.W))      // Make width dynamic
  // Muxes
  // val mrf_in_mux    = Wire(SInt(params.C_width.W))
  val mrf_out_demux = Wire(SInt(params.C_width.W))
  // Matrix Register + Logic
  val mregs         = Seq.fill(params.n_mrf_regs)(Reg(Vec(regs_per_mrf_reg, SInt(params.C_width.W))))
  // Pipeline register for readout 
  val read_pp = Reg(SInt(params.C_width.W))
  // If buffering (Unecessary if using VRF as buffer; true functional unit)
  val vs1 = Reg(Vec(varch_ratio, UInt(params.dLen.W)))
  val vs2 = Reg(Vec(varch_ratio, UInt(params.dLen.W)))


  // TODO: Padded inputs to large width of respective datatype (i.e. int8 to int32)
  //       This needs to be dynamic and data type is programmable

  // TODO: Need to check for overflow and saturate to accumulator width
  prod := io.in0*io.in1(params.B_width-1, 0) // Save to intermediate to force overflow
  sum  := prod + mrf_out_demux

  // mrf_in_mux := 

  // Programmatically create array to construct decoder
  val demux_logic = (0 until params.n_mrf_regs).map(mreg => {
                      (0 until regs_per_mrf_reg).map(ureg => {
                        (Cat(mreg.U, ureg.U) === io.mrf_idx) -> mregs(mreg)(ureg)
                      })
                    })
  mrf_out_demux := MuxCase(0.S, demux_logic.flatten)

  // Data going into MRF
  mregs.zipWithIndex.map({ case (mrf, mrf_idx) => {
    mrf.zipWithIndex.map({ case(reg, reg_idx) => {
      val mrf_glbl_idx = Cat(mrf_idx.U, reg_idx.U)
      when(io.row_en && !io.readout && io.mrf_idx === mrf_glbl_idx) {
        reg := MuxCase(0.S, Array( io.reg_rst  -> 0.S, 
                                    io.load  -> io.in1,
                                    io.macc_en     -> sum,
                                    ~io.macc_en    -> prod)) 
      }
    }})
  }})

  // Pipeline register
  read_pp := Mux(io.readout, io.read_in, mrf_out_demux)
  io.out := read_pp // Output selected 
}


// Can load C using ports of OPU cell
// class OuterProductUnit(params: OPUParameters)(implicit p : Parameters) extends CoreModule()(p) with HasVectorParams  {
class OuterProductUnit(params: OPUParameters)(implicit p : Parameters) extends IterativeFunctionalUnit()(p)  {
  // val io = IO(new Bundle{
  //   val in0    = Input(UInt(dLen.W))
  //   val in1    = Input(UInt(dLen.W))
  //   val out    = Output(SInt(dLen.W))
  //   val rd_out = Input(Bool())

  //   val load   = Input(Bool())
  //   val row_en = Input(Bool())
  //   val acc    = Input(Bool())
  //   val cfg_en = Input(Bool())
  //   val msel   = Input(UInt(2.W))
  // })

  // Control signals from sequencer
  val cntrl_io = IO(new OuterProductIO(params, dLen, egsTotal, egsPerVReg))

  // Extract data from issued instruction
  val in0 = WireInit(UInt(dLen.W), 0.U)
  val in1 = WireInit(UInt(dLen.W), 0.U)
  val rmask = WireInit(UInt(dLenB.W), 0.U)
  val wmask = WireInit(UInt(dLenB.W), 0.U)
  when (io.iss.valid) {
    in0   := io.iss.op.rvs1_data
    in1   := io.iss.op.rvs2_data
    wmask := io.iss.op.wmask
    rmask := io.iss.op.rmask
    // The reamaining inputs aren't used
    // val rvd_data  = UInt(dLen.W)
    // val rvm_data  = UInt(dLen.W)
    // val rvs1_elem = UInt(64.W)
    // val rvs2_elem = UInt(64.W)
    // val rvd_elem  = UInt(64.W)
  }



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
    in0_a_wire(i) := in0(hi, lo).asSInt
  }
  for (i <- (0 until numel_B)) {
    var hi = ((i+1)*params.B_width - 1)%dLen
    var lo = ((i)*params.B_width)%dLen
    in1_b_wire(i) := in1(hi, lo).asSInt
  }
  // Wire inputs a C value to cells
  for (i <- (0 until numel_C)) {
    var hi = ((i+1)*params.C_width - 1)%dLen
    var lo = ((i)*params.C_width)%dLen
    in0_c_wire(i) := in0(hi, lo).asSInt
    in1_c_wire(i) := in1(hi, lo).asSInt 
  }

  // Connect Cells 
  cell_array.zipWithIndex.foreach({ case(cell_row, row_idx) => {
    cell_row.zipWithIndex.foreach({ case(cell, col_idx) => {
      val load_grp = (col_idx/numel_C).toInt
      // Wire Controls
      cell.io.row_en    := cntrl_io.row_en(row_idx)   // Enable rows
      cell.io.readout  := cntrl_io.read_en(row_idx)  // One hot; Indicates which row is being read out
      cell.io.load      := cntrl_io.load(load_grp)    // Indicates if cell is loading vlaue
      cell.io.mrf_idx   := cntrl_io.mrf_idx           // Just to initial for compilation
      cell.io.macc_en   := cntrl_io.macc_en           // Optional; hardcoded
      cell.io.cfg_en    := cntrl_io.cnfg_en           // Optional; hardcoded
      cell.io.reg_rst   := cntrl_io.reset

      // Wire inputs
      cell.io.in0 := in0_a_wire(row_idx)
      cell.io.in1 := Mux(cntrl_io.load(load_grp), in1_c_wire(col_idx%numel_C), in1_b_wire(col_idx))
    }})
  }})

  // Connect for systolic read out
  cell_array(0).foreach(cell => cell.io.read_in := 0.S)
  for (j <- 1 until numel_A) {
    for (i <- 0 until numel_B) {
      cell_array(j)(i).io.read_in := cell_array(j-1)(i).io.out
    }
  }


  // // Logic to drive array (just a fake implementation)
  // val state = RegInit(UInt(1.W), 0.U)
  // val logic_cnt = RegInit(UInt((256/8).W), 0.U)
  // when(state === 0.U && io.en === true.B) {
  //   state := 1.U
  // } otherwise {
  //   when (logic_cnt === (16*8 - 1).U) {
  //     state := 0.U
  //     logic_cnt := 0.U
  //   } otherwise {
  //     logic_cnt := logic_cnt + 1.U
  //   }
  // }
  // cell_array.zipWithIndex.foreach({ case(row,j) => {
  //     row.zipWithIndex.foreach({case (cell,i) => {
  //       cell.io.reg_rst := false.B // (logic_cnt(4) === 0.U)
  //       cell.io.read_out_en := (logic_cnt(1,0) === 3.U)
  //   }})
  // }})


  // True Move Out LOGIC ************************
  val ncols = dLen/params.B_width
  val num_reads = ncols*params.C_width/dLen
  val read_sub_vector = Wire(Vec(num_reads, UInt(dLen.W)))
  val last_row_outputs = cell_array(numel_A-1).map(cell => cell.io.out.asUInt).reduceLeft(_ ## _)
  for (i <- 0 until num_reads) { read_sub_vector(i) := last_row_outputs((i+1)*dLen-1, i*dLen) }
  row_read_index := cntrl_io.mrf_idx  // Will truncate correctly, but should explicitly bit slice as mrf_idx is for full MRF, but folding MRF over array

  io.write.valid     := cntrl_io.write_val
  io.write.bits.eg   := cntrl_io.write_eg
  io.write.bits.mask := wmask
  io.write.bits.data := read_sub_vector(row_read_index).asUInt

  // ********************************************

  io.stall := false.B
  io.acc  := false.B
  last := false.B // Might need to change this
  io.tail := false.B
  io.busy := false.B  // Placeholder
  io.hazard := DontCare  // In simple implementation hazard is solely performed by sequencer; limits performance 
  io.hazard.valid := false.B  // In simple implementation hazard is solely performed by sequencer; limits performance 
  io.scalar_write := DontCare
  io.scalar_write.valid := false.B // Ensure never asserted
  io.set_vxsat := false.B
  io.set_fflags.valid := false.B
  io.set_fflags.bits := DontCare

}




// // Can load C using ports of OPU cell
// // class OuterProductUnit(params: OPUParameters)(implicit p : Parameters) extends CoreModule()(p) with HasVectorParams  {
// class OuterCluster(params: OPUParameters)(implicit p : Parameters) extends IterativeFunctionalUnit()(p)  {
//   val io = = IO(new Bundle{
//     val in0      = Input(UInt((params.A_width*cluster_ydim).W))
//     val in1      = Input(UInt((params.B_width*cluster_xdim).W))
//     val out      = Output(SInt(dLen.W))

//     val read_en  = Input(Vec(params.cluster_xdim, Bool()))
//     val row_en   = Input(Vec(params.cluster_xdim, Bool()))
//     val load     = Input(Bool())
//     val mrf_idx  = Input(UInt(log2Ceil(nmrf*regs_per_mrf_reg).W))
//     val macc_en  = Input(Bool())
//     val cnfg_en  = Input(Bool())
//     val reset    = Input(Bool())
//   })

// }
