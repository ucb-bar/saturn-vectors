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
  val detectTininess : Bool,

  // val types : Seq[OPUTypes],
  val cluster : Boolean = false,
  val cluster_xdim : Int = 4,
  val cluster_ydim : Int = 4,
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
                        val w0 = log2Ceil(params.n_mrf_regs)
                        val w1 = log2Ceil(regs_per_mrf_reg)
                        print(s"w0 {$w0}, w1: ${w1}  ")
                        println(s"MRF: ${mreg} UREG: ${ureg} Global: ${(mreg << w1) | ureg} ")
                        (((mreg << w1) + ureg).U(3.W) === io.mrf_idx) -> mregs(mreg)(ureg)
                      })
                    })
  mrf_out_demux := MuxCase(0.S, demux_logic.flatten)

  // Data going into MRF
  mregs.zipWithIndex.map({ case (mrf, mrf_idx) => {
    mrf.zipWithIndex.map({ case(reg, reg_idx) => {
      val w0 = log2Ceil(params.n_mrf_regs)
      val w1 = log2Ceil(regs_per_mrf_reg)
      val mrf_glbl_idx = Cat(mrf_idx.U(w0.W), reg_idx.U(w1.W))
      when(io.row_en && !io.readout && (io.mrf_idx === mrf_glbl_idx)) {
        reg := MuxCase(reg, Array(  io.reg_rst  -> 0.S, 
                                    io.load     -> io.in1,
                                    io.macc_en  -> sum))//,
                                    // ~io.macc_en    -> prod)) 
      }
    }})
  }})

  // Pipeline register
  read_pp := MuxCase(read_pp, Array(
                              (!io.row_en && io.readout) -> io.read_in, 
                              (io.row_en  && io.readout) -> mrf_out_demux))
  io.out := read_pp // Output selected 
}


// Can load C using ports of OPU cell
// class OuterProductUnit(params: OPUParameters)(implicit p : Parameters) extends CoreModule()(p) with HasVectorParams  {
class OuterProductUnit(params: OPUParameters)(implicit p : Parameters) extends IterativeFunctionalUnit()(p)  {
  // Control signals from sequencer
  val cntrl_io = IO(new OuterProductIO(params, dLen, egsTotal, egsPerVReg))

  // Extract data from issued instruction
  val in0 = WireInit(UInt(dLen.W), 0.U)
  val in1 = WireInit(UInt(dLen.W), 0.U)
  val rmask = WireInit(UInt(dLenB.W), 0.U)
  val wmask = WireInit(UInt(dLenB.W), 0.U)
  // Segment inputs for easier manipulation
  val numel_A = dLen/params.A_width
  val numel_B = dLen/params.B_width
  val numel_C = dLen/params.C_width
  val xdim = numel_B/params.cluster_xdim
  val ydim = numel_A/params.cluster_ydim
  val cluster_in0_bits = params.cluster_ydim*params.A_width
  val cluster_in1_bits = params.cluster_ydim*params.B_width
  val growth_factor = params.C_width/params.B_width
  val in0_a_wire = WireInit(VecInit(Seq.fill(ydim)(0.U(cluster_in0_bits.W)))) // Reg(VecInit[UInt(params.A_width.W)](VLEN/params.A_width, UInt(params.A_width.W)))
  val in1_b_wire = WireInit(VecInit(Seq.fill(xdim)(0.U(cluster_in1_bits.W)))) // Reg(VecInit[UInt(params.A_width.W)](VLEN/params.B_width, UInt(params.B_width.W)))
  val in0_c_wire = WireInit(VecInit(Seq.fill(numel_C)(0.S(params.C_width.W)))) // Reg(VecInit[UInt(params.A_width.W)](VLEN/params.B_width, UInt(params.B_width.W)))
  val in1_c_wire = WireInit(VecInit(Seq.fill(numel_C)(0.S(params.C_width.W)))) // Reg(VecInit[UInt(params.A_width.W)](VLEN/params.B_width, UInt(params.B_width.W)))



  // Segement input 1 vector into elements for reordering
  val in1_ele_inorder = (0 until numel_B).map(i => {
    val hi = (i+1)*params.B_width - 1
    val lo = i*params.B_width
    io.iss.op.rvs2_data(hi, lo)
  })
  val in1_shuffle = (0 until numel_B).map(ele => {
    val new_idx = floor(ele*8/32) + (ele*8) % 32
    in1_ele_inorder(new_idx.toInt)
  })

  // Reorder input columns for bette readout if VLEN != DLEN
  val output_fold = xdim/growth_factor
  // val in1_reorder = Wire(Vec(output_fold, UInt(params.B_width.W)))



  in0   := io.iss.op.rvs1_data
  // in1   := io.iss.op.rvs2_data
  in1   := in1_shuffle.reduceRight((left, right) => right ## left)
  wmask := io.iss.op.wmask
  rmask := io.iss.op.rmask




  val row_read_index = RegInit(UInt(2.W), 0.U)
  val cell_array = Seq.fill(ydim, xdim){Module(new OuterProductCluster(params))}
  
  // val num_sheets     = 4
  // val sheets_color0 = Seq.fill(8, num_sheets){Module(new OuterProductCluster(params))}
  // val sheets_color1 = Seq.fill(8, num_sheets){Module(new OuterProductCluster(params))}

  // Wire inputs as A/B row/cols to cells
  for (i <- (0 until ydim)) {
    var hi = ((i+1)*cluster_in0_bits - 1)
    var lo = ((i)*cluster_in0_bits)
    in0_a_wire(i) := in0(hi, lo)
  }
  for (i <- (0 until xdim)) {
    var hi = ((i+1)*cluster_in1_bits - 1)
    var lo = ((i)*cluster_in1_bits)
    in1_b_wire(i) := in1(hi, lo)
  }
  // Wire inputs a C value to cells
  for (i <- (0 until numel_C)) {
    var hi = ((i+1)*params.C_width - 1)
    var lo = ((i)*params.C_width)
    in0_c_wire(i) := in0(hi, lo).asSInt
    in1_c_wire(i) := in1(hi, lo).asSInt 
  }

  val cluster_row_en  = (0 until ydim).map(c => cntrl_io.row_en.slice(c*params.cluster_ydim, (c+1)*params.cluster_ydim))
  val cluster_read_en = (0 until ydim).map(c => cntrl_io.read_en.slice(c*params.cluster_ydim, (c+1)*params.cluster_ydim))

  // Connect Cells 
  cell_array.zipWithIndex.foreach({ case(cell_row, row_idx) => {
    cell_row.zipWithIndex.foreach({ case(cell, col_idx) => {
      val load_grp = (col_idx/numel_C).toInt
      // Wire Controls
      cell.io.row_en    := cluster_row_en(row_idx)   // Enable rows
      cell.io.read_en   := cluster_read_en(row_idx)  // One hot; Indicates which row is being read out
      cell.io.load      := cntrl_io.load             // Indicates if cell is loading vlaue
      cell.io.mrf_idx   := cntrl_io.mrf_idx          // Just to initial for compilation
      cell.io.macc_en   := cntrl_io.macc_en          // Optional; hardcoded
      cell.io.cnfg_en   := cntrl_io.cnfg_en          // Optional; hardcoded
      cell.io.reset     := cntrl_io.reset

      // Wire inputs
      cell.io.in0 := in0_a_wire(row_idx)
      cell.io.in1 := in1_b_wire(col_idx) //Mux(cntrl_io.load(load_grp), in1_c_wire(col_idx%numel_C), in1_b_wire(col_idx))
      dontTouch(cell.io)
    }})
  }})

  // Connect for systolic read out
  cell_array(0).foreach(cell => cell.io.read_in := 0.S)
  for (j <- 1 until ydim) {
    for (i <- 0 until xdim) {
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
  // val read_sub_vector = Wire(Vec(num_reads, UInt(dLen.W)))
  // val last_row_outputs = cell_array(numel_A-1).map(cell => cell.io.out.asUInt).reduceLeft(_ ## _)
  // for (i <- 0 until num_reads) { read_sub_vector(i) := last_row_outputs((i+1)*dLen-1, i*dLen) }

  // println(s"growth_factor: ${growth_factor}")
  // val last_row = cell_array.last
  // val cells_per_read = numel_B/growth_factor
  // val read_out_wire = (0 until growth_factor).map(i => {
  //   val sub_row = last_row.slice(i*cells_per_read, (i+1)*cells_per_read)
  //   // println(s"slice:(${(i+1)*cells_per_read-1}, ${i*cells_per_read})")
  //   println(s"tmp0: ${cell_array.last}")
  //   println(s"tmp1: ${sub_row}")
  //   (cntrl_io.load.asUInt === i.U) -> sub_row.map(cell => cell.io.out.asUInt).reduceRight((left, right) => right ## left)
  // })
  // row_read_index := cntrl_io.mrf_idx  // Will truncate correctly, but should explicitly bit slice as mrf_idx is for full MRF, but folding MRF over array



  io.write.valid     := cntrl_io.write_val
  io.write.bits.eg   := cntrl_io.write_eg
  io.write.bits.mask := wmask
  // io.write.bits.data := MuxCase(0.U, read_out_wire)
  io.write.bits.data := cell_array.last.map(cell => cell.io.out.asUInt).reduceRight((left, right) => right ## left)

    // read_out_wire(cntrl_io.load)

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
  
  val YOU_SHALL_PASS = Wire(Bool())
  YOU_SHALL_PASS := in0(0) & in1(1)
  dontTouch(YOU_SHALL_PASS)
}




// Can load C using ports of OPU cell
// class OuterProductUnit(params: OPUParameters)(implicit p : Parameters) extends CoreModule()(p) with HasVectorParams  {
class OuterProductCluster(params: OPUParameters)(implicit p : Parameters) extends CoreModule()(p) with HasVectorParams {
  // Scala Constants 
  // val dLen              = params.dLen
  // val varch_ratio       = vLen/params.dLen
  val nmrf              = params.n_mrf_regs
  val prod_width        = params.A_width + params.B_width
  val regs_per_mrf_reg  = scala.math.pow(egsPerVReg, 2).toInt
  val ntotal_regs       = params.n_mrf_regs*regs_per_mrf_reg

  val io = IO(new Bundle{
    val in0      = Input(UInt((params.cluster_ydim*params.A_width).W))
    val in1      = Input(UInt((params.cluster_ydim*params.B_width).W))
    val out      = Output(SInt(params.C_width.W))
    val read_in  = Input(SInt((params.cluster_xdim*params.C_width).W)) // Read out support


    val mrf_idx  = Input(UInt(log2Ceil(nmrf*regs_per_mrf_reg).W))
    val load     = Input(Vec(params.cluster_xdim, Bool()))
    val row_en   = Input(Vec(params.cluster_ydim, Bool()))
    val read_en  = Input(Vec(params.cluster_ydim, Bool()))
    val macc_en  = Input(Bool())
    val cnfg_en  = Input(Bool())
    val reset    = Input(Bool())
  })

  // OPU cell cluster
  val cluster = Seq.fill(params.cluster_xdim, params.cluster_ydim){Module(new OuterProductCell(params))}

  // Segment inputs for easier manipulation
  val numel_A = params.cluster_ydim
  val numel_B = params.cluster_xdim
  val numel_C = (params.B_width*params.cluster_xdim)/params.C_width
  val growth_factor = params.C_width/params.B_width
  val in0_a_wire = WireInit(VecInit(Seq.fill(numel_A)(0.S(params.A_width.W)))) // Reg(VecInit[UInt(params.A_width.W)](VLEN/params.A_width, UInt(params.A_width.W)))
  val in1_b_wire = WireInit(VecInit(Seq.fill(numel_B)(0.S(params.B_width.W)))) // Reg(VecInit[UInt(params.A_width.W)](VLEN/params.B_width, UInt(params.B_width.W)))

  for (i <- (0 until numel_A)) {
    var hi = ((i+1)*params.A_width - 1)
    var lo = ((i)*params.A_width)
    in0_a_wire(i) := io.in0(hi, lo).asSInt
  }
  for (i <- (0 until numel_B)) {
    var hi = ((i+1)*params.B_width - 1)
    var lo = ((i)*params.B_width)
    in1_b_wire(i) := io.in1(hi, lo).asSInt
  }

  // Connect control signals to cells
  cluster.zipWithIndex.foreach({ case(cell_row, row_idx) => {
    cell_row.zipWithIndex.foreach({ case(cell, col_idx) => {
      // Wire Controls
      cell.io.row_en    := io.row_en(row_idx)   // Enable rows
      cell.io.readout   := io.read_en(row_idx)  // One hot; Indicates which row is being read out
      cell.io.load      := io.load(col_idx)     // Indicates if cell is loading vlaue
      cell.io.mrf_idx   := io.mrf_idx           // Just to initial for compilation
      cell.io.macc_en   := io.macc_en           // Optional; hardcoded
      cell.io.cfg_en    := io.cnfg_en           // Optional; hardcoded
      cell.io.reg_rst   := io.reset

      // Wire inputs
      cell.io.in0 := io.in0(row_idx).asSInt
      cell.io.in1 := Mux(io.load(col_idx), io.in1.asSInt, in1_b_wire(col_idx).asSInt)
      dontTouch(cell.io)
    }})
  }})

  // Connect for systolic read out
  cluster(0).foreach(cell => cell.io.read_in := io.read_in)
  for (j <- 1 until params.cluster_ydim) {
    for (i <- 0 until params.cluster_xdim) {
      cluster(j)(i).io.read_in := cluster(j-1)(i).io.out
    }
  }

  // Select cluster output based upon row and read group
  val mux_logic = cluster.zipWithIndex.map({ case(cell_row, row_idx) => {
                    cell_row.zipWithIndex.map({ case(cell, col_idx) => {
                      ((OHToUInt(io.read_en) === row_idx.U) && (OHToUInt(io.load) === col_idx.U)) -> cell.io.out
                    }})
                  }})
  io.out := MuxCase(0.S, mux_logic.flatten)

}
