package saturn.backend

import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import saturn.common._
import saturn.insns._
import scala.math._
import saturn.exu._


class OuterProductIO(params : OPUParameters, dLen : Int, egsTotal : Int, egsPerVReg : Int) extends Bundle {
  val nmrf       = params.n_mrf_regs
  val nrows      = dLen/params.A_width
  val ncell_grps = params.C_width/min(params.A_width, params.B_width)
  val regs_per_mrf_reg = scala.math.pow(egsPerVReg, 2).toInt

  // Write Control
  val write_eg   = Input(UInt(log2Ceil(egsTotal).W)) // Forward which element to write to OPU from control logic
  val write_val  = Input(Bool())           // Forward write valid to OPU from contr ol logic
  val write_mask = Input(UInt(dLen.W))     // Forward write mask to OPU from control logic

  // Array control
  // val opu_en   = Input(Bool())              // DEPRECATED; Indicate to perform MACC
  val read_en  = Input(Vec(nrows, Bool()))
  val row_en   = Input(Vec(nrows, Bool()))
  val load     = Input(Vec(ncell_grps, Bool()))
  val mrf_idx  = Input(UInt(log2Ceil(nmrf*regs_per_mrf_reg).W))
  val macc_en  = Input(Bool())
  val cnfg_en  = Input(Bool())
  val reset    = Input(Bool())
}


// Create states for FSM
object State extends ChiselEnum { val IDLE, MVIN, MVOUT0, MVOUT1, OPROD = Value }

// 
class OuterProductUnitSequencerIO(params : OPUParameters, maxDepth: Int, nFUs: Int)(implicit p: Parameters) extends ExecuteSequencerIO(maxDepth, nFUs) {
  val opu_cntrl = Flipped(new OuterProductIO(params, dLen, egsTotal, egsPerVReg))
}


// This implementation assumes that the programmer sets LMUL to the correct value 
// prior to move-ins and move-outs

class OuterProductSequencer(params : OPUParameters, exu_insns: Seq[VectorInstruction])(implicit p: Parameters) extends Sequencer[ExecuteMicroOp]()(p) {
  import State._

  // Hardcode these for now
  val nFUs = 1
  val maxPipeDepth = 1 
  val min_dtype = 8
  val CLUSTER_SIZE = 1 // How many cells per cluster

  // INSTRUCTION ENCODING TODO: Figure out a better way to do this
  val MVIN_FUNCT6  = 0
  val MVOUT_FUNCT6 = 1
  val OPROD_FUNCT6 = 2
  val RESET_FUNCT6 = 3

  // Maintain Execution Sequencer IO
  val io = IO(new OuterProductUnitSequencerIO(params, maxPipeDepth, nFUs))

  // Ergonomic 
  val varch_ratio     = egsPerVReg                  // Rename because legacy
  val array_nrows     = dLen/params.A_width
  val array_ncols     = dLen/params.B_width
  val nloads_per_row  = params.C_width/params.B_width
  val opu_dim         = dLen/min_dtype              // Dimension of PE array 
  val min_inwidth     = Math.min(params.A_width, params.B_width)  
  val growth_factor   = params.C_width/min_inwidth  // Ratio between accumulator and input (TODO: expression is probably wrong for rectangular arrays)
  val mvin_vrf_reads  = varch_ratio*growth_factor   // Number of VRF writes to mvin row of OPU

  // Signals
  val mvin_enable  = WireInit(false.B)
  val mvout_enable = WireInit(false.B)
  val oprod_enable = WireInit(false.B)
  // val row_load     = Wire(Vec(opu_dim, Bool())) // Enables row to be loaded
  // val load         = Wire(Vec(growth_factor, Bool())) // One bit per group of cells written by one DLEM (total # of cells/# of cells written by DLEN)
  // FSM
  val ctrl_st      = RegInit(IDLE)
  val egidx0       = Reg(UInt(log2Ceil(egsPerVReg).W))
  val egidx1       = Reg(UInt(log2Ceil(egsPerVReg).W))
  val regidx       = Reg(UInt(log2Ceil(egsPerVReg).W))   // Index for internal register in matrix register row
  val op_egidx     = Reg(UInt((2*log2Ceil(egsPerVReg)).W))
  val load_idx     = Reg(UInt(log2Ceil(growth_factor * egsPerVReg).W))   // Index for internal register in matrix register row

  val load_cnt    = Reg(UInt(log2Ceil(growth_factor).W))
  val mvout_cnt   = Reg(UInt(log2Ceil(dLen/CLUSTER_SIZE).W))  // TODO: this assume no clustering
  val mvout_cnt_rows   = Reg(UInt(log2Ceil(egsPerVReg).W)) 
  val mvout_cnt_dlen   = Reg(UInt(log2Ceil(nloads_per_row).W)) 
  
  // Registers for extracting registers (ergonomic)
  val mvin_vreg   = Reg(UInt(5.W))
  val mvin_mrow   = Reg(UInt(log2Ceil(dLen/params.A_width).W)) // Row within the MRF not the cell array
  val mvin_mreg   = Reg(UInt(log2Ceil(params.n_mrf_regs).W))
  val mvout_vreg  = Reg(UInt(5.W))
  val mvout_mrow  = Reg(UInt(log2Ceil(dLen/params.A_width).W))
  val mvout_mreg  = Reg(UInt(log2Ceil(params.n_mrf_regs).W)) // MRF number
  val oprod_vreg0 = Reg(UInt(5.W))
  val oprod_vreg1 = Reg(UInt(5.W))
  val oprod_mreg  = Reg(UInt(log2Ceil(params.n_mrf_regs).W))



  // Default IO Settings
  io.acc_ready := false.B // Signal not used
  io.vgu <> DontCare
  io.pipe_write_req <> DontCare

  // From issue queue
  io.vat          <> DontCare
  io.head         <> DontCare 
  // io.vat_head     <> DontCare 
  // io.older_writes <> DontCare 
  // io.older_reads  <> DontCare 


  //********************************************************************
  // io.iss        = Decoupled(issType)
  // io.dis        = Flipped(Decoupled(new BackendIssueInst))
  // io.dis_stall  = Input(Bool()) // used to disable OOO
  // io.seq_hazard = Output(Valid(new SequencerHazard))
  //********************************************************************

  io.opu_cntrl.row_en := (0.U(array_nrows.W)).asBools
  io.opu_cntrl.read_en := (0.U(array_nrows.W)).asBools
  io.opu_cntrl.load := (0.U(growth_factor.W)).asBools

  // Vector Reag
  io.rvs1.valid := false.B
  io.rvs2.valid := false.B
  io.rvd.valid  := false.B
  io.rvm.valid  := false.B
  io.rvs1.bits  := DontCare
  io.rvs2.bits  := DontCare
  io.rvd.bits   := DontCare
  io.rvm.bits   := DontCare

  // Hazard
  io.seq_hazard.valid := true.B   // This is probably wrong
  io.seq_hazard.bits  := DontCare

  // Control
  io.opu_cntrl.write_val  := false.B
  io.opu_cntrl.write_eg   := DontCare
  io.opu_cntrl.write_mask := DontCare
  io.opu_cntrl.mrf_idx    := 0.U
  io.opu_cntrl.macc_en    := true.B
  io.opu_cntrl.cnfg_en    := false.B
  io.opu_cntrl.reset      := false.B

  // Dispatch

  io.seq_hazard.valid := false.B
  io.vat := DontCare

  // Issue 
  io.iss.valid := false.B 
  io.iss.bits <> DontCare   // Just to get the thing compiling
  io.iss.bits.fu_sel := 0.U
  io.iss.bits.eidx := 0.U
  io.iss.bits.vl := 0.U // TODO: Set this to a correct value
  io.iss.bits.rvs1_eew := 0.U //TODO: Do these need to be set 
  io.iss.bits.rvs2_eew := 0.U //TODO: Do these need to be set
  io.iss.bits.rvd_eew := 0.U
  io.iss.bits.vd_eew := 0.U
  io.iss.bits.sew := 0.U
  io.iss.bits.scalar := DontCare
  io.iss.bits.use_scalar_rvs1 := false.B
  io.iss.bits.use_zero_rvs2 := false.B
  io.iss.bits.use_slide_rvs2 := false.B



  // Main control FSM
  //  - when valid instruction 
  //  - decode intstruction and figure which operation
  //  - trigger FSM, and wait until done
  switch(ctrl_st) {
    is(IDLE) {
      when(io.dis.fire) {
        // io.dis.ready := true.B
        mvin_vreg   := io.dis.bits.rs1
        mvin_mrow   := io.dis.bits.rs2
        mvin_mreg   := io.dis.bits.rd
        mvout_vreg  := io.dis.bits.rs1
        mvout_mrow  := io.dis.bits.rs2
        mvout_mreg  := io.dis.bits.rd
        oprod_vreg0 := io.dis.bits.rs1
        oprod_vreg1 := io.dis.bits.rs2
        oprod_mreg  := io.dis.bits.rd

        ctrl_st := MuxCase(IDLE, Array( 
                          (io.dis.bits.funct6 === MVIN_FUNCT6.U)  -> MVIN,
                          (io.dis.bits.funct6 === MVOUT_FUNCT6.U) -> MVOUT0,
                          (io.dis.bits.funct6 === OPROD_FUNCT6.U) -> OPROD))
      } .otherwise {
        ctrl_st := IDLE 
        // io.dis.ready := false.B
      }

      // Reset counters
      egidx0   := 0.U
      egidx1   := 0.U
      load_cnt := 0.U
      load_idx := 0.U
      mvout_cnt := 0.U
      mvout_cnt_rows := 0.U
      mvout_cnt_dlen := 0.U

      // Stop VRF requests
      io.rvs1.valid := false.B
      io.rvs2.valid := false.B
      io.rvd.valid  := false.B
      io.rvm.valid  := false.B

      // No need to prioritize funct6 takes single value
      // mvin_enable  := io.dis.valid && (io.dis.bits.funct6 === MVIN_FUNCT6.U)
      // mvout_enable := io.dis.valid && (io.dis.bits.funct6 === MVOUT_FUNCT6.U)
      // oprod_enable := io.dis.valid && (io.dis.bits.funct6 === OPROD_FUNCT6.U)
      io.opu_cntrl.reset := io.dis.valid && (io.dis.bits.funct6 === RESET_FUNCT6.U)
    }

    is(MVIN) {

      load_idx := Mux(io.rvs1.fire, load_idx + 1.U, load_idx)
      ctrl_st  := Mux(load_idx === (nloads_per_row-1).U, IDLE, MVIN)

      io.opu_cntrl.row_en(mvin_mrow) := io.rvs1.fire
      io.opu_cntrl.read_en           := WireInit(VecInit(Seq.fill(array_nrows)(false.B)))
      io.opu_cntrl.mrf_idx           := Cat(mvin_mreg, mvin_mrow/array_nrows.U + load_idx)
      io.opu_cntrl.load              := UIntToOH(load_idx(log2Ceil(nloads_per_row)-1, 0) + 1.U).asBools

      io.rvs2.valid   := (load_idx < (growth_factor * egsPerVReg).U)
      io.rvs2.bits.eg := getEgId(mvout_vreg, 0.U, 8.U, false.B) + load_idx
      mvin_enable := true.B

      // // NOTE: you are writing the entire vector 
      // when (load_idx === (growth_factor * egsPerVReg).U) {
      //   load_idx := 0.U
      //   ctrl_st := IDLE
      //   io.rvs1.valid := false.B
      // } otherwise {
      //   when(io.rvs1.fire) {
      //     load_idx := load_idx + 1.U
      //     // TODO: Pulse load signal for correct row + cell group
      //     // val mvin_egidx = RegInit(UInt(log2Ceil(params.C_width/params.B_width).W), load_idx(log2Ceil(params.C_width/params.B_width)-1, 0))
      //     io.opu_cntrl.load := UIntToOH(load_idx(log2Ceil(params.C_width/params.B_width)-1, 0)).asBools   // TODO: Modulo for quick development; remove in improved version
      //   } otherwise {
      //     io.opu_cntrl.load := UIntToOH(0.U(log2Ceil(params.C_width/params.B_width).W)).asBools
      //   }

      //   // io.row_load  := UIntToOH(mvin_mrow)
      //   io.rvs2.bits.eg := load_idx
      //   io.rvs2.valid := true.B
      // }
      // io.iss.valid :=  io.rvs1.fire

    }

    // // Orignal: Non-functional
    // is(MVOUT0){
    //   mvout_enable := true.B
    //   when (mvout_cnt === (mvout_mrow + varch_ratio.U) - 1.U) {
    //     ctrl_st := IDLE
    //   } .otherwise {
    //     mvout_cnt := mvout_cnt + 1.U
    //   }

    //   regidx := Mux(mvout_cnt < varch_ratio.U, Cat(mvout_mreg, mvout_mrow + mvout_cnt), regidx)   // SLOPPY BUT WHATEVER
    //   io.opu_cntrl.row_en   := UIntToOH(mvout_mrow(log2Ceil(array_nrows)-1, 0)).asBools       // Indicate that row being read
    //   io.opu_cntrl.read_en  := (-1).S(io.opu_cntrl.read_en.getWidth.W).asBools                    // Cells that they are reading
    //   io.opu_cntrl.mrf_idx  := Cat(mvout_mreg, mvout_mrow + regidx)                               // Indicate the MRF register and row in that MRF
    //   io.opu_cntrl.load     := WireInit(VecInit(Seq.fill(ncell_grps)(false.B))))

    //   // TODO: Set Value for VectorPipeWriteReqIO
    //   // TODO: Indicate write hazard. Assert write hazard for entire vector register (all element groups in vector register)
      
    //   // io.older_writes := FillInterleaved(varch_ratio, true.B) << (mvout_vreg * varch_ratio.U) // TODO: Should do multiplication by two with shift to avoid overflow

    //   io.opu_cntrl.write_val  := (mvout_cnt >= mvout_mrow) // Rows zero-indexed 
    //   io.opu_cntrl.write_eg   := getEgId(mvout_vreg, 0.U, 8.U, false.B)
    //   io.opu_cntrl.write_mask := (-1).S(io.opu_cntrl.write_mask.getWidth.W).asUInt  // TODO: Base off of AVL
    //   io.seq_hazard.valid     := true.B   // This is probably wrong
      
    //   val tmp = WireInit(VecInit(Seq.fill(egsTotal)(false.B)))
    //   val base_eg = getEgId(mvout_vreg, 0.U, 8.U, false.B)
    //   for (i <- 0 to egsPerVReg) {
    //     tmp(base_eg + i.U) := true.B
    //   }
    //   io.seq_hazard.bits.wintent   := tmp.asUInt

    // }

    // This state just shifts first array tile data to bottom of array
    is(MVOUT0) {

      io.opu_cntrl.write_eg   := 0.U
      io.opu_cntrl.write_val  := false.B
      io.opu_cntrl.write_mask := 0.U

      io.opu_cntrl.load     := WireInit(VecInit(Seq.fill(io.opu_cntrl.load.length)(false.B)))
      val tmp = Reg(UInt(io.opu_cntrl.read_en.length.W))
      tmp :=((1.U(array_nrows.W) << mvout_mrow) - 1.U)
      io.opu_cntrl.read_en  := tmp.asBools        // Cells that they are reading
      io.opu_cntrl.row_en   := Mux(mvout_cnt < egsPerVReg.U, 
                                  WireInit(VecInit(UIntToOH(mvout_mrow(log2Ceil(array_nrows)-1, 0)).asBools)), // Only works for power of two, but f*ck it
                                  WireInit(VecInit(Seq.fill(array_nrows)(false.B))))       // Indicate that row being read
      io.opu_cntrl.mrf_idx  := Mux(mvout_cnt < egsPerVReg.U, 
                                  Cat(mvout_mreg, mvout_mrow/array_nrows.U + mvout_cnt), 
                                  0.U)                            // Indicate the MRF register and row in that MRF
      
      mvout_cnt := Mux(mvout_cnt === (mvout_mrow-2.U), 0.U, mvout_cnt + 1.U)
      ctrl_st   := Mux(mvout_cnt === (mvout_mrow-2.U), MVOUT1, MVOUT0)
      mvout_enable := true.B 

      // io.opu_cntrl.mrf_idx  := Cat(mvout_mreg, mvout_mrow + regidx)                               // Indicate the MRF register and row in that MRF


      
      // // Move data through pipeline registers
      // when (mvout_cnt === (mvout_mrow-1.U)) {
      //   ctrl_st   := MVOUT1
      //   mvout_cnt := 0.U
      // } .otherwise {
      //   mvout_cnt := mvout_cnt + 1.U
      // }

      // // TODO: Only activate row_en for row being read and those below
      // when (mvout_cnt < varch_ratio.U) {
      //   regidx := Cat(mvout_mreg, mvout_mrow + mvout_cnt)
      //   io.opu_cntrl.read_en  := UIntToOH(mvout_mrow(log2Ceil(array_nrows)-1, 0)).asBools       // Indicate that row being read
      // } .otherwise {
      //   regidx := regidx
      //   io.opu_cntrl.read_en := 0.U.asBools
      // }


      // io.opu_cntrl.mrf_idx  := regidx                            // Indicate the MRF register and row in that MRF
      // io.opu_cntrl.row_en   := ((1.U << mvout_mrow) - 1.U).asBools        // Cells that they are reading
      // // regidx := Mux(mvout_cnt < varch_ratio.U, Cat(mvout_mreg, mvout_mrow + mvout_cnt), regidx)   // SLOPPY BUT WHATEVER
      // // io.opu_cntrl.read_en  := Mux(mvout_cnt < varch_ratio.U, UIntToOH(mvout_mrow(log2Ceil(array_nrows)-1, 0)).asBools       // Indicate that row being read

    }

    is(MVOUT1) {

      when (mvout_cnt_rows === (egsPerVReg-1).U) {
        ctrl_st := IDLE
        mvout_cnt_rows := 0.U
        mvout_cnt_dlen := 0.U
        io.opu_cntrl.read_en := WireInit(VecInit(Seq.fill(array_nrows)(false.B)))
      } otherwise {
        when (mvout_cnt_dlen === (nloads_per_row-1).U) {
          mvout_cnt_rows := mvout_cnt_rows + 1.U
          mvout_cnt_dlen := 0.U
          io.opu_cntrl.read_en := WireInit(VecInit(Seq.fill(array_nrows)(true.B)))
        } otherwise {
          mvout_cnt_dlen := mvout_cnt_dlen + 1.U
          io.opu_cntrl.read_en := WireInit(VecInit(Seq.fill(array_nrows)(false.B)))
        }
      }

      mvin_enable := true.B 
      io.opu_cntrl.load     := UIntToOH(mvout_cnt_dlen).asBools
      io.opu_cntrl.row_en   := WireInit(VecInit(Seq.fill(array_nrows)(false.B)))
      io.opu_cntrl.write_val  := (mvout_cnt >= mvout_mrow) // Rows zero-indexed 
      io.opu_cntrl.write_eg   := getEgId(mvout_vreg, 0.U, 8.U, false.B)
      io.opu_cntrl.write_mask := (-1).S(io.opu_cntrl.write_mask.getWidth.W).asUInt  // TODO: Base off of AVL
    }

    is(OPROD){


      when (op_egidx === (2*egsPerVReg-1).U) {
        ctrl_st := IDLE
      }

      when (io.rvs1.fire && io.rvs2.fire) {
        op_egidx := op_egidx + 1.U
        io.opu_cntrl.row_en := ((-1).S(array_nrows.W)).asBools
      } .otherwise {
        op_egidx := op_egidx
        io.opu_cntrl.row_en := (0.U(array_nrows.W)).asBools
      }




      egidx0 := op_egidx(log2Ceil(egsPerVReg)-1, 0)
      egidx1 := op_egidx(2*log2Ceil(egsPerVReg)-1, log2Ceil(egsPerVReg))

      // Load element group from vector
      io.rvs1.valid   := true.B
      io.rvs2.valid   := true.B
      io.rvs1.bits.eg := egidx0
      io.rvs2.bits.eg := egidx1
      io.iss.valid    := io.rvs1.fire && io.rvs2.fire
      oprod_enable    := true.B

    }
  }

  def accepts(inst: VectorIssueInst) : Bool = !inst.vmu 


  io.opu_cntrl.macc_en := true.B
  io.opu_cntrl.cnfg_en := false.B
  io.iss.valid := false.B // Combination of row_en, load, and readout serve as valids
  io.dis.ready := !(mvin_enable | mvout_enable | oprod_enable)
  io.busy      := !(mvin_enable | mvout_enable | oprod_enable)
}



// // Move-Ins take priority
// mvin_enable  := (io.dis.bits.funct6 == MVIN_FUNCT6)
// mvout_enable := (io.dis.bits.funct6 == MVOUT_FUNCT6) && !mvin_enable
// oprod_enable := (io.dis.bits.funct6 == OPROD_FUNCT6) && !mvin_enable && !mvout_enable

// Move-In FSM (ReadIn)
//    Assume Full MRF Load or Partial Load? 
//     - Would need to support in instruction encoding 
//    Access DLEN per cycle up until the number of rows/columns 
// val opu_row_grp = Reg(UInt(log2Ceil(mvin_vrf_reads).W))

// Extract the vector register
// Extract the MRF & row #

// Move-Out FSM (Readout)
// OuterProduct FSM
//    VRF Access
//    - Cycle 1: Two A0 - B0
//    - Cycle 2: Two A0 - B1
//    - Cycle 3: Two A1 - B1
//    - Cycle 4: Two A1 - B0
