package booster

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.rocket.Instructions._


// object VFrontendDecodeTable
// {
//   def loadStore(aluFn: ALUFN = ALUFN()): Array[(BitPat, List[BitPat])] = Array(
//                         //           jal                                                                renf1               fence.i
//                         //   val     | jalr                                                             | renf2             |
//                         //   | fp_val| | renx2                                                          | | renf3           |
//                         //   | | rocc| | | renx1             s_alu1                       mem_val       | | | wfd           |
//                         //   | | | br| | | | scie      s_alu2|   imm   dw     alu         | mem_cmd     | | | | mul         |
//                         //   | | | | | | | | | zbk     |     |   |     |      |           | |           | | | | | div       | fence
//                         //   | | | | | | | | | | zkn   |     |   |     |      |           | |           | | | | | | wxd     | | amo
//                         //   | | | | | | | | | | | zks |     |   |     |      |           | |           | | | | | | |       | | | dp
//     VLE8_V           -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VLE16_V          -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VLE32_V          -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VLE64_V          -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VLE8FF_V         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VLE16FF_V        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VLE32FF_V        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VLE64FF_V        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VLUXEI8_V        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VLUXEI16_V       -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VLUXEI32_V       -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VLUXEI64_V       -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VLSE8_V          -> List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VLSE16_V         -> List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VLSE32_V         -> List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VLSE64_V         -> List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VLOXEI8_V        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VLOXEI16_V       -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VLOXEI32_V       -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VLOXEI64_V       -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VSE8_V           -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSE16_V          -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSE32_V          -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSE64_V          -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VSUXEI8_V        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSUXEI16_V       -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSUXEI32_V       -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSUXEI64_V       -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VSSE8_V          -> List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSSE16_V         -> List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSSE32_V         -> List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSSE64_V         -> List(Y,N,N,N,N,N,Y,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VSOXEI8_V        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSOXEI16_V       -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSOXEI32_V       -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSOXEI64_V       -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VL1RE8_V         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VL1RE16_V        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VL1RE32_V        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VL1RE64_V        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VL2RE8_V         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VL2RE16_V        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VL2RE32_V        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VL2RE64_V        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VL4RE8_V         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VL4RE16_V        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VL4RE32_V        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VL4RE64_V        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VL8RE8_V         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VL8RE16_V        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VL8RE32_V        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VL8RE64_V        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VS1R_V           -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VS2R_V           -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VS4R_V           -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VS8R_V           -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N))

//   def integerWidening(aluFn: ALUFN = ALUFN()): Array[(BitPat, List[BitPat])] = Array(
//                         //           jal                                                                renf1               fence.i
//                         //   val     | jalr                                                             | renf2             |
//                         //   | fp_val| | renx2                                                          | | renf3           |
//                         //   | | rocc| | | renx1             s_alu1                       mem_val       | | | wfd           |
//                         //   | | | br| | | | scie      s_alu2|   imm   dw     alu         | mem_cmd     | | | | mul         |
//                         //   | | | | | | | | | zbk     |     |   |     |      |           | |           | | | | | div       | fence
//                         //   | | | | | | | | | | zkn   |     |   |     |      |           | |           | | | | | | wxd     | | amo
//                         //   | | | | | | | | | | | zks |     |   |     |      |           | |           | | | | | | |       | | | dp
//     VWREDSUMU_VS     -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VWREDSUM_VS      -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VWADDU_VV        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VWADDU_VX        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VWADD_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VWADD_VX         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VWSUBU_VV        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VWSUBU_VX        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VWSUB_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VWSUB_VX         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VWADDU_WV        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VWADDU_WX        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VWADD_WV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VWADD_WX         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VWSUBU_WV        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VWSUBU_WX        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VWSUB_WV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VWSUB_WX         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VWMULU_VV        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VWMULU_VX        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VWMULSU_VV       -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VWMULSU_VX       -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VWMUL_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VWMUL_VX         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VWMACCU_VV       -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VWMACCU_VX       -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VWMACC_VV        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VWMACC_VX        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VWMACCUS_VX      -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VWMACCSU_VV      -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VWMACCSU_VX      -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N))

//   def integer(aluFn: ALUFN = ALUFN()): Array[(BitPat, List[BitPat])] = Array(
//                         //           jal                                                                renf1               fence.i
//                         //   val     | jalr                                                             | renf2             |
//                         //   | fp_val| | renx2                                                          | | renf3           |
//                         //   | | rocc| | | renx1             s_alu1                       mem_val       | | | wfd           |
//                         //   | | | br| | | | scie      s_alu2|   imm   dw     alu         | mem_cmd     | | | | mul         |
//                         //   | | | | | | | | | zbk     |     |   |     |      |           | |           | | | | | div       | fence
//                         //   | | | | | | | | | | zkn   |     |   |     |      |           | |           | | | | | | wxd     | | amo
//                         //   | | | | | | | | | | | zks |     |   |     |      |           | |           | | | | | | |       | | | dp
//     VADD_VV          -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VADD_VX          -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VADD_VI          -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSUB_VV          -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSUB_VX          -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VRSUB_VX         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VRSUB_VI         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VMAX_VV          -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMAX_VX          -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMAXU_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMAXU_VX         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMIN_VV          -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMIN_VX          -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMINU_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMINU_VX         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VAND_VV          -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VAND_VX          -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VAND_VI          -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VOR_VV           -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VOR_VX           -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VOR_VI           -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VXOR_VV          -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VXOR_VX          -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VXOR_VI          -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VRGATHER_VV      -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VRGATHER_VX      -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VRGATHER_VI      -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VREDSUM_VS       -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VREDAND_VS       -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VREDOR_VS        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VREDXOR_VS       -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VREDMINU_VS      -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VREDMIN_VS       -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VREDMAX_VS       -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VREDMAXU_VS      -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VAADDU_VV        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VAADDU_VX        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VAADD_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VAADD_VX         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VASUBU_VV        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VASUBU_VX        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VASUB_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VASUB_VX         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VSLIDEUP_VX      -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSLIDEUP_VI      -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSLIDE1UP_VX     -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VRGATHEREI16_VV  -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSLIDEDOWN_VX    -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSLIDEDOWN_VI    -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSLIDE1DOWN_VX   -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VADC_VVM         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VADC_VXM         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VADC_VIM         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VMADC_VI         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMADC_VIM        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMADC_VVM        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMADC_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMADC_VX         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMADC_VXM        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VSBC_VVM         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSBC_VXM         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VMSBC_VVM        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMSBC_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMSBC_VX         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMSBC_VXM        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VMV_X_S          -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
//     VCPOP_M          -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
//     VFIRST_M         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),

//     VMV_S_X          -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VZEXT_VF2        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VZEXT_VF4        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VZEXT_VF8        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSEXT_VF2        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSEXT_VF4        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSEXT_VF8        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VMSBF_M          -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMSOF_M          -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMSIF_M          -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMSOF_M          -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VIOTA_M          -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VID_V            -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VMERGE_VIM       -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMERGE_VVM       -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMERGE_VXM       -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMV_V_V          -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMV_V_I          -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMV_V_X          -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VMSEQ_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMSEQ_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMSEQ_VX         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMSNE_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMSNE_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMSNE_VX         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMSLTU_VV        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMSLTU_VX        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMSLT_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMSLT_VX         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMSLEU_VV        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMSLEU_VV        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMSLEU_VX        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMSLE_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMSLE_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMSLE_VX         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMSGTU_VI        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMSGTU_VX        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMSGT_VI         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMSGT_VX         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VCOMPRESS_VM     -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMANDN_MM        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMAND_MM         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMOR_MM          -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMXOR_MM         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMORN_MM         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMNAND_MM        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMNOR_MM         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMXNOR_MM        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VSADDU_VV        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSADDU_VX        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSADDU_VI        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSADD_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSADD_VX         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSADD_VI         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSSUBU_VV        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSSUBU_VX        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSSUB_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSSUB_VX         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VSLL_VV          -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSLL_VX          -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSLL_VI          -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VSMUL_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSMUL_VX         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VMV1R_V          -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMV2R_V          -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMV4R_V          -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMV8R_V          -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VDIVU_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VDIVU_VX         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VDIV_VV          -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VDIV_VX          -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VREMU_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VREMU_VX         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VREM_VV          -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VREM_VX          -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMULHU_VV        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMULHU_VX        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMUL_VV          -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMUL_VX          -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMULHSU_VV       -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMULHSU_VX       -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMULH_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMULH_VX         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VSRL_VV          -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSRL_VX          -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSRL_VI          -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSRA_VV          -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSRA_VX          -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSRA_VI          -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSSRL_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSSRL_VX         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSSRL_VI         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSSRA_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSSRA_VX         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VSSRA_VI         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VNSRL_WV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VNSRL_WX         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VNSRL_WI         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VNSRA_WV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VNSRA_WX         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VNSRA_WI         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VNCLIP_WV        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VNCLIP_WX        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VNCLIP_WI        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VNCLIPU_WV       -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VNCLIPU_WX       -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VNCLIPU_WI       -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VMADD_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMADD_VX         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VNMSUB_VV        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VNMSUB_VX        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMACC_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMACC_VX         -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VNMSAC_VV        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VNMSAC_VX        -> List(Y,N,N,N,N,N,N,Y,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N))

//   def float(aluFn: ALUFN = ALUFN()): Array[(BitPat, List[BitPat])] = Array(
//                         //           jal                                                                renf1               fence.i
//                         //   val     | jalr                                                             | renf2             |
//                         //   | fp_val| | renx2                                                          | | renf3           |
//                         //   | | rocc| | | renx1             s_alu1                       mem_val       | | | wfd           |
//                         //   | | | br| | | | scie      s_alu2|   imm   dw     alu         | mem_cmd     | | | | mul         |
//                         //   | | | | | | | | | zbk     |     |   |     |      |           | |           | | | | | div       | fence
//                         //   | | | | | | | | | | zkn   |     |   |     |      |           | |           | | | | | | wxd     | | amo
//                         //   | | | | | | | | | | | zks |     |   |     |      |           | |           | | | | | | |       | | | dp
//     VFADD_VF         -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFADD_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFREDUSUM_VS     -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFSUB_VF         -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFSUB_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFREDOSUM_VS     -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFMIN_VF         -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFMIN_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFREDMIN_VS      -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFMAX_VF         -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFMAX_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFREDMAX_VS      -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFSGNJ_VF        -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFSGNJ_VV        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFSGNJN_VF       -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFSGNJN_VV       -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFSGNJX_VF       -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFSGNJX_VV       -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VFSLIDE1UP_VF    -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFSLIDE1DOWN_VF  -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VFCVT_F_X_V      -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFCVT_F_XU_V     -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFCVT_RTZ_X_F_V  -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFCVT_RTZ_XU_F_V -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFCVT_X_F_V      -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFCVT_XU_F_V     -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFWCVT_X_F_V     -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFWCVT_XU_F_V    -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFWCVT_F_X_V     -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFWCVT_F_XU_V    -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFWCVT_F_F_V     -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFWCVT_RTZ_X_F_V -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFWCVT_RTZ_XU_F_V-> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFNCVT_X_F_W     -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFNCVT_XU_F_W    -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFNCVT_F_X_W     -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFNCVT_F_XU_W    -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFNCVT_F_F_W     -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFNCVT_ROD_F_F_W -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFNCVT_RTZ_X_F_W -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFNCVT_RTZ_XU_F_W-> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VFSQRT_V         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFRSQRT7_V       -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFREC7_V         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFCLASS_V        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VFMERGE_VFM      -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFMV_V_F         -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFMV_F_S         -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N),
//     VFMV_S_F         -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VMFEQ_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMFEQ_VF         -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMFLE_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMFLE_VF         -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMFLT_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMFLT_VF         -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMFNE_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMFNE_VF         -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMFGT_VF         -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VMFGE_VF         -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),


//     VFDIV_VF         -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFDIV_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFRDIV_VF        -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFMUL_VF         -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFMUL_VV         -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFRSUB_VF        -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VFMADD_VV        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFMADD_VF        -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFNMADD_VV       -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFNMADD_VF       -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFMSUB_VV        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFMSUB_VF        -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFNMSUB_VV       -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFNMSUB_VF       -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFMACC_VV        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFMACC_VF        -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFNMACC_VV       -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFNMACC_VF       -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFMSAC_VV        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFMSAC_VF        -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFNMSAC_VV       -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFNMSAC_VF       -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VFWADD_VV        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFWADD_VF        -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFWREDUSUM_VS    -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFWSUB_VV        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFWSUB_VF        -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFWREDOSUM_VS    -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFWADD_WV        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFWADD_WF        -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFWSUB_WV        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFWSUB_WF        -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFWMUL_VV        -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFWMUL_VF        -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),

//     VFWMACC_VV       -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFWMACC_VF       -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFWNMACC_VV      -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFWNMACC_VF      -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFWMSAC_VV       -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFWMSAC_VF       -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFWNMSAC_VV      -> List(Y,N,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
//     VFWNMSAC_VF      -> List(Y,Y,N,N,N,N,N,N,N,N,N,N,A2_X, A1_X, IMM_X,DW_X,aluFn.FN_X,   N,M_X,        Y,N,N,N,N,N,N,CSR.N,N,N,N,N))
// }

// class VFrontendDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
// {
//   val table: Array[(BitPat, List[BitPat])] = (
//     VFrontendDecodeTable.integer(aluFn) ++
//     VFrontendDecodeTable.integerWidening(aluFn) ++
//     VFrontendDecodeTable.loadStore(aluFn) ++
//     VFrontendDecodeTable.float(aluFn))
// }
