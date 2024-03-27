// Modified version of:
// vector_defines.h
// https://github.com/RALC88/riscv-vectorized-benchmark-suite/blob/rvv-1.0/common/vector_defines.h
// Find details on the original version below
// Author: Matteo Perotti <mperotti@iis.ee.ethz.ch>

// RISC-V VECTOR intrinsics mapping by Cristóbal Ramírez Lazo, "Barcelona 2019"

#include "riscv_vector.h"

/*
  Data-Type Intrinsics
*/

#define _MMR_f64 vfloat64m1_t
#define _MMR_f32 vfloat32m1_t

#define _MMR_i64 vint64m1_t
#define _MMR_i32 vint32m1_t

#define _MMR_u64 vuint64m1_t
#define _MMR_u32 vuint32m1_t

#define _MMR_MASK_i64 vbool64_t
#define _MMR_MASK_i32 vbool32_t

/*
  Reinterpret Intrinsics
*/

#define _MM_CAST_i32_u32(op1) __riscv_vreinterpret_v_u32m1_i32m1(op1)
#define _MM_CAST_u32_i32(op1) __riscv_vreinterpret_v_i32m1_u32m1(op1)
#define _MM_CAST_i64_u64(op1) __riscv_vreinterpret_v_u64m1_i64m1(op1)
#define _MM_CAST_u64_i64(op1) __riscv_vreinterpret_v_i64m1_u64m1(op1)

#define _MM_CAST_i32_f32(op1) __riscv_vreinterpret_v_f32m1_i32m1(op1)
#define _MM_CAST_u32_f32(op1) __riscv_vreinterpret_v_f32m1_u32m1(op1)
#define _MM_CAST_f32_i32(op1) __riscv_vreinterpret_v_i32m1_f32m1(op1)
#define _MM_CAST_f32_u32(op1) __riscv_vreinterpret_v_u32m1_f32m1(op1)

#define _MM_CAST_i64_f64(op1) __riscv_vreinterpret_v_f64m1_i64m1(op1)
#define _MM_CAST_u64_f64(op1) __riscv_vreinterpret_v_f64m1_u64m1(op1)
#define _MM_CAST_f64_i64(op1) __riscv_vreinterpret_v_i64m1_f64m1(op1)
#define _MM_CAST_f64_u64(op1) __riscv_vreinterpret_v_u64m1_f64m1(op1)

/*
  Integer Intrinsics
*/

#define _MM_SET_i64(op1, vl) __riscv_vmv_v_x_i64m1(op1, vl)
#define _MM_SET_i32(op1, vl) __riscv_vmv_v_x_i32m1(op1, vl)

#define _MM_MERGE_i64(op1, op2, op3, vl) __riscv_vmerge_vvm_i64m1(op1, op2, op3, vl)
#define _MM_MERGE_i32(op1, op2, op3, vl) __riscv_vmerge_vvm_i32m1(op1, op2, op3, vl)

#define _MM_AND_i64(op1, op2, vl) __riscv_vand_vv_i64m1(op1, op2, vl)
#define _MM_AND_i32(op1, op2, vl) __riscv_vand_vv_i32m1(op1, op2, vl)

#define _MM_OR_i64(op1, op2, vl) __riscv_vor_vv_i64m1(op1, op2, vl)
#define _MM_OR_i32(op1, op2, vl) __riscv_vor_vv_i32m1(op1, op2, vl)

#define _MM_XOR_i64(op1, op2, vl) __riscv_vxor_vv_i64m1(op1, op2, vl)
#define _MM_XOR_i32(op1, op2, vl) __riscv_vxor_vv_i32m1(op1, op2, vl)

#define _MM_SLL_i64(op1, op2, vl) __riscv_vsll_vv_i64m1(op1, op2, vl)
#define _MM_SLL_i32(op1, op2, vl) __riscv_vsll_vv_i32m1(op1, op2, vl)

#define _MM_SRL_i64(op1, op2, vl) __riscv_vsrl_vv_u64m1(op1, op2, vl)
#define _MM_SRL_i32(op1, op2, vl) __riscv_vsrl_vv_u32m1(op1, op2, vl)

#define _MM_ADD_i64(op1, op2, vl) __riscv_vadd_vv_i64m1(op1, op2, vl)
#define _MM_ADD_i32(op1, op2, vl) __riscv_vadd_vv_i32m1(op1, op2, vl)

#define _MM_SUB_i64(op1, op2, vl) __riscv_vsub_vv_i64m1(op1, op2, vl)
#define _MM_SUB_i32(op1, op2, vl) __riscv_vsub_vv_i32m1(op1, op2, vl)

#define _MM_MUL_i64(op1, op2, vl) __riscv_vmul_vv_i64m1(op1, op2, vl)
#define _MM_MUL_i32(op1, op2, vl) __riscv_vmul_vv_i32m1(op1, op2, vl)

#define _MM_VMSEQ_i64(op1, op2, vl) __riscv_vmseq_vv_i64m1_b64(op1, op2, vl)
#define _MM_VMSEQ_i32(op1, op2, vl) __riscv_vmseq_vv_i32m1_b32(op1, op2, vl)

/*
  Floating-Point Intrinsics
*/

#define _MM_SET_f64(op1, vl) __riscv_vfmv_v_f_f64m1(op1, vl)
#define _MM_SET_f32(op1, vl) __riscv_vfmv_v_f_f32m1(op1, vl)

#define _MM_MERGE_f64(op1, op2, op3, vl) __riscv_vmerge_vvm_f64m1(op1, op2, op3, vl)
#define _MM_MERGE_f32(op1, op2, op3, vl) __riscv_vmerge_vvm_f32m1(op1, op2, op3, vl)

#define _MM_MAX_f64(op1, op2, vl) __riscv_vfmax_vv_f64m1(op1, op2, vl)
#define _MM_MAX_f32(op1, op2, vl) __riscv_vfmax_vv_f32m1(op1, op2, vl)

#define _MM_ADD_f64(op1, op2, vl) __riscv_vfadd_vv_f64m1(op1, op2, vl)
#define _MM_ADD_f32(op1, op2, vl) __riscv_vfadd_vv_f32m1(op1, op2, vl)

#define _MM_SUB_f64(op1, op2, vl) __riscv_vfsub_vv_f64m1(op1, op2, vl)
#define _MM_SUB_f32(op1, op2, vl) __riscv_vfsub_vv_f32m1(op1, op2, vl)

#define _MM_MUL_f64(op1, op2, vl) __riscv_vfmul_vv_f64m1(op1, op2, vl)
#define _MM_MUL_f32(op1, op2, vl) __riscv_vfmul_vv_f32m1(op1, op2, vl)

#define _MM_MACC_f64(op1, op2, op3, vl) __riscv_vfmacc_vv_f64m1(op1, op2, op3, vl)
#define _MM_MACC_f32(op1, op2, op3, vl) __riscv_vfmacc_vv_f32m1(op1, op2, op3, vl)

#define _MM_MADD_f64(op1, op2, op3, vl) __riscv_vfmadd_vv_f64m1(op1, op2, op3, vl)
#define _MM_MADD_f32(op1, op2, op3, vl) __riscv_vfmadd_vv_f32m1(op1, op2, op3, vl)

#define _MM_VFCVT_F_X_f64(op1, vl) __riscv_vfcvt_f_x_v_f64m1(op1, vl)
#define _MM_VFCVT_F_X_f32(op1, vl) __riscv_vfcvt_f_x_v_f32m1(op1, vl)
#define _MM_VFCVT_X_F_i64(op1, vl) __riscv_vfcvt_x_f_v_i64m1(op1, vl)
#define _MM_VFCVT_X_F_i32(op1, vl) __riscv_vfcvt_x_f_v_i32m1(op1, vl)
#define _MM_VFWCVT_F_F_f64(op1, vl) __riscv_vfwcvt_f_f_v_f64m2(op1, vl)
#define _MM_VFNCVT_F_F_f32(op1, vl) __riscv_vfncvt_f_f_w_f32m1(op1, vl)
#define _MM_VFWCVT_F_X_f64(op1, vl) __riscv_vfwcvt_f_x_v_f64m2(op1, vl)
#define _MM_VFCVT_f32_i32(op1, vl) __riscv_vfcvt_f_x_v_f32m1(op1, vl)

#define _MM_VFLE_f64(op1, op2, vl) __riscv_vmfle_vv_f64m1_b64(op1, op2, vl)
#define _MM_VFLE_f32(op1, op2, vl) __riscv_vmfle_vv_f32m1_b32(op1, op2, vl)

#define _MM_VFLT_f64(op1, op2, vl) __riscv_vmflt_vv_f64m1_b64(op1, op2, vl)
#define _MM_VFLT_f32(op1, op2, vl) __riscv_vmflt_vv_f32m1_b32(op1, op2, vl)

/*
  Ancillary Defines
*/

#define FENCE() asm volatile("fence");
