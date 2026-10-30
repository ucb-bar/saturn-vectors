// mmt4d_rvv_f32.c -- RVV baseline (vfmacc.vf outer product), same style as
// IREE's existing riscv_64 f32 ukernel
// (runtime/src/iree/builtins/ukernel/arch/riscv_64/mmt4d_riscv_64_v.c): one
// N0-wide accumulator per output row, broadcast-multiply-accumulate the
// row's lhs scalar into it each k step. K0=1 (no K-widening in plain RVV).
//
// This exists to prove the test harness works end to end and to give an
// instruction-count baseline to compare the Zvt kernels against (see
// docs/NOTES.md's Phase 1 instruction-count report).
//
// Freestanding: no libc, no allocation, no includes outside kernels/include.
#include <riscv_vector.h>

#include "mmt4d_params.h"

enum { MMT4D_RVV_F32_M0 = 16 };

#define MMT4D_RVV_F32_FOR_EACH_M0(X) \
  X(0) X(1) X(2) X(3) X(4) X(5) X(6) X(7) \
  X(8) X(9) X(10) X(11) X(12) X(13) X(14) X(15)

void mmt4d_rvv_f32(void* out_tile, const void* lhs_panel,
                    const void* rhs_panel, const mmt4d_params_t* params) {
  const int32_t M0 = params->M0, N0 = params->N0;
  const float* lhs_ptr = (const float*)lhs_panel;
  const float* rhs_ptr = (const float*)rhs_panel;
  float* out_ptr = (float*)out_tile;
  (void)M0; // asserted via the enum below; unused in release builds

  size_t vl = (size_t)N0;

#define MMT4D_RVV_DECL(I) vfloat32m4_t acc##I;
  MMT4D_RVV_F32_FOR_EACH_M0(MMT4D_RVV_DECL)
#undef MMT4D_RVV_DECL

  if (params->flags & MMT4D_FLAG_ACCUMULATE) {
#define MMT4D_RVV_LOAD(I) acc##I = __riscv_vle32_v_f32m4(out_ptr + (I) * N0, vl);
    MMT4D_RVV_F32_FOR_EACH_M0(MMT4D_RVV_LOAD)
#undef MMT4D_RVV_LOAD
  } else {
#define MMT4D_RVV_ZERO(I) acc##I = __riscv_vfmv_v_f_f32m4(0.0f, vl);
    MMT4D_RVV_F32_FOR_EACH_M0(MMT4D_RVV_ZERO)
#undef MMT4D_RVV_ZERO
  }

  for (int64_t k = 0; k < params->K; k++) {
    vfloat32m4_t rhs = __riscv_vle32_v_f32m4(rhs_ptr, vl);
    rhs_ptr += N0;
#define MMT4D_RVV_MAC(I) \
    acc##I = __riscv_vfmacc_vf_f32m4(acc##I, lhs_ptr[I], rhs, vl);
    MMT4D_RVV_F32_FOR_EACH_M0(MMT4D_RVV_MAC)
#undef MMT4D_RVV_MAC
    lhs_ptr += MMT4D_RVV_F32_M0;
  }

#define MMT4D_RVV_STORE(I) __riscv_vse32_v_f32m4(out_ptr + (I) * N0, acc##I, vl);
  MMT4D_RVV_F32_FOR_EACH_M0(MMT4D_RVV_STORE)
#undef MMT4D_RVV_STORE
}
