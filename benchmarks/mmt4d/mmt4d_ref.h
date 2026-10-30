// mmt4d_ref.h -- scalar reference tile functions.
//
// Ground truth for test_mmt4d.c: a straightforward scalar implementation of
// the mmt4d tile contraction (see mmt4d_params.h for the panel layout and
// semantics). These are the trusted implementations that every optimized
// kernel (RVV, Zvt) is checked against -- never "fix" a failing optimized
// kernel by changing these (CLAUDE.md "Don't").

#ifndef MMT4D_REF_H
#define MMT4D_REF_H

#include <stdint.h>

#include "mmt4d_params.h"

static inline void mmt4d_ref_tile_f32(void* out_tile, const void* lhs_panel,
                                       const void* rhs_panel,
                                       const mmt4d_params_t* params) {
  const float* lhs = (const float*)lhs_panel;
  const float* rhs = (const float*)rhs_panel;
  float* out = (float*)out_tile;
  const int32_t M0 = params->M0, N0 = params->N0, K0 = params->K0;

  if (!(params->flags & MMT4D_FLAG_ACCUMULATE)) {
    for (int32_t i = 0; i < M0 * N0; i++) out[i] = 0.0f;
  }
  for (int64_t k1 = 0; k1 < params->K; k1++) {
    const float* lhs_k1 = lhs + k1 * M0 * K0;
    const float* rhs_k1 = rhs + k1 * N0 * K0;
    for (int32_t m0 = 0; m0 < M0; m0++) {
      for (int32_t n0 = 0; n0 < N0; n0++) {
        float acc = 0.0f;
        for (int32_t k0 = 0; k0 < K0; k0++) {
          acc += lhs_k1[m0 * K0 + k0] * rhs_k1[n0 * K0 + k0];
        }
        out[m0 * N0 + n0] += acc;
      }
    }
  }
}

// s8 x s8 -> s32, sign-extending before multiplying.
static inline void mmt4d_ref_tile_s8(void* out_tile, const void* lhs_panel,
                                      const void* rhs_panel,
                                      const mmt4d_params_t* params) {
  const int8_t* lhs = (const int8_t*)lhs_panel;
  const int8_t* rhs = (const int8_t*)rhs_panel;
  int32_t* out = (int32_t*)out_tile;
  const int32_t M0 = params->M0, N0 = params->N0, K0 = params->K0;

  if (!(params->flags & MMT4D_FLAG_ACCUMULATE)) {
    for (int32_t i = 0; i < M0 * N0; i++) out[i] = 0;
  }
  for (int64_t k1 = 0; k1 < params->K; k1++) {
    const int8_t* lhs_k1 = lhs + k1 * M0 * K0;
    const int8_t* rhs_k1 = rhs + k1 * N0 * K0;
    for (int32_t m0 = 0; m0 < M0; m0++) {
      for (int32_t n0 = 0; n0 < N0; n0++) {
        int32_t acc = 0;
        for (int32_t k0 = 0; k0 < K0; k0++) {
          acc += (int32_t)lhs_k1[m0 * K0 + k0] * (int32_t)rhs_k1[n0 * K0 + k0];
        }
        out[m0 * N0 + n0] += acc;
      }
    }
  }
}

#endif // MMT4D_REF_H
