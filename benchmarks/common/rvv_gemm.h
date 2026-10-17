// rvv_gemm.h: plain-RVV int8 x int8 -> int32 GEMM with per-column bias, matching
// vme-fused-lm2-gemm-transpose's semantics exactly:
//   C[m][n] = bias[n] + sum_k At[k][m] * B[k][n]       (both operands signed int8,
//                                                        A pre-transposed, K-major)
// No VME/Xsfmm instructions -- standard RVV only. Written as a speed-of-light
// baseline to measure the OPU's speedup over generic vector code on the same problem.
//
// Vectorizes across N (one output row per m-iteration, N columns at a time) using
// LMUL=8 at SEW=32 for the accumulator, and the matching LMUL=2 at SEW=8 for the raw
// int8 B-row load -- same LMUL/SEW ratio (8/32 == 2/8), so both vsetvli calls below
// request the same element count N and there's no tail handling needed as long as
// N <= VLEN/4 (true for every shape this benchmark suite uses: N = 2*TE = VLEN/4,
// which is also exactly e32,m8's VLMAX at that VLEN).
//
// A is never vectorized: it's one scalar per (m,k), broadcast across the row via
// vmacc.vx, which is the natural shape for an outer-product-structured GEMM even
// without tile-register hardware.

#ifndef RVV_GEMM_H
#define RVV_GEMM_H

#include <stdint.h>
#include <stddef.h>

static inline void rvv_gemm_bias(int32_t* c, const uint8_t* at, const uint8_t* b,
                                  const int32_t* bias, size_t M, size_t N, size_t K) {
  for (size_t m = 0; m < M; m++) {
    int32_t* crow = c + m * N;
    asm volatile("vsetvli zero, %0, e32, m8, ta, ma" : : "r"(N));
    asm volatile("vle32.v v8, (%0)" : : "r"(bias) : "memory");
    for (size_t k = 0; k < K; k++) {
      int32_t a_val = (int32_t)(int8_t)at[k * M + m];
      const uint8_t* brow = b + k * N;
      asm volatile("vsetvli zero, %0, e8, m2, ta, ma" : : "r"(N));
      asm volatile("vle8.v v16, (%0)" : : "r"(brow) : "memory");
      asm volatile("vsetvli zero, %0, e32, m8, ta, ma" : : "r"(N));
      asm volatile("vsext.vf4 v24, v16");
      asm volatile("vmacc.vx v8, %0, v24" : : "r"(a_val));
    }
    asm volatile("vse32.v v8, (%0)" : : "r"(crow) : "memory");
  }
}

#endif // RVV_GEMM_H
