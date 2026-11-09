// vec-fused-lm2-gemm-transpose: plain-RVV baseline for vme-fused-lm2-gemm-transpose,
// used to measure the VME/Xsfmm matrix unit's speedup over generic vector code on the
// identical problem (C[m][n] = bias[n] + sum_k At[k][m]*B[k][n], int8 x int8 -> int32,
// A pre-transposed, K-major). See common/rvv_gemm.h for the kernel itself -- no
// VME/Xsfmm instructions anywhere in this file.
//
// Mirrors vme-fused-lm2-gemm-transpose/main.c's M/N sizing (M=N=2*TE, TE=VLEN/8),
// K-sweep parameters, and random-data generation (same LCG seed and fill order) so a
// run of each against identical M/N/K produces byte-identical inputs -- see the
// spike cross-check instructions in the project notes for why that's enough to prove
// the two kernels compute identical outputs without a side-by-side diff.

#include "rvv_gemm.h"
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#ifndef M_BLOCKS
#define M_BLOCKS 1      // M = M_BLOCKS * 2 * TE
#endif
#ifndef N_BLOCKS
#define N_BLOCKS 1      // N = N_BLOCKS * 2 * TE
#endif
#ifndef K_DIM
#define K_DIM 1024
#endif
#ifndef K_SWEEP_MIN
#define K_SWEEP_MIN 32
#endif
#ifndef K_SWEEP_STEP
#define K_SWEEP_STEP 32
#endif
#ifndef MAX_TE
#define MAX_TE 128
#endif
#define MAX_M (M_BLOCKS * 2 * MAX_TE)
#define MAX_N (N_BLOCKS * 2 * MAX_TE)
#ifndef VERIFY
#define VERIFY 1
#endif

static uint8_t at[K_DIM * MAX_M] __attribute__((aligned(64)));   // A^T, k-major: at[k*M + m]
static uint8_t bm[K_DIM * MAX_N] __attribute__((aligned(64)));   // B, k-major:   bm[k*N + n]
static int32_t c_bias[MAX_N] __attribute__((aligned(64)));       // per-output-column bias
static int32_t c_vme[MAX_M * MAX_N] __attribute__((aligned(64)));
#if VERIFY
static int32_t c_ref[MAX_M * MAX_N] __attribute__((aligned(64)));
#endif

static uint32_t lcg_state = 1;
static uint32_t lcg(void) { lcg_state = lcg_state * 1664525u + 1013904223u; return lcg_state >> 8; }

static inline size_t rvv_rdcycle(void) { size_t c; asm volatile("csrr %0, mcycle" : "=r"(c)); return c; }
static inline size_t rvv_read_vlenb(void) { size_t v; asm volatile("csrr %0, vlenb" : "=r"(v)); return v; }

// Runs the RVV kernel through the full K sweep against the scalar reference,
// printing a k,cycles,ndiverge table and a final PASSED/FAILED -- same format as
// vme-fused-lm2-gemm-transpose's run_sweep so the two logs parse identically.
static int run_sweep(size_t M, size_t N) {
  printf("-- rvv_gemm_bias --\n");
  printf("k,cycles,ndiverge\n");
  int any_diverged = 0;
  for (size_t K = K_SWEEP_MIN; K <= (size_t)K_DIM; K += K_SWEEP_STEP) {
#ifndef NO_WARMUP
    rvv_gemm_bias(c_vme, at, bm, c_bias, M, N, K);
#endif
    size_t cycles1 = rvv_rdcycle();
    rvv_gemm_bias(c_vme, at, bm, c_bias, M, N, K);
    size_t cycles2 = rvv_rdcycle();

    size_t ndiverge = 0;
#if VERIFY
    for (size_t i = 0; i < M; i++) {
      for (size_t j = 0; j < N; j++) {
        int32_t acc = c_bias[j];
        for (size_t k = 0; k < K; k++)
          acc += (int32_t)(int8_t)at[k * M + i] * (int32_t)(int8_t)bm[k * N + j];
        c_ref[i * N + j] = acc;
      }
    }
    for (size_t i = 0; i < M; i++) {
      for (size_t j = 0; j < N; j++) {
        if (c_vme[i * N + j] != c_ref[i * N + j]) {
          if (ndiverge < 16)
            printf("DIVERGENCE at K=%lu C[%lu][%lu]: got 0x%08x, expected 0x%08x\n",
                   K, i, j, (uint32_t)c_vme[i * N + j], (uint32_t)c_ref[i * N + j]);
          ndiverge++;
        }
      }
    }
#endif
    if (ndiverge) any_diverged = 1;
    printf("%lu,%lu,%lu\n", K, cycles2 - cycles1, ndiverge);
  }
  printf(any_diverged ? "FAILED\n" : "PASSED\n");
  return any_diverged;
}

int main(void) {
  size_t vlen = rvv_read_vlenb() * 8;
  size_t te = vlen / 8; // matches opuTE(); only used here for M/N sizing and reporting
  if (te > MAX_TE) { printf("TE %lu > MAX_TE\n", te); exit(1); }
  size_t M = M_BLOCKS * 2 * te, N = N_BLOCKS * 2 * te;
  printf("RVV fused GEMM+bias (transpose-A): VLEN=%lu TE=%lu M=%lu N=%lu, sweeping K=%d..%d step %d\n",
         vlen, te, M, N, K_SWEEP_MIN, K_DIM, K_SWEEP_STEP);

  // same seed, same fill order as vme-fused-lm2-gemm-transpose's default (non-debug) path
  for (size_t i = 0; i < (size_t)K_DIM * M; i++) at[i] = (uint8_t)lcg();
  for (size_t i = 0; i < (size_t)K_DIM * N; i++) bm[i] = (uint8_t)lcg();
  for (size_t j = 0; j < N; j++) c_bias[j] = (int32_t)(lcg() & 0xffff) - 0x8000;

  int any_diverged = run_sweep(M, N);
  return any_diverged ? 1 : 0;
}
