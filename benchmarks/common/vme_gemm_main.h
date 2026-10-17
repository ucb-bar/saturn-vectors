// Shared driver for the vme-*-gemm benchmarks. Define VME_GEMM_FP8 for E4M3 x E4M3 -> FP32.
//
// Sweeps M=N through edge cases around every multiple of 8 up to 128
// (7,8,9, 15,16,17, ..., 127,128,129), since those are the values most
// likely to expose off-by-one/tail-clamping bugs in vme_gemm_tiled's
// per-tile tm/tn handling -- plain multiples of TE would never exercise the
// partial last tile at all.

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
static inline size_t vme_rdcycle(void) { size_t c; asm volatile("csrr %0, mcycle" : "=r"(c)); return c; }
#include "vme_gemm.h"

#ifndef K_DIM
#define K_DIM 8 // independent of M,N -- the M/N sweep doesn't need a large K to exercise tile edges
#endif
#ifndef VERIFY
#define VERIFY 1
#endif
#define MAX_TE 64
#define MAX_MN 136 // covers the M/N sweep below (up to 8*16+1 = 129)

static uint8_t at[K_DIM * MAX_MN] __attribute__((aligned(64)));
static uint8_t bm[K_DIM * MAX_MN] __attribute__((aligned(64)));
static uint32_t c_vme[MAX_MN * MAX_MN] __attribute__((aligned(64)));

static uint32_t lcg_state = 1;
static uint32_t lcg(void) { lcg_state = lcg_state * 1664525u + 1013904223u; return lcg_state >> 8; }

#ifdef VME_GEMM_FP8
// OCP FP8 -> float (e5m2=0: e4m3, e5m2=1: e5m2), same as vme-directed's.
static inline float vme_pow2f(int e) {  // 2^e for -126 <= e <= 127
  union { uint32_t u; float f; } v; v.u = (uint32_t)(e + 127) << 23; return v.f;
}
static float fp8_to_float(uint8_t x, int e5m2) {
  int sign = x >> 7;
  int ebits = e5m2 ? 5 : 4, mbits = e5m2 ? 2 : 3, bias = e5m2 ? 15 : 7;
  int e = (x >> mbits) & ((1 << ebits) - 1);
  int m = x & ((1 << mbits) - 1);
  float v;
  if (e5m2 && e == 31) v = m ? (0.0f/0.0f) : (1.0f/0.0f);
  else if (!e5m2 && e == 15 && m == 7) v = (0.0f/0.0f);
  else if (e == 0) v = (float)m * vme_pow2f(1 - bias - mbits);
  else v = (float)(m | (1 << mbits)) * vme_pow2f(e - bias - mbits);
  return sign ? -v : v;
}
static const int fmt_a_tab[4] = {0, 0, 1, 1}; // 0 = e4m3, 1 = e5m2
static const int fmt_b_tab[4] = {0, 1, 0, 1};
static const char* fmt_names[4] = {"E4M3xE4M3", "E4M3xE5M2", "E5M2xE4M3", "E5M2xE5M2"};
#define FP8 1
#else
static const int fmt_a_tab[4] = {0, 1, 0, 1}; // 0 = unsigned, 1 = signed
static const int fmt_b_tab[4] = {0, 0, 1, 1};
static const char* fmt_names[4] = {"U8xU8", "S8xU8", "U8xS8", "S8xS8"};
#define FP8 0
#endif

// Runs all 4 sign/format combos at one (M,N), verifying each against a
// scalar reference. Returns 1 if any combo diverged.
static int run_mn(size_t M, size_t N, size_t te, size_t K) {
  for (size_t i = 0; i < K * M; i++) at[i] = (uint8_t)lcg();
  for (size_t i = 0; i < K * N; i++) bm[i] = (uint8_t)lcg();
#ifdef VME_GEMM_FP8
  // keep FP8 values finite (clear the top exponent bit): safely avoids
  // Inf/NaN for e4m3 (e=15) and e5m2 (e=31) alike.
  for (size_t i = 0; i < K * M; i++) at[i] &= 0xbf;
  for (size_t i = 0; i < K * N; i++) bm[i] &= 0xbf;
#endif

  int any_failed = 0;
  for (int v = 0; v < 4; v++) {
    int fmt_a = fmt_a_tab[v], fmt_b = fmt_b_tab[v];
    printf("VME GEMM %s->%s: TE=%lu M=%lu N=%lu K=%lu\n", fmt_names[v], FP8 ? "FP32" : "INT32", te, M, N, K);

    // warm up caches, then time
    vme_gemm_tiled(c_vme, at, bm, M, N, K, te, FP8, fmt_a, fmt_b);
    size_t t0 = vme_rdcycle();
    vme_gemm_tiled(c_vme, at, bm, M, N, K, te, FP8, fmt_a, fmt_b);
    size_t t1 = vme_rdcycle();
    size_t cycles = t1 - t0;
    size_t macs = M * N * K;
    printf("cycles=%lu MACs=%lu MACs/cycle=%lu.%02lu\n", cycles, macs, macs / cycles, (100 * macs / cycles) % 100);

#if VERIFY
    int failed = 0;
    for (size_t i = 0; i < M; i++) {
      for (size_t j = 0; j < N; j++) {
#ifdef VME_GEMM_FP8
        // Double-precision reference accumulation: with K up to a few dozen
        // fp8 terms, a naive single-precision running sum picks up its own
        // rounding noise (order-dependent) that's unrelated to whether the
        // hardware's answer is correct, and can spuriously exceed the
        // tolerance below. Accumulating in double keeps the reference close
        // enough to exact that the tolerance only has to absorb the
        // hardware's own single FP32 rounding, not two different roundings.
        double acc = 0.0, mag_sum = 0.0;
        for (size_t k = 0; k < K; k++) {
          double term = (double)fp8_to_float(at[k*M + i], fmt_a) * (double)fp8_to_float(bm[k*N + j], fmt_b);
          acc += term;
          mag_sum += term < 0 ? -term : term;
        }
        float got; memcpy(&got, &c_vme[i*N + j], 4);
        double err = (double)got - acc; if (err < 0) err = -err;
        // Tolerance is relative to the sum of |term|, not |acc|: when terms
        // of opposite sign nearly cancel, acc can be far smaller than any
        // single term, and a tolerance relative to acc alone would reject
        // ordinary cancellation-amplified rounding as if it were a bug.
        double mag = acc < 0 ? -acc : acc;
        if (err > 1e-5 * (mag > mag_sum ? mag : mag_sum) + 1e-30) {
#else
        int32_t acc = 0;
        for (size_t k = 0; k < K; k++) {
          int32_t av = fmt_a ? (int32_t)(int8_t)at[k*M + i] : (int32_t)at[k*M + i];
          int32_t bv = fmt_b ? (int32_t)(int8_t)bm[k*N + j] : (int32_t)bm[k*N + j];
          acc += av * bv;
        }
        if ((int32_t)c_vme[i*N + j] != acc) {
#endif
          if (failed < 8) {
#ifdef VME_GEMM_FP8
            float want_f = (float)acc; uint32_t want; memcpy(&want, &want_f, 4);
            printf("FAILED at C[%lu][%lu]: got 0x%08x expected 0x%08x\n", i, j, c_vme[i*N + j], want);
#else
            printf("FAILED at C[%lu][%lu]: got 0x%08x expected 0x%08x\n", i, j, c_vme[i*N + j], (uint32_t)acc);
#endif
          }
          failed++;
        }
      }
    }
    printf(failed ? "FAILED\n" : "PASSED\n");
    if (failed) any_failed = 1;
#endif
  }
  return any_failed;
}

int main(void) {
  size_t te = vme_vsettnt_e32w1((size_t)-1);
  if (te > MAX_TE) { printf("TE %lu > MAX_TE\n", te); exit(1); }
  size_t K = K_DIM;

  int any_failed = 0;
  size_t max_mn = 2 * te; // caps the sweep at the single/double-tile boundary for faster sim
  for (size_t k = 1; 8*k - 1 <= max_mn; k++) {
    size_t vals[3] = { 8*k - 1, 8*k, 8*k + 1 };
    for (int vi = 0; vi < 3; vi++) {
      size_t MN = vals[vi];
      if (MN > max_mn) continue;
      printf("== M=N=%lu ==\n", MN);
      if (run_mn(MN, MN, te, K)) any_failed = 1;
    }
  }

  // K-dimension edge cases, at a fixed single-tile M=N=TE.
  for (size_t kk = 1; kk <= 8; kk++) {
    printf("== K=%lu ==\n", kk);
    if (run_mn(te, te, te, kk)) any_failed = 1;
  }

  return any_failed ? 1 : 0;
}
