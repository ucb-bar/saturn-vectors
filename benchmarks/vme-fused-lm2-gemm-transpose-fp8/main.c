// vme-fused-lm2-gemm-transpose-fp8: FP8 (E4M3xE4M3) counterpart of
// vme-fused-lm2-gemm-transpose, swept across the same tk=4/1/2 kernel
// variants (tk = rows consumed per sf.mm.e4.e4 instruction, i.e. the
// vme_vsettk() argument hoisted out of the k-loop).
//
// Identical 2x2-super-tile structure to the int8 original (four TExTE tiles
// accumulated in parallel, sf.vtzero'd up front, per-column bias folded in on
// readout instead of a separate bias pass) -- only the operand format and the
// bias-add instruction differ: operands are raw e4m3 bytes (altfmt=0 selects
// e4m3 for B via vme_vsettnt_e8w4, VME_MM_E4_E4's rd-bit0 selects e4m3 for A,
// same as vme_gemm.h/vme-fp8-gemm/vme_mxfp8_matmul.h), the OPU accumulates in
// FP32 instead of int32, and the per-column bias is added with vfadd.vv
// (float) instead of vadd.vv (int) -- the row loads (vme_load_k4, raw byte
// loads) are format-agnostic and reused unchanged from vme_gemm.h.
//
// C[m][n] = c_bias[n] + sum_k A_e4m3[k][m] * B_e4m3[k][n]   (fp8 e4m3 x e4m3 -> fp32)

#include "vme_gemm.h"
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#ifndef M_BLOCKS
#define M_BLOCKS 1      // M = M_BLOCKS * 2 * TE
#endif
#ifndef N_BLOCKS
#define N_BLOCKS 1      // N = N_BLOCKS * 2 * TE
#endif
#ifndef K_DIM
#define K_DIM 32       // max K swept
#endif
#ifndef K_SWEEP_MIN
#define K_SWEEP_MIN 4
#endif
#ifndef K_SWEEP_STEP
#define K_SWEEP_STEP 4
#endif
#ifndef MAX_TE
#define MAX_TE 64
#endif
#define MAX_M (M_BLOCKS * 2 * MAX_TE)
#define MAX_N (N_BLOCKS * 2 * MAX_TE)

// OCP e4m3 -> float, same conversion as vme_gemm_main.h's fp8_to_float with
// e5m2 fixed to 0 (both operands here are always e4m3).
static inline float vme_pow2f(int e) {  // 2^e for -126 <= e <= 127
  union { uint32_t u; float f; } v; v.u = (uint32_t)(e + 127) << 23; return v.f;
}
static float e4m3_to_float(uint8_t x) {
  int sign = x >> 7;
  int e = (x >> 3) & 0xf;
  int m = x & 0x7;
  float v;
  if (e == 15 && m == 7) v = (0.0f / 0.0f);
  else if (e == 0) v = (float)m * vme_pow2f(1 - 7 - 3);
  else v = (float)(m | 0x8) * vme_pow2f(e - 7 - 3);
  return sign ? -v : v;
}

// tk=4: vme_vsettk(4) hoisted out of the k-loop, 4 rows consumed per sf.mm.
static void vme_fp8_gemm_2x2_bias(uint32_t* c, const uint8_t* at, const uint8_t* b,
                                   const float* bias, size_t M, size_t N, size_t K, size_t te) {
  for (size_t m0 = 0; m0 < M; m0 += 2 * te) {
    for (size_t n0 = 0; n0 < N; n0 += 2 * te) {
      vme_vsettnt_e8w4(te);
      vme_vsettm(te);
      VME_VTZERO(0); VME_VTZERO(1); VME_VTZERO(2); VME_VTZERO(3);
      size_t k0 = 0;
      const uint8_t* a0 = at + m0;
      const uint8_t* bb = b + n0;
      vme_vsettk(4);
      while (K - k0 > 4) {
        vme_load_k4(a0, a0 + te, bb, bb + te, M, N, 4);
        VME_MM_E4_E4(0, 0, 16); VME_MM_E4_E4(1, 0, 24);
        VME_MM_E4_E4(2, 8, 16); VME_MM_E4_E4(3, 8, 24);
        a0 += 4 * M;
        bb += 4 * N;
        k0 += 4;
      }
      {
        size_t kk = K - k0;
        vme_load_k4(a0, a0 + te, bb, bb + te, M, N, kk);
        vme_vsettk(kk);
        VME_MM_E4_E4(0, 0, 16); VME_MM_E4_E4(1, 0, 24);
        VME_MM_E4_E4(2, 8, 16); VME_MM_E4_E4(3, 8, 24);
      }
      vme_vsettnt_e32w1(te);
      for (size_t r = 0; r < te; r++) {
        for (size_t t = 0; t < 4; t += 2) {
          size_t col0a = n0 + (t & 1) * te;
          uint32_t* dsta = c + (m0 + (t >> 1) * te + r) * N + col0a;
          size_t t2 = t + 1;
          size_t col0b = n0 + (t2 & 1) * te;
          uint32_t* dstb = c + (m0 + (t2 >> 1) * te + r) * N + col0b;

          VME_VTMV_V_T(16, vme_tss(t, VME_TSS_ROW, r));
          VME_VTMV_V_T(20, vme_tss(t2, VME_TSS_ROW, r));
          asm volatile("vle32.v v24, (%0)" : : "r"(bias + col0a) : "memory");
          asm volatile("vle32.v v28, (%0)" : : "r"(bias + col0b) : "memory");
          asm volatile("vfadd.vv v16, v16, v24");
          asm volatile("vfadd.vv v20, v20, v28");
          asm volatile("vse32.v v16, (%0)" : : "r"(dsta) : "memory");
          asm volatile("vse32.v v20, (%0)" : : "r"(dstb) : "memory");
        }
      }
    }
  }
  VME_VTDISCARD();
}

// tk=1: vme_vsettk(1) hoisted out of the k-loop, 1 row consumed per sf.mm.
static void vme_fp8_gemm_2x2_bias_k1(uint32_t* c, const uint8_t* at, const uint8_t* b,
                                      const float* bias, size_t M, size_t N, size_t K, size_t te) {
  for (size_t m0 = 0; m0 < M; m0 += 2 * te) {
    for (size_t n0 = 0; n0 < N; n0 += 2 * te) {
      vme_vsettnt_e8w4(te);
      vme_vsettm(te);
      VME_VTZERO(0); VME_VTZERO(1); VME_VTZERO(2); VME_VTZERO(3);
      size_t k0 = 0;
      const uint8_t* a0 = at + m0;
      const uint8_t* bb = b + n0;
      vme_vsettk(1);
      while (K - k0 > 1) {
        vme_load_k4(a0, a0 + te, bb, bb + te, M, N, 1);
        VME_MM_E4_E4(0, 0, 16); VME_MM_E4_E4(1, 0, 24);
        VME_MM_E4_E4(2, 8, 16); VME_MM_E4_E4(3, 8, 24);
        a0 += 1 * M;
        bb += 1 * N;
        k0 += 1;
      }
      {
        size_t kk = K - k0;
        vme_load_k4(a0, a0 + te, bb, bb + te, M, N, kk);
        vme_vsettk(kk);
        VME_MM_E4_E4(0, 0, 16); VME_MM_E4_E4(1, 0, 24);
        VME_MM_E4_E4(2, 8, 16); VME_MM_E4_E4(3, 8, 24);
      }
      vme_vsettnt_e32w1(te);
      for (size_t r = 0; r < te; r++) {
        for (size_t t = 0; t < 4; t += 2) {
          size_t col0a = n0 + (t & 1) * te;
          uint32_t* dsta = c + (m0 + (t >> 1) * te + r) * N + col0a;
          size_t t2 = t + 1;
          size_t col0b = n0 + (t2 & 1) * te;
          uint32_t* dstb = c + (m0 + (t2 >> 1) * te + r) * N + col0b;

          VME_VTMV_V_T(16, vme_tss(t, VME_TSS_ROW, r));
          VME_VTMV_V_T(20, vme_tss(t2, VME_TSS_ROW, r));
          asm volatile("vle32.v v24, (%0)" : : "r"(bias + col0a) : "memory");
          asm volatile("vle32.v v28, (%0)" : : "r"(bias + col0b) : "memory");
          asm volatile("vfadd.vv v16, v16, v24");
          asm volatile("vfadd.vv v20, v20, v28");
          asm volatile("vse32.v v16, (%0)" : : "r"(dsta) : "memory");
          asm volatile("vse32.v v20, (%0)" : : "r"(dstb) : "memory");
        }
      }
    }
  }
  VME_VTDISCARD();
}

// tk=2: vme_vsettk(2) hoisted out of the k-loop, 2 rows consumed per sf.mm.
static void vme_fp8_gemm_2x2_bias_k2(uint32_t* c, const uint8_t* at, const uint8_t* b,
                                      const float* bias, size_t M, size_t N, size_t K, size_t te) {
  for (size_t m0 = 0; m0 < M; m0 += 2 * te) {
    for (size_t n0 = 0; n0 < N; n0 += 2 * te) {
      vme_vsettnt_e8w4(te);
      vme_vsettm(te);
      VME_VTZERO(0); VME_VTZERO(1); VME_VTZERO(2); VME_VTZERO(3);
      size_t k0 = 0;
      const uint8_t* a0 = at + m0;
      const uint8_t* bb = b + n0;
      vme_vsettk(2);
      while (K - k0 > 2) {
        vme_load_k4(a0, a0 + te, bb, bb + te, M, N, 2);
        VME_MM_E4_E4(0, 0, 16); VME_MM_E4_E4(1, 0, 24);
        VME_MM_E4_E4(2, 8, 16); VME_MM_E4_E4(3, 8, 24);
        a0 += 2 * M;
        bb += 2 * N;
        k0 += 2;
      }
      {
        size_t kk = K - k0;
        vme_load_k4(a0, a0 + te, bb, bb + te, M, N, kk);
        vme_vsettk(kk);
        VME_MM_E4_E4(0, 0, 16); VME_MM_E4_E4(1, 0, 24);
        VME_MM_E4_E4(2, 8, 16); VME_MM_E4_E4(3, 8, 24);
      }
      vme_vsettnt_e32w1(te);
      for (size_t r = 0; r < te; r++) {
        for (size_t t = 0; t < 4; t += 2) {
          size_t col0a = n0 + (t & 1) * te;
          uint32_t* dsta = c + (m0 + (t >> 1) * te + r) * N + col0a;
          size_t t2 = t + 1;
          size_t col0b = n0 + (t2 & 1) * te;
          uint32_t* dstb = c + (m0 + (t2 >> 1) * te + r) * N + col0b;

          VME_VTMV_V_T(16, vme_tss(t, VME_TSS_ROW, r));
          VME_VTMV_V_T(20, vme_tss(t2, VME_TSS_ROW, r));
          asm volatile("vle32.v v24, (%0)" : : "r"(bias + col0a) : "memory");
          asm volatile("vle32.v v28, (%0)" : : "r"(bias + col0b) : "memory");
          asm volatile("vfadd.vv v16, v16, v24");
          asm volatile("vfadd.vv v20, v20, v28");
          asm volatile("vse32.v v16, (%0)" : : "r"(dsta) : "memory");
          asm volatile("vse32.v v20, (%0)" : : "r"(dstb) : "memory");
        }
      }
    }
  }
  VME_VTDISCARD();
}

static uint8_t at[K_DIM * MAX_M] __attribute__((aligned(64)));   // A^T, k-major: at[k*M + m], raw e4m3 bytes
static uint8_t bm[K_DIM * MAX_N] __attribute__((aligned(64)));   // B, k-major:   bm[k*N + n], raw e4m3 bytes
static float c_bias[MAX_N] __attribute__((aligned(64)));         // per-output-column bias
static uint32_t c_vme[MAX_M * MAX_N] __attribute__((aligned(64))); // raw fp32 bits out of the OPU
static uint32_t bad_rc[MAX_TE][MAX_TE];   // divergence count by (row,col) position within a te x te tile
static uint32_t bad_t[4];                 // divergence count by which of the 4 2x2 sub-tiles

static uint32_t lcg_state = 1;
static uint32_t lcg(void) { lcg_state = lcg_state * 1664525u + 1013904223u; return lcg_state >> 8; }

static inline size_t vme_rdcycle(void) { size_t c; asm volatile("csrr %0, mcycle" : "=r"(c)); return c; }

typedef void (*gemm_fn_t)(uint32_t*, const uint8_t*, const uint8_t*, const float*, size_t, size_t, size_t, size_t);

// Runs one kernel variant through the full K sweep against a double-precision
// scalar reference, printing a k,cycles,ndiverge table and a final
// PASSED/FAILED for that variant. Returns 1 if any K diverged.
//
// Reference accumulation is done in double (same rationale as
// vme_gemm_main.h's fp8 path): with K up to a few dozen e4m3 terms plus a
// bias, a naive single-precision running sum picks up its own order-dependent
// rounding noise unrelated to whether the hardware's answer is correct. The
// divergence tolerance is relative to the sum of |term| (bias included), not
// |acc| alone, so that ordinary cancellation-amplified rounding isn't
// mistaken for a bug when terms of opposite sign nearly cancel.
static int run_sweep(const char* name, gemm_fn_t fn, const uint8_t* at, const uint8_t* bm,
                      const float* c_bias, size_t M, size_t N, size_t te) {
  printf("-- %s --\n", name);
  printf("k,cycles,ndiverge\n");
  int any_diverged = 0;
  for (size_t K = K_SWEEP_MIN; K <= (size_t)K_DIM; K += K_SWEEP_STEP) {
    // Fused GEMM: zero-accumulate the matmul, add the per-column bias at readout.
    // Run once to warm up (icache/tile state), then time a second run.
#ifndef NO_WARMUP
    fn(c_vme, at, bm, c_bias, M, N, K, te);
#endif
    size_t cycles1 = vme_rdcycle();
    fn(c_vme, at, bm, c_bias, M, N, K, te);
    size_t cycles2 = vme_rdcycle();

    size_t ndiverge = 0;
#ifndef VERIFY
#define VERIFY 1
#endif
#if VERIFY
    for (size_t i = 0; i < M; i++) {
      for (size_t j = 0; j < N; j++) {
        double acc = (double)c_bias[j];
        double mag_sum = acc < 0 ? -acc : acc;
        for (size_t k = 0; k < K; k++) {
          double term = (double)e4m3_to_float(at[k * M + i]) * (double)e4m3_to_float(bm[k * N + j]);
          acc += term;
          mag_sum += term < 0 ? -term : term;
        }
        float got; memcpy(&got, &c_vme[i * N + j], 4);
        double err = (double)got - acc; if (err < 0) err = -err;
        double mag = acc < 0 ? -acc : acc;
        if (err > 1e-5 * (mag > mag_sum ? mag : mag_sum) + 1e-30) {
          if (ndiverge < 16)
            printf("DIVERGENCE at K=%lu C[%lu][%lu]: got %g, expected %g\n", K, i, j, (double)got, acc);
          ndiverge++;
          bad_rc[i % te][j % te]++;
          size_t t = ((i % (2 * te)) / te) * 2 + ((j % (2 * te)) / te);
          bad_t[t]++;
        }
      }
    }
#endif
    if (ndiverge) any_diverged = 1;

    printf("%lu,%lu,%lu\n", K, cycles2 - cycles1, ndiverge);
  }
  if (any_diverged) {
    printf("bad_t (sub-tile 0..3): %u %u %u %u\n", bad_t[0], bad_t[1], bad_t[2], bad_t[3]);
    printf("bad_rc (row,col within a %lux%lu tile):\n", te, te);
    for (size_t r = 0; r < te; r++) {
      for (size_t c = 0; c < te; c++)
        if (bad_rc[r][c]) printf("  (r=%lu,c=%lu): %u\n", r, c, bad_rc[r][c]);
    }
  }
  printf(any_diverged ? "FAILED\n" : "PASSED\n");
  return any_diverged;
}

int main(void) {
  size_t te = vme_vsettnt_e32w1((size_t)-1);
  if (te > MAX_TE) { printf("TE %lu > MAX_TE\n", te); exit(1); }
  size_t M = M_BLOCKS * 2 * te, N = N_BLOCKS * 2 * te;
  printf("VME fused FP8 GEMM+bias (E4M3xE4M3, transpose-A): TE=%lu M=%lu N=%lu, sweeping K=%d..%d step %d\n",
         te, M, N, K_SWEEP_MIN, K_DIM, K_SWEEP_STEP);

  // keep e4m3 values finite: clear the top exponent bit (0xbf mask), same as
  // vme_gemm_main.h -- avoids ever generating the e=15,m=7 NaN encoding.
  for (size_t i = 0; i < (size_t)K_DIM * M; i++) at[i] = (uint8_t)lcg() & 0xbf;
  for (size_t i = 0; i < (size_t)K_DIM * N; i++) bm[i] = (uint8_t)lcg() & 0xbf;
  for (size_t j = 0; j < N; j++) c_bias[j] = (float)((int32_t)(lcg() & 0xffff) - 0x8000);

  int any_diverged = 0;
  struct { const char* name; gemm_fn_t fn; } variants[] = {
    { "vme_fp8_gemm_2x2_bias (tk=4)",    vme_fp8_gemm_2x2_bias },
    { "vme_fp8_gemm_2x2_bias_k1 (tk=1)", vme_fp8_gemm_2x2_bias_k1 },
    { "vme_fp8_gemm_2x2_bias_k2 (tk=2)", vme_fp8_gemm_2x2_bias_k2 },
  };
  for (size_t v = 0; v < sizeof(variants) / sizeof(variants[0]); v++) {
    for (size_t r = 0; r < te; r++)
      for (size_t c = 0; c < te; c++)
        bad_rc[r][c] = 0;
    bad_t[0] = bad_t[1] = bad_t[2] = bad_t[3] = 0;
    if (run_sweep(variants[v].name, variants[v].fn, at, bm, c_bias, M, N, te))
      any_diverged = 1;
  }
  return any_diverged ? 1 : 0;
}
