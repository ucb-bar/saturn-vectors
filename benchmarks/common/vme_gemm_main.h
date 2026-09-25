// Shared driver for the vme-*-gemm benchmarks. Define VME_GEMM_FP8 for E4M3 x E4M3 -> FP32.

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
static inline size_t vme_rdcycle(void) { size_t c; asm volatile("csrr %0, mcycle" : "=r"(c)); return c; }
#include "vme_gemm.h"

#ifndef M_BLOCKS
#define M_BLOCKS 1      // M = M_BLOCKS * 2 * TE
#endif
#ifndef N_BLOCKS
#define N_BLOCKS 1      // N = N_BLOCKS * 2 * TE
#endif
#ifndef K_DIM
#define K_DIM 64
#endif
#ifndef VERIFY
#define VERIFY 1
#endif
#define MAX_TE 64
#define MAX_M (M_BLOCKS * 2 * MAX_TE)
#define MAX_N (N_BLOCKS * 2 * MAX_TE)

static uint8_t at[K_DIM * MAX_M] __attribute__((aligned(64)));
static uint8_t bm[K_DIM * MAX_N] __attribute__((aligned(64)));
static uint32_t c_vme[MAX_M * MAX_N] __attribute__((aligned(64)));

static uint32_t lcg_state = 1;
static uint32_t lcg(void) { lcg_state = lcg_state * 1664525u + 1013904223u; return lcg_state >> 8; }

#ifdef VME_GEMM_FP8
static inline float vme_pow2f(int e) {  // 2^e for -126 <= e <= 127
  union { uint32_t u; float f; } v; v.u = (uint32_t)(e + 127) << 23; return v.f;
}
static float e4m3_to_float(uint8_t x) {
  int e = (x >> 3) & 15, m = x & 7;
  float v = e ? (float)(m | 8) * vme_pow2f(e - 10) : (float)m * vme_pow2f(-9);
  return (x & 0x80) ? -v : v;
}
#endif

int main(void) {
  size_t te = vme_vsettnt_e32w1((size_t)-1);
  if (te > MAX_TE) { printf("TE %lu > MAX_TE\n", te); exit(1); }
  size_t M = M_BLOCKS * 2 * te, N = N_BLOCKS * 2 * te, K = K_DIM;
  for (size_t i = 0; i < K * M; i++) at[i] = (uint8_t)lcg();
  for (size_t i = 0; i < K * N; i++) bm[i] = (uint8_t)lcg();
#ifdef VME_GEMM_FP8
  // keep E4M3 values finite (clear the top exponent bit, so never 0x7f/0xff NaN)
  for (size_t i = 0; i < K * M; i++) at[i] &= 0xbf;
  for (size_t i = 0; i < K * N; i++) bm[i] &= 0xbf;
  const int fp8 = 1;
  const char* name = "E4M3xE4M3->FP32";
#else
  const int fp8 = 0;
  const char* name = "INT8xINT8->INT32";
#endif
  printf("VME GEMM %s: TE=%lu M=%lu N=%lu K=%lu\n", name, te, M, N, K);

  // warm up caches, then time
  vme_gemm_2x2(c_vme, at, bm, M, N, K, te, fp8);
  size_t t0 = vme_rdcycle();
  vme_gemm_2x2(c_vme, at, bm, M, N, K, te, fp8);
  size_t t1 = vme_rdcycle();
  size_t cycles = t1 - t0;
  size_t macs = M * N * K;
  printf("cycles=%lu MACs=%lu MACs/cycle=%lu.%02lu\n", cycles, macs, macs / cycles, (100 * macs / cycles) % 100);

#if VERIFY
  for (size_t i = 0; i < M; i++) {
    for (size_t j = 0; j < N; j++) {
#ifdef VME_GEMM_FP8
      volatile float acc = 0.0f;
      for (size_t k = 0; k < K; k++) acc = acc + e4m3_to_float(at[k*M + i]) * e4m3_to_float(bm[k*N + j]);
      float got; memcpy(&got, &c_vme[i*N + j], 4);
      float err = got - acc; if (err < 0) err = -err;
      float mag = acc < 0 ? -acc : acc;
      if (err > 1e-5f * mag + 1e-30f) {
#else
      int32_t acc = 0;
      for (size_t k = 0; k < K; k++) acc += (int32_t)(int8_t)at[k*M + i] * (int32_t)(int8_t)bm[k*N + j];
      if ((int32_t)c_vme[i*N + j] != acc) {
#endif
        printf("FAILED at C[%lu][%lu]: got 0x%08x\n", i, j, c_vme[i*N + j]);
        exit(1);
      }
    }
  }
  printf("PASSED\n");
#endif
  return 0;
}
