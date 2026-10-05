// vme-fused-lm2-gemm-transpose: VME/Xsfmm port of opu-fused-lm2-gemm-transpose.
//
// The original benchmark hand-tiled a signed int8 GEMM with a per-output-column
// bias onto the retired BME encodings (bme.h), using LMUL=2 micro-tiles
// (i8_mm_bme_lm2) to double throughput. "Transpose" refers to A being supplied
// pre-transposed (K-major, at[k*M+i]) -- the natural operand layout for the
// outer-product array, both then and now -- not an explicit transpose step.
//
// This version does the same fused GEMM-with-bias computation, but drives the
// current Xsfmm/VME subset via a bias-fused variant of vme_gemm.h's
// vme_gemm_2x2 (the 2x2-super-tile analogue of the old LM2 micro-tiling: four
// TExTE tiles accumulated in parallel). The four tiles are sf.vtzero.t'd (the
// "zero tile instruction", replacing the old OPMVINBCAST bias-broadcast), and
// the per-column bias is instead added on the way out: each tile row read back
// via sf.vtmv.v.t is added (vadd.vv) to the matching MAX_TE-wide slice of the
// bias vector before being stored -- mirroring how the original's
// i32_lm2_store_ct fused the readout and store into one pass.
//
// C[m][n] = c_bias[n] + sum_k At[k][m] * B[k][n]        (signed int8 x int8 -> int32)

#include "vme_gemm.h"
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#ifndef M_BLOCKS
#define M_BLOCKS 4      // M = M_BLOCKS * 2 * TE
#endif
#ifndef N_BLOCKS
#define N_BLOCKS 4      // N = N_BLOCKS * 2 * TE
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
#define MAX_TE 64
#define MAX_M (M_BLOCKS * 2 * MAX_TE)
#define MAX_N (N_BLOCKS * 2 * MAX_TE)

// Like vme_gemm_2x2 (int8 path only), but adds a per-output-column bias to
// each tile row as it's read out of the accumulator, instead of zero-init
// followed by a separate whole-array bias pass.
static void vme_gemm_2x2_bias(uint32_t* c, const uint8_t* at, const uint8_t* b,
                               const int32_t* bias, size_t M, size_t N, size_t K, size_t te) {
  for (size_t m0 = 0; m0 < M; m0 += 2 * te) {
    for (size_t n0 = 0; n0 < N; n0 += 2 * te) {
      // altfmt=1 (vme_vsettnt_e8w4_alt) is what makes B's sf.mm.s.s read
      // signed -- plain vme_vsettnt_e8w4 leaves B unsigned (vtmms_tvv.h).
      vme_vsettnt_e8w4_alt(te);
      vme_vsettm(te);
      VME_VTZERO(0); VME_VTZERO(1); VME_VTZERO(2); VME_VTZERO(3);
      for (size_t k0 = 0; k0 < K; k0 += 4) {
        size_t kk = (K - k0) < 4 ? (K - k0) : 4;
        const uint8_t* a0 = at + k0 * M + m0;
        const uint8_t* bb = b + k0 * N + n0;
        vme_load_k4(a0, a0 + te, bb, bb + te, M, N, kk);
        vme_vsettk(kk);
        VME_MM_S_S(0, 0, 16); VME_MM_S_S(1, 0, 24);
        VME_MM_S_S(2, 8, 16); VME_MM_S_S(3, 8, 24);
      }
      // Read each tile row out, add the bias slice for its column block, store.
      // v24 (not v20) for the bias load: at e32w1 vlmul is 8, so every plain
      // vector operand here must be 8-register-aligned (v16 and v24 both are).
      vme_vsettnt_e32w1(te);
      for (size_t r = 0; r < te; r++) {
        for (size_t t = 0; t < 4; t++) {
          size_t col0 = n0 + (t & 1) * te;
          uint32_t* dst = c + (m0 + (t >> 1) * te + r) * N + col0;
          VME_VTMV_V_T(16, vme_tss(t, VME_TSS_ROW, r));
          asm volatile("vle32.v v24, (%0)" : : "r"(bias + col0) : "memory");
          asm volatile("vadd.vv v16, v16, v24");
          asm volatile("vse32.v v16, (%0)" : : "r"(dst) : "memory");
        }
      }
    }
  }
  VME_VTDISCARD();
}

static uint8_t at[K_DIM * MAX_M] __attribute__((aligned(64)));   // A^T, k-major: at[k*M + m]
static uint8_t bm[K_DIM * MAX_N] __attribute__((aligned(64)));   // B, k-major:   bm[k*N + n]
static int32_t c_bias[MAX_N] __attribute__((aligned(64)));       // per-output-column bias
static uint32_t c_vme[MAX_M * MAX_N] __attribute__((aligned(64)));
static int32_t c_ref[MAX_M * MAX_N] __attribute__((aligned(64)));
static uint32_t bad_rc[MAX_TE][MAX_TE];   // divergence count by (row,col) position within a te x te tile
static uint32_t bad_t[4];                 // divergence count by which of the 4 2x2 sub-tiles

static uint32_t lcg_state = 1;
static uint32_t lcg(void) { lcg_state = lcg_state * 1664525u + 1013904223u; return lcg_state >> 8; }

static inline size_t vme_rdcycle(void) { size_t c; asm volatile("csrr %0, mcycle" : "=r"(c)); return c; }

int main(void) {
  size_t te = vme_vsettnt_e32w1((size_t)-1);
  if (te > MAX_TE) { printf("TE %lu > MAX_TE\n", te); exit(1); }
  size_t M = M_BLOCKS * 2 * te, N = N_BLOCKS * 2 * te;
  printf("VME fused GEMM+bias (transpose-A): TE=%lu M=%lu N=%lu, sweeping K=%d..%d step %d\n",
         te, M, N, K_SWEEP_MIN, K_DIM, K_SWEEP_STEP);

  for (size_t i = 0; i < (size_t)K_DIM * M; i++) at[i] = (uint8_t)lcg();
  for (size_t i = 0; i < (size_t)K_DIM * N; i++) bm[i] = (uint8_t)lcg();
#ifdef VME_DEBUG_ZERO_BIAS
  for (size_t j = 0; j < N; j++) c_bias[j] = 0;
#else
  for (size_t j = 0; j < N; j++) c_bias[j] = (int32_t)(lcg() & 0xffff) - 0x8000;
#endif

  printf("k,cycles,ndiverge\n");
  int any_diverged = 0;
  for (size_t K = K_SWEEP_MIN; K <= (size_t)K_DIM; K += K_SWEEP_STEP) {
    // Fused GEMM: zero-accumulate the matmul, add the per-column bias at readout.
    // Run once to warm up (icache/tile state), then time a second run.
#ifndef NO_WARMUP
    vme_gemm_2x2_bias(c_vme, at, bm, c_bias, M, N, K, te);
#endif
    size_t cycles1 = vme_rdcycle();
    vme_gemm_2x2_bias(c_vme, at, bm, c_bias, M, N, K, te);
    size_t cycles2 = vme_rdcycle();

    // Reference: scalar signed int8 GEMM with the same bias, C[m][n] = bias[n] + sum_k At[k][m]*B[k][n].
    for (size_t i = 0; i < M; i++) {
      for (size_t j = 0; j < N; j++) {
        int32_t acc = c_bias[j];
        for (size_t k = 0; k < K; k++)
          acc += (int32_t)(int8_t)at[k * M + i] * (int32_t)(int8_t)bm[k * N + j];
        c_ref[i * N + j] = acc;
      }
    }

    size_t ndiverge = 0;
    for (size_t i = 0; i < M; i++) {
      for (size_t j = 0; j < N; j++) {
        if ((int32_t)c_vme[i * N + j] != c_ref[i * N + j]) {
          if (ndiverge < 16)
            printf("DIVERGENCE at K=%lu C[%lu][%lu]: got 0x%08x, expected 0x%08x\n",
                   K, i, j, c_vme[i * N + j], (uint32_t)c_ref[i * N + j]);
          ndiverge++;
          bad_rc[i % te][j % te]++;
          size_t t = ((i % (2 * te)) / te) * 2 + ((j % (2 * te)) / te);
          bad_t[t]++;
        }
      }
    }
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
  return any_diverged ? 1 : 0;
}
