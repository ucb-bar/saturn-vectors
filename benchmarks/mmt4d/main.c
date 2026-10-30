// Phase 1 task 1 (and the ongoing test driver for every later task): first
// validates mmt4d_ref.h itself by packing plain row-major matrices into
// mmt4d panel layout and comparing against a naive matmul on the unpacked
// matrices. Later tasks add more kernel variants (rvv, zvt f32/s8, zvt 2x2)
// to the KERNELS[] table below, reusing this same sweep.
//
// Testing requirements (CLAUDE.md): random inputs with a fixed seed (printed
// on failure), K1 in {0,1,2,3,TK-1,TK,TK+1,2*TK+3,64} per kernel's own TK,
// both ACCUMULATE settings, s8 extremes -128/127, s32 exact match, f32
// K-scaled relative tolerance, PASS/FAIL per case, nonzero exit on failure.
#include <math.h>
#include <stdint.h>
#include <stdio.h>

#include "mmt4d_params.h"
#include "mmt4d_ref.h"

void mmt4d_rvv_f32(void* out_tile, const void* lhs_panel,
                    const void* rhs_panel, const mmt4d_params_t* params);
void mmt4d_zvt_s8(void* out_tile, const void* lhs_panel,
                   const void* rhs_panel, const mmt4d_params_t* params);

#define SEED 0x5eed1234u

static uint32_t g_rng;
static void rng_seed(uint32_t s) { g_rng = s ? s : 1; }
// xorshift32
static uint32_t rng_next(void) {
  g_rng ^= g_rng << 13;
  g_rng ^= g_rng >> 17;
  g_rng ^= g_rng << 5;
  return g_rng;
}
static float rand_f32(void) {
  int32_t v = (int32_t)(rng_next() & 0xff) - 128; // [-128,127]
  return (float)v * (1.0f / 32.0f);               // [-4, ~4]
}
static int8_t rand_s8(void) { return (int8_t)(rng_next() & 0xff); }
// occasionally force an extreme, per CLAUDE.md's s8 testing requirement.
static int8_t rand_s8_with_extremes(void) {
  uint32_t r = rng_next();
  if ((r & 0xff) == 0) return -128;
  if ((r & 0xff) == 1) return 127;
  return (int8_t)(r >> 8);
}

typedef enum { TYPE_F32, TYPE_S8 } elem_type_t;

typedef struct {
  const char* name;
  mmt4d_tile_func_t fn;
  elem_type_t type;
  int32_t M0, N0, K0;
  int64_t tk; // this kernel's max K-rows consumed per inner step (its "TK")
} kernel_case_t;

#define MAX_M0 32
#define MAX_N0 32
#define MAX_K1 64
#define MAX_K0 4
#define MAX_K (MAX_K1 * MAX_K0)

static float f32_lhs_plain[MAX_M0][MAX_K];
static float f32_rhs_plain[MAX_N0][MAX_K];
// Flat, densely-packed panel buffers: mmt4d_ref (and every real kernel)
// indexes these with stride M0*K0/N0*K0 computed from the *actual* per-case
// M0/N0/K0, not MAX_M0/MAX_K0 -- a fixed-dimension [MAX_K1][MAX_M0][MAX_K0]
// array would silently pad every row to MAX_K0/MAX_M0, misaligning the
// layout the kernel actually reads whenever the real size differs (it always
// does for M0, and for K0 in the f32 case).
static float f32_lhs_panel[MAX_K1 * MAX_M0 * MAX_K0];
static float f32_rhs_panel[MAX_K1 * MAX_N0 * MAX_K0];
static float f32_out[MAX_M0 * MAX_N0];
static float f32_out_ref[MAX_M0 * MAX_N0];

static int8_t s8_lhs_plain[MAX_M0][MAX_K];
static int8_t s8_rhs_plain[MAX_N0][MAX_K];
static int8_t s8_lhs_panel[MAX_K1 * MAX_M0 * MAX_K0];
static int8_t s8_rhs_panel[MAX_K1 * MAX_N0 * MAX_K0];
static int32_t s8_out[MAX_M0 * MAX_N0];
static int32_t s8_out_ref[MAX_M0 * MAX_N0];

static int run_one_f32(const kernel_case_t* kc, int64_t K1, int accumulate) {
  int32_t M0 = kc->M0, N0 = kc->N0, K0 = kc->K0;
  int64_t K = K1 * K0;

  for (int32_t m = 0; m < M0; m++)
    for (int64_t k = 0; k < K; k++) f32_lhs_plain[m][k] = rand_f32();
  for (int32_t n = 0; n < N0; n++)
    for (int64_t k = 0; k < K; k++) f32_rhs_plain[n][k] = rand_f32();
  for (int32_t i = 0; i < M0 * N0; i++) {
    f32_out[i] = f32_out_ref[i] = accumulate ? rand_f32() : 0.0f;
  }

  for (int64_t k1 = 0; k1 < K1; k1++)
    for (int32_t m = 0; m < M0; m++)
      for (int32_t k0 = 0; k0 < K0; k0++)
        f32_lhs_panel[k1 * M0 * K0 + m * K0 + k0] = f32_lhs_plain[m][k1 * K0 + k0];
  for (int64_t k1 = 0; k1 < K1; k1++)
    for (int32_t n = 0; n < N0; n++)
      for (int32_t k0 = 0; k0 < K0; k0++)
        f32_rhs_panel[k1 * N0 * K0 + n * K0 + k0] = f32_rhs_plain[n][k1 * K0 + k0];

  // Naive reference, directly on the unpacked matrices: out[m][n] +=
  // sum_k lhs_plain[m][k] * rhs_plain[n][k] (mmt4d's rhs is logically [N][K]).
  for (int32_t m = 0; m < M0; m++) {
    for (int32_t n = 0; n < N0; n++) {
      float acc = 0.0f;
      for (int64_t k = 0; k < K; k++)
        acc += f32_lhs_plain[m][k] * f32_rhs_plain[n][k];
      f32_out_ref[m * N0 + n] += acc;
    }
  }

  mmt4d_params_t params = {
      .K = K1, .M0 = M0, .N0 = N0, .K0 = K0,
      .flags = accumulate ? MMT4D_FLAG_ACCUMULATE : 0u};
  kc->fn(f32_out, f32_lhs_panel, f32_rhs_panel, &params);

  // K-scaled relative tolerance: summation order can differ.
  float tol = (float)(K > 0 ? K : 1) * 1e-5f;
  for (int32_t i = 0; i < M0 * N0; i++) {
    float got = f32_out[i], want = f32_out_ref[i];
    float scale = fabsf(want) > 1.0f ? fabsf(want) : 1.0f;
    if (fabsf(got - want) > tol * scale) {
      printf("FAIL %s f32 K1=%lld accumulate=%d at i=%d: got=%d want=%d (x1000)\n",
             kc->name, (long long)K1, accumulate, i, (int)(got * 1000),
             (int)(want * 1000));
      return 0;
    }
  }
  return 1;
}

static int run_one_s8(const kernel_case_t* kc, int64_t K1, int accumulate) {
  int32_t M0 = kc->M0, N0 = kc->N0, K0 = kc->K0;
  int64_t K = K1 * K0;

  for (int32_t m = 0; m < M0; m++)
    for (int64_t k = 0; k < K; k++) s8_lhs_plain[m][k] = rand_s8_with_extremes();
  for (int32_t n = 0; n < N0; n++)
    for (int64_t k = 0; k < K; k++) s8_rhs_plain[n][k] = rand_s8_with_extremes();
  for (int32_t i = 0; i < M0 * N0; i++) {
    int32_t seed = accumulate ? (int32_t)(rng_next() & 0xffff) - 0x8000 : 0;
    s8_out[i] = s8_out_ref[i] = seed;
  }

  for (int64_t k1 = 0; k1 < K1; k1++)
    for (int32_t m = 0; m < M0; m++)
      for (int32_t k0 = 0; k0 < K0; k0++)
        s8_lhs_panel[k1 * M0 * K0 + m * K0 + k0] = s8_lhs_plain[m][k1 * K0 + k0];
  for (int64_t k1 = 0; k1 < K1; k1++)
    for (int32_t n = 0; n < N0; n++)
      for (int32_t k0 = 0; k0 < K0; k0++)
        s8_rhs_panel[k1 * N0 * K0 + n * K0 + k0] = s8_rhs_plain[n][k1 * K0 + k0];

  for (int32_t m = 0; m < M0; m++) {
    for (int32_t n = 0; n < N0; n++) {
      int32_t acc = 0;
      for (int64_t k = 0; k < K; k++)
        acc += (int32_t)s8_lhs_plain[m][k] * (int32_t)s8_rhs_plain[n][k];
      s8_out_ref[m * N0 + n] += acc;
    }
  }

  mmt4d_params_t params = {
      .K = K1, .M0 = M0, .N0 = N0, .K0 = K0,
      .flags = accumulate ? MMT4D_FLAG_ACCUMULATE : 0u};
  kc->fn(s8_out, s8_lhs_panel, s8_rhs_panel, &params);

  for (int32_t i = 0; i < M0 * N0; i++) {
    if (s8_out[i] != s8_out_ref[i]) {
      printf("FAIL %s s8 K1=%lld accumulate=%d at i=%d: got=%ld want=%ld\n",
             kc->name, (long long)K1, accumulate, i, (long)s8_out[i],
             (long)s8_out_ref[i]);
      return 0;
    }
  }
  return 1;
}

// K1 in {0,1,2,3,TK-1,TK,TK+1,2*TK+3,64}, de-duplicated and clamped to
// MAX_K1.
//
// MMT4D_FAST_SWEEP (set via -DMMT4D_FAST_SWEEP, see the test_mmt4d_fast
// Makefile target) drops the expensive high-K1 points (2*TK+3, 64) for quick
// RTL-sim iteration -- it does not shrink M0/N0/K0, which are fixed per
// kernel tile shape, so this is still exercising the real kernel, just with
// a shorter reduction depth. Use plain test_mmt4d for the full sweep this
// file's CLAUDE.md coverage requirements call for.
static int build_k1_list(int64_t tk, int64_t* out) {
#ifdef MMT4D_FAST_SWEEP
  int64_t raw[4] = {0, 1, 2, tk};
  const int raw_n = 4;
#else
  int64_t raw[9] = {0, 1, 2, 3, tk - 1, tk, tk + 1, 2 * tk + 3, 64};
  const int raw_n = 9;
#endif
  int n = 0;
  for (int i = 0; i < raw_n; i++) {
    int64_t v = raw[i];
    if (v < 0 || v > MAX_K1) continue;
    int dup = 0;
    for (int j = 0; j < n; j++) if (out[j] == v) dup = 1;
    if (!dup) out[n++] = v;
  }
  return n;
}

static int run_kernel(const kernel_case_t* kc) {
  int64_t k1_list[9];
  int n = build_k1_list(kc->tk, k1_list);
  int all_ok = 1;
  for (int i = 0; i < n; i++) {
    for (int accumulate = 0; accumulate <= 1; accumulate++) {
      rng_seed(SEED + (uint32_t)(i * 2 + accumulate));
      int ok = (kc->type == TYPE_F32) ? run_one_f32(kc, k1_list[i], accumulate)
                                       : run_one_s8(kc, k1_list[i], accumulate);
      if (!ok) {
        printf("  (seed base=0x%x)\n", SEED);
        all_ok = 0;
      }
    }
  }
  printf("%s %s\n", all_ok ? "PASS" : "FAIL", kc->name);
  return all_ok;
}

// zvt_f32/zvt_f32_2x2 (mmt4d_zvt_f32.c/mmt4d_zvt_f32_2x2.c) are intentionally
// not exercised here: f32-input matmul on the VME outer-product unit is out
// of scope (only fp8- and int8-input matmul, with f32 output, are).
static const kernel_case_t KERNELS[] = {
    {"ref_f32", mmt4d_ref_tile_f32, TYPE_F32, 16, 16, 1, 1},
    {"ref_s8", mmt4d_ref_tile_s8, TYPE_S8, 16, 16, 4, 4},
    {"rvv_f32", mmt4d_rvv_f32, TYPE_F32, 16, 16, 1, 1},
    {"zvt_s8", mmt4d_zvt_s8, TYPE_S8, 16, 16, 4, 4},
};

int main(void) {
  int all_ok = 1;
  for (size_t i = 0; i < sizeof(KERNELS) / sizeof(KERNELS[0]); i++) {
    if (!run_kernel(&KERNELS[i])) all_ok = 0;
  }
  printf(all_ok ? "PASS test_mmt4d\n" : "FAIL test_mmt4d\n");
  return all_ok ? 0 : 1;
}
