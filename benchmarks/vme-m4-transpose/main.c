// vme-m4-transpose: VME/Xsfmm port of opu-m4-transpose.
//
// The original used all four BME tile registers (m0..m3) as a fast 4x-wide
// register-shuffle: load a 2*TE x 2*TE block of C_in into the tiles row-wise
// (VMV_RV), then read it back out row-wise too (VMV_VR) but store it to the
// *transposed* memory position (c_out[j*M+i] instead of c_out[i*N+j]) --
// the BME tile array had no separate row/column readout mode, so the actual
// transpose was purely an address-swap in software.
//
// The current Xsfmm/VME tiles instead transpose for free: sf.vtmv.t.v/v.t
// both take a row-or-column pattern in the TSS. Writing a block in with the
// ROW pattern and reading it back with the COLUMN pattern transposes each
// TE x TE sub-tile in hardware (this is also what vme-directed's "row-in /
// column-out" check exercises for a single tile). This benchmark does the
// same thing across all 4 quadrants of a 2*TE x 2*TE super-block at once,
// the way vme_gemm_2x2 uses 4 tiles for a GEMM super-block.
//
// For sub-tile t (row-quadrant rq = t>>1, col-quadrant cq = t&1) covering
// input rows [m0+rq*te, +te) x cols [n0+cq*te, +te):
//   load:  row r of the input sub-block -> sf.vtmv.t.v row r of tile t
//   store: column c of tile t (already the transpose of the sub-block)
//          -> row (n0+cq*te+c) of c_out, starting at column (m0+rq*te)
// Quadrants 1 and 2 (off-diagonal) end up swapped in the output, which falls
// out naturally from row axis -> output column, column axis -> output row.
//
// vme_transpose_2x2_colrow below is the dual: load each sub-tile COLUMN-wise
// (sf.vtmv.t.v with the COLUMN pattern) and read it back ROW-wise, the mirror
// image of vme-directed's "column-in / row-out" single-tile check. Same
// transpose, opposite hardware datapath -- the input column and output row
// aren't contiguous in memory, so that direction uses strided load/store
// instead of the unit-stride ones above.

#include "vme.h"
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#ifndef M_BLOCKS
#define M_BLOCKS 4      // M = M_BLOCKS * 2 * TE
#endif
#ifndef N_BLOCKS
#define N_BLOCKS 4      // N = N_BLOCKS * 2 * TE
#endif
#define MAX_TE 64
#define MAX_M (M_BLOCKS * 2 * MAX_TE)
#define MAX_N (N_BLOCKS * 2 * MAX_TE)

static inline size_t vme_rdcycle(void) { size_t c; asm volatile("csrr %0, mcycle" : "=r"(c)); return c; }

static void vme_transpose_2x2(int32_t* c_out, const int32_t* c_in, size_t M, size_t N, size_t te) {
  vme_vsettnt_e32w1(te);
  for (size_t m0 = 0; m0 < M; m0 += 2 * te) {
    for (size_t n0 = 0; n0 < N; n0 += 2 * te) {
      for (size_t t = 0; t < 4; t++) {
        size_t rq = t >> 1, cq = t & 1;
        for (size_t r = 0; r < te; r++) {
          const int32_t* src = c_in + (m0 + rq * te + r) * N + (n0 + cq * te);
          asm volatile("vle32.v v16, (%0)" : : "r"(src) : "memory");
          VME_VTMV_T_V(vme_tss(t, VME_TSS_ROW, r), 16);
        }
      }
      for (size_t t = 0; t < 4; t++) {
        size_t rq = t >> 1, cq = t & 1;
        for (size_t c = 0; c < te; c++) {
          int32_t* dst = c_out + (n0 + cq * te + c) * M + (m0 + rq * te);
          VME_VTMV_V_T(16, vme_tss(t, VME_TSS_COL, c));
          asm volatile("vse32.v v16, (%0)" : : "r"(dst) : "memory");
        }
      }
    }
  }
  VME_VTDISCARD();
}

// Dual of vme_transpose_2x2: load column-wise (TSS_COL), store row-wise
// (TSS_ROW). Tile content ends up identical to the row-in/column-out
// version, so the transpose comes out the same; only the load/store
// datapath differs. Input columns and output rows aren't contiguous in
// c_in/c_out (both row-major), so this uses strided vle/vse instead of
// the unit-stride ones in vme_transpose_2x2.
static void vme_transpose_2x2_colrow(int32_t* c_out, const int32_t* c_in, size_t M, size_t N, size_t te) {
  vme_vsettnt_e32w1(te);
  size_t in_stride = N * sizeof(int32_t);
  size_t out_stride = M * sizeof(int32_t);
  for (size_t m0 = 0; m0 < M; m0 += 2 * te) {
    for (size_t n0 = 0; n0 < N; n0 += 2 * te) {
      for (size_t t = 0; t < 4; t++) {
        size_t rq = t >> 1, cq = t & 1;
        for (size_t c = 0; c < te; c++) {
          const int32_t* src = c_in + (m0 + rq * te) * N + (n0 + cq * te + c);
          asm volatile("vlse32.v v16, (%0), %1" : : "r"(src), "r"(in_stride) : "memory");
          VME_VTMV_T_V(vme_tss(t, VME_TSS_COL, c), 16);
        }
      }
      for (size_t t = 0; t < 4; t++) {
        size_t rq = t >> 1, cq = t & 1;
        for (size_t r = 0; r < te; r++) {
          int32_t* dst = c_out + (n0 + cq * te) * M + (m0 + rq * te + r);
          VME_VTMV_V_T(16, vme_tss(t, VME_TSS_ROW, r));
          asm volatile("vsse32.v v16, (%0), %1" : : "r"(dst), "r"(out_stride) : "memory");
        }
      }
    }
  }
  VME_VTDISCARD();
}

static int32_t c_in[MAX_M * MAX_N] __attribute__((aligned(64)));
static int32_t c_out[MAX_N * MAX_M] __attribute__((aligned(64)));  // note: transposed shape N x M

static uint32_t lcg_state = 1;
static uint32_t lcg(void) { lcg_state = lcg_state * 1664525u + 1013904223u; return lcg_state >> 8; }

static void check_transpose(const char* label, size_t M, size_t N) {
  for (size_t i = 0; i < M; i++) {
    for (size_t j = 0; j < N; j++) {
      if (c_out[j * M + i] != c_in[i * N + j]) {
        printf("DIVERGENCE (%s) at (%lu,%lu): c_out=0x%x, c_in=0x%x\n",
               label, i, j, c_out[j * M + i], c_in[i * N + j]);
        exit(1);
      }
    }
  }
  printf("PASSED (%s)\n", label);
}

int main(void) {
  size_t te = vme_vsettnt_e32w1((size_t)-1);
  if (te > MAX_TE) { printf("TE %lu > MAX_TE\n", te); exit(1); }
  size_t M = M_BLOCKS * 2 * te, N = N_BLOCKS * 2 * te;
  printf("VME M4 transpose: TE=%lu M=%lu N=%lu\n", te, M, N);

  for (size_t i = 0; i < M * N; i++) c_in[i] = (int32_t)lcg();

  // Run once to warm up, then time a second run.
  vme_transpose_2x2(c_out, c_in, M, N, te);
  size_t cycles1 = vme_rdcycle();
  vme_transpose_2x2(c_out, c_in, M, N, te);
  size_t cycles2 = vme_rdcycle();
  printf("cycles: %lu\n", cycles2 - cycles1);
  check_transpose("row-in/column-out", M, N);

  vme_transpose_2x2_colrow(c_out, c_in, M, N, te);
  cycles1 = vme_rdcycle();
  vme_transpose_2x2_colrow(c_out, c_in, M, N, te);
  cycles2 = vme_rdcycle();
  printf("cycles (column-in/row-out): %lu\n", cycles2 - cycles1);
  check_transpose("column-in/row-out", M, N);

  return 0;
}
