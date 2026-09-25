// vme_gemm.h: GEMM on the Xsfmm/VME subset with a 2x2 grid of tiles.
//
//   C[M][N] = sum_k A[k][m] * B[k][n]      at[k*M + m], b[k*N + n] (both "k-major")
//
// Each 2TE x 2TE block of C is accumulated in mt0..mt12 (tile index 0..3):
//   A rows (k0..k0+3) of m-block 0 -> v0, v2, v4, v6   (vs2 = v0)
//   A rows of m-block 1           -> v1, v3, v5, v7   (vs2 = v1)
//   B rows of n-block 0           -> v8, v10, v12, v14 (vs1 = v8)
//   B rows of n-block 1           -> v9, v11, v13, v15 (vs1 = v9)
// so every A and B row feeds two sf.mm instructions.

#ifndef VME_GEMM_H
#define VME_GEMM_H

#include "vme.h"

#define VME_LOAD_ROW(vreg, ptr) asm volatile("vle8.v " vreg ", (%0)" : : "r"(ptr) : "memory")

static inline void vme_load_k4(const uint8_t* a0, const uint8_t* a1, const uint8_t* b0, const uint8_t* b1,
                               size_t lda, size_t ldb, size_t kk) {
  VME_LOAD_ROW("v0", a0); VME_LOAD_ROW("v1", a1); VME_LOAD_ROW("v8", b0); VME_LOAD_ROW("v9", b1);
  if (kk > 1) {
    VME_LOAD_ROW("v2", a0 + lda); VME_LOAD_ROW("v3", a1 + lda);
    VME_LOAD_ROW("v10", b0 + ldb); VME_LOAD_ROW("v11", b1 + ldb);
  }
  if (kk > 2) {
    VME_LOAD_ROW("v4", a0 + 2*lda); VME_LOAD_ROW("v5", a1 + 2*lda);
    VME_LOAD_ROW("v12", b0 + 2*ldb); VME_LOAD_ROW("v13", b1 + 2*ldb);
  }
  if (kk > 3) {
    VME_LOAD_ROW("v6", a0 + 3*lda); VME_LOAD_ROW("v7", a1 + 3*lda);
    VME_LOAD_ROW("v14", b0 + 3*ldb); VME_LOAD_ROW("v15", b1 + 3*ldb);
  }
}

// c points to int32_t (int8 GEMM) or float (FP8 GEMM); M, N multiples of 2*te.
static inline void vme_gemm_2x2(void* c, const uint8_t* at, const uint8_t* b,
                                size_t M, size_t N, size_t K, size_t te, int fp8) {
  uint32_t* cw = (uint32_t*)c;
  for (size_t m0 = 0; m0 < M; m0 += 2*te) {
    for (size_t n0 = 0; n0 < N; n0 += 2*te) {
      vme_vsettnt_e8w4(te);
      vme_vsettm(te);
      VME_VTZERO(0); VME_VTZERO(1); VME_VTZERO(2); VME_VTZERO(3);
      for (size_t k0 = 0; k0 < K; k0 += 4) {
        size_t kk = (K - k0) < 4 ? (K - k0) : 4;
        const uint8_t* a0 = at + k0*M + m0;
        const uint8_t* bb = b + k0*N + n0;
        vme_load_k4(a0, a0 + te, bb, bb + te, M, N, kk);
        vme_vsettk(kk);
        if (fp8) {
          VME_MM_E4_E4(0, 0, 8); VME_MM_E4_E4(1, 0, 9);
          VME_MM_E4_E4(2, 1, 8); VME_MM_E4_E4(3, 1, 9);
        } else {
          VME_MM_S_S(0, 0, 8); VME_MM_S_S(1, 0, 9);
          VME_MM_S_S(2, 1, 8); VME_MM_S_S(3, 1, 9);
        }
      }
      // write back 32-bit results row by row
      vme_vsettnt_e32w1(te);
      for (size_t r = 0; r < te; r++) {
        for (size_t t = 0; t < 4; t++) {
          uint32_t* dst = cw + (m0 + (t >> 1) * te + r) * N + n0 + (t & 1) * te;
          VME_VTMV_V_T(16, vme_tss(t, VME_TSS_ROW, r));
          asm volatile("vse32.v v16, (%0)" : : "r"(dst) : "memory");
        }
      }
    }
  }
  VME_VTDISCARD();
}

#endif // VME_GEMM_H
