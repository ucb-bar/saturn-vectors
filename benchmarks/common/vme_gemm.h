// vme_gemm.h: GEMM on the Xsfmm/VME subset with a 2x2 grid of tiles.
//
//   C[M][N] = sum_k A[k][m] * B[k][n]      at[k*M + m], b[k*N + n] (both "k-major")
//
// sf.mm's vs2/vs1 are hardware-fixed 8-register-aligned windows: spike's
// ZVT_CHECK_*_EMUL requires rs1()/rs2() % 8 < 8/kmax(), and ZVT_CHECK_ALIGN_RS1/2
// require alignment to vflmul. At e8w4 (int8, widen=4) kmax()=4 and vflmul=2
// (EMUL=LMUL since EEW=SEW=8), so rs1()/rs2() must be exactly one of
// {0, 8, 16, 24} -- the four operand bases below each get their own
// non-overlapping 8-register window, which also means all 32 vector
// registers are live for the duration of the k-loop (freed again once the
// e32w1 tile-readout phase below reconfigures vlmul).
//
// Each 2TE x 2TE block of C is accumulated in mt0..mt12 (tile index 0..3):
//   A rows (k0..k0+3) of m-block 0 -> v0, v2, v4, v6     (vs2 = v0)
//   A rows of m-block 1           -> v8, v10, v12, v14   (vs2 = v8)
//   B rows of n-block 0           -> v16, v18, v20, v22  (vs1 = v16)
//   B rows of n-block 1           -> v24, v26, v28, v30  (vs1 = v24)
// so every A and B row feeds two sf.mm instructions. Each listed register is
// itself an LMUL=2 group (e.g. v0 covers v0:v1) holding one TE-wide row.

#ifndef VME_GEMM_H
#define VME_GEMM_H

#include "vme.h"

#define VME_LOAD_ROW(vreg, ptr) asm volatile("vle8.v " vreg ", (%0)" : : "r"(ptr) : "memory")

static inline void vme_load_k4(const uint8_t* a0, const uint8_t* a1, const uint8_t* b0, const uint8_t* b1,
                               size_t lda, size_t ldb, size_t kk) {
  VME_LOAD_ROW("v0", a0); VME_LOAD_ROW("v8", a1); VME_LOAD_ROW("v16", b0); VME_LOAD_ROW("v24", b1);
  if (kk > 1) {
    VME_LOAD_ROW("v2", a0 + lda); VME_LOAD_ROW("v10", a1 + lda);
    VME_LOAD_ROW("v18", b0 + ldb); VME_LOAD_ROW("v26", b1 + ldb);
  }
  if (kk > 2) {
    VME_LOAD_ROW("v4", a0 + 2*lda); VME_LOAD_ROW("v12", a1 + 2*lda);
    VME_LOAD_ROW("v20", b0 + 2*ldb); VME_LOAD_ROW("v28", b1 + 2*ldb);
  }
  if (kk > 3) {
    VME_LOAD_ROW("v6", a0 + 3*lda); VME_LOAD_ROW("v14", a1 + 3*lda);
    VME_LOAD_ROW("v22", b0 + 3*ldb); VME_LOAD_ROW("v30", b1 + 3*ldb);
  }
}

// c points to int32_t (int8 GEMM) or float (FP8 GEMM); M, N multiples of 2*te.
static inline void vme_gemm_2x2(void* c, const uint8_t* at, const uint8_t* b,
                                size_t M, size_t N, size_t K, size_t te, int fp8,
                                int fmt_a, int fmt_b) {
  uint32_t* cw = (uint32_t*)c;
  for (size_t m0 = 0; m0 < M; m0 += 2*te) {
    for (size_t n0 = 0; n0 < N; n0 += 2*te) {
      // B's (vs1's) sign/format is vtype.altfmt, not part of the sf.mm
      // encoding -- select it via which vme_vsettnt_e8w4* variant ran
      // (fmt_b=0: unsigned/e4m3 via vme_vsettnt_e8w4; fmt_b=1: signed/e5m2
      // via vme_vsettnt_e8w4_alt). Same altfmt bit, same meaning, for both
      // int8 and fp8 (see VME_MM_* in vme.h).
      if (fmt_b) vme_vsettnt_e8w4_alt(te); else vme_vsettnt_e8w4(te);
      vme_vsettm(te);
      VME_VTZERO(0); VME_VTZERO(1); VME_VTZERO(2); VME_VTZERO(3);
      for (size_t k0 = 0; k0 < K; k0 += 4) {
        size_t kk = (K - k0) < 4 ? (K - k0) : 4;
        const uint8_t* a0 = at + k0*M + m0;
        const uint8_t* bb = b + k0*N + n0;
        vme_load_k4(a0, a0 + te, bb, bb + te, M, N, kk);
        vme_vsettk(kk);
        // A's (vs2's) sign/format IS part of the encoding (rd bit0); B's was
        // already configured above via vtype.altfmt, so the macro picked
        // here only needs to match fmt_a -- the _U/_S (or _E4/_E5) suffix is
        // purely for readability, both aliases are identical encodings.
        if (fp8) {
          if (fmt_a) {
            VME_MM_E5_E4(0, 0, 16); VME_MM_E5_E4(1, 0, 24);
            VME_MM_E5_E4(2, 8, 16); VME_MM_E5_E4(3, 8, 24);
          } else {
            VME_MM_E4_E4(0, 0, 16); VME_MM_E4_E4(1, 0, 24);
            VME_MM_E4_E4(2, 8, 16); VME_MM_E4_E4(3, 8, 24);
          }
        } else {
          if (fmt_a) {
            VME_MM_S_U(0, 0, 16); VME_MM_S_U(1, 0, 24);
            VME_MM_S_U(2, 8, 16); VME_MM_S_U(3, 8, 24);
          } else {
            VME_MM_U_U(0, 0, 16); VME_MM_U_U(1, 0, 24);
            VME_MM_U_U(2, 8, 16); VME_MM_U_U(3, 8, 24);
          }
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

static inline void vme_load_k4_1(const uint8_t* a0, const uint8_t* b0,
                                  size_t lda, size_t ldb, size_t kk) {
  VME_LOAD_ROW("v0", a0); VME_LOAD_ROW("v16", b0);
  if (kk > 1) { VME_LOAD_ROW("v2", a0 + lda); VME_LOAD_ROW("v18", b0 + ldb); }
  if (kk > 2) { VME_LOAD_ROW("v4", a0 + 2*lda); VME_LOAD_ROW("v20", b0 + 2*ldb); }
  if (kk > 3) { VME_LOAD_ROW("v6", a0 + 3*lda); VME_LOAD_ROW("v22", b0 + 3*ldb); }
}

// One TE x TE tile per (m0,n0) outer position (not vme_gemm_2x2's 2x2 super-
// tile), with the last row/column block clamped to whatever's left when M or
// N isn't a multiple of TE. Simpler and slower than vme_gemm_2x2 (no shared
// A/B load across a 2x2 grid), but correctly handles arbitrary M/N -- for
// edge-case coverage, not the throughput path.
static inline void vme_gemm_tiled(void* c, const uint8_t* at, const uint8_t* b,
                                   size_t M, size_t N, size_t K, size_t te, int fp8,
                                   int fmt_a, int fmt_b) {
  uint32_t* cw = (uint32_t*)c;
  for (size_t m0 = 0; m0 < M; m0 += te) {
    size_t tm = (M - m0 < te) ? (M - m0) : te;
    for (size_t n0 = 0; n0 < N; n0 += te) {
      size_t tn = (N - n0 < te) ? (N - n0) : te;
      // vme_vsettnt_e8w4*(n) also sets vl=n as a side effect (it doubles as
      // "set tile N"/vl) -- but the row loads below need vl=te regardless of
      // tn, since A's row spans tm (which can exceed tn for a partial tile).
      // A plain vsetvli can't substitute for it here: vsettnt/vsettm
      // configure matrix-mode vtype/mtype state beyond vl that sf.mm depends
      // on, and mixing in a plain vsetvli traps (same reason sf.msetmtype
      // needs its own two-call dance in vme.h). So this calls vsettnt_e8w4*
      // twice per k-chunk, staying entirely within the native instruction
      // family: once with te (sets vl=te for the loads), once with tn (sets
      // mtype.tn for sf.mm) right before issuing it.
      //
      // VTZERO itself needs matrix mode already established (it trapped when
      // called before any vsettnt_e8w4*/vsettm in this iteration) -- so set
      // tn/tm once up front just for that, redundantly re-set inside the loop.
      if (fmt_b) vme_vsettnt_e8w4_alt(tn); else vme_vsettnt_e8w4(tn);
      vme_vsettm(tm);
      VME_VTZERO(0);
      size_t k0 = 0;
      const uint8_t* a0 = at + m0;
      const uint8_t* bb = b + n0;
      while (K - k0 > 4) {
        if (fmt_b) vme_vsettnt_e8w4_alt(te); else vme_vsettnt_e8w4(te);
        vme_load_k4_1(a0, bb, M, N, 4);
        if (fmt_b) vme_vsettnt_e8w4_alt(tn); else vme_vsettnt_e8w4(tn);
        vme_vsettm(tm);
        vme_vsettk(4);
        if (fp8) { if (fmt_a) VME_MM_E5_E4(0, 0, 16); else VME_MM_E4_E4(0, 0, 16); }
        else     { if (fmt_a) VME_MM_S_U(0, 0, 16);   else VME_MM_U_U(0, 0, 16); }
        a0 += 4 * M;
        bb += 4 * N;
        k0 += 4;
      }
      {
        size_t kk = K - k0;
        if (fmt_b) vme_vsettnt_e8w4_alt(te); else vme_vsettnt_e8w4(te);
        vme_load_k4_1(a0, bb, M, N, kk);
        if (fmt_b) vme_vsettnt_e8w4_alt(tn); else vme_vsettnt_e8w4(tn);
        vme_vsettm(tm);
        vme_vsettk(kk);
        if (fp8) { if (fmt_a) VME_MM_E5_E4(0, 0, 16); else VME_MM_E4_E4(0, 0, 16); }
        else     { if (fmt_a) VME_MM_S_U(0, 0, 16);   else VME_MM_U_U(0, 0, 16); }
      }
      // write back tm rows x tn columns (not the full te x te tile)
      vme_vsettnt_e32w1(tn);
      for (size_t r = 0; r < tm; r++) {
        uint32_t* dst = cw + (m0 + r) * N + n0;
        VME_VTMV_V_T(16, vme_tss(0, VME_TSS_ROW, r));
        asm volatile("vse32.v v16, (%0)" : : "r"(dst) : "memory");
      }
    }
  }
  VME_VTDISCARD();
}

#endif // VME_GEMM_H
