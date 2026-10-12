// OPFMACC.S end to end: a 64x64x96 GEMM with P3109 block scales (block 32) on
// the 2x2 kernel, rule A registers (data in v2 v4 v6 v8, their scales in
// v3 v5 v7 v9), compared bit for bit with gen_data.py's reference. New scales
// are loaded at every block boundary, so the OPU's scale cache must drop the
// old ones; in block 1 one scale register is written by vmv1r.v instead of a
// load, the register file's other write path.
#include <stdio.h>
#include <stdint.h>
#include "rvv_mx.h"
#include "bme.h"

#define M 64
#define N 64
#define K 96
#define BLK 32

extern uint8_t a0[], b0[], sa0[], sb0[], a1[], b1[], sa1[], sb1[];
extern uint32_t c0[], out0[], c1[], out1[];

#define LD(v, p) asm volatile("vle8.v " v ", (%0)" : : "r"(p))

static void gemm(const uint8_t *a, const uint8_t *b, const uint8_t *sa, const uint8_t *sb,
                 uint32_t *c, int altfmt) {
    asm volatile("vsetvli x0, %0, e32, m4, ta, ma\n\tvmv.v.i v12, 0" : : "r"(32));
    OPMVINBCAST(m0, v12); OPMVINBCAST(m1, v12); OPMVINBCAST(m2, v12); OPMVINBCAST(m3, v12);
    if (altfmt) VSETVLI_ALTFMT_X0(32, SEW_E8, LMUL_M1, 1);
    else        VSETVLI_ALTFMT_X0(32, SEW_E8, LMUL_M1, 0);
    for (int blk = 0; blk < K / BLK; blk++) {
        LD("v3", &sa[M * blk]);
        LD("v5", &sa[M * blk + 32]);
        LD("v7", &sb[N * blk]);
        if (blk == 1) {
            LD("v20", &sb[N * blk + 32]);
            asm volatile("vmv1r.v v9, v20");
        } else {
            LD("v9", &sb[N * blk + 32]);
        }
        for (int k = blk * BLK; k < (blk + 1) * BLK; k++) {
            LD("v2", &a[M * k]);
            LD("v4", &a[M * k + 32]);
            LD("v6", &b[N * k]);
            LD("v8", &b[N * k + 32]);
            OPFMACCS(m0, v6, v2);
            OPFMACCS(m1, v8, v2);
            OPFMACCS(m2, v6, v4);
            OPFMACCS(m3, v8, v4);
        }
    }
    asm volatile("vsetvli x0, %0, e32, m4, ta, ma" : : "r"(32));
    for (int r = 0; r < 32; r++) {
        VMV_VR(v12, r, m0); asm volatile("vse32.v v12, (%0)" : : "r"(&c[r * N]) : "memory");
        VMV_VR(v12, r, m1); asm volatile("vse32.v v12, (%0)" : : "r"(&c[r * N + 32]) : "memory");
        VMV_VR(v12, r, m2); asm volatile("vse32.v v12, (%0)" : : "r"(&c[(r + 32) * N]) : "memory");
        VMV_VR(v12, r, m3); asm volatile("vse32.v v12, (%0)" : : "r"(&c[(r + 32) * N + 32]) : "memory");
    }
}

static int check(const uint32_t *out, const uint32_t *ref, int altfmt) {
    int bad = 0;
    for (int i = 0; i < M * N; i++)
        if (out[i] != ref[i] && bad++ < 4)
            printf("MISMATCH altfmt %d: m=%d n=%d got %08x want %08x\n",
                   altfmt, i / N, i % N, out[i], ref[i]);
    printf("altfmt %d: %d of %d differ\n", altfmt, bad, M * N);
    return bad != 0;
}

int main(void) {
    gemm(a0, b0, sa0, sb0, out0, 0);
    int failed = check(out0, c0, 0);
    gemm(a1, b1, sa1, sb1, out1, 1);
    failed |= check(out1, c1, 1);
    printf(failed ? "FAILED\n" : "passed\n");
    return failed;
}
