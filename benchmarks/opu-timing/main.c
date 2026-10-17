// Steady-state timing of OPU instruction patterns, for the block-scaling
// ISA design. Build with -DBASIC for the single-pattern measurements A-G,
// -DRULES=1..3 for the pairing rules, -DRULES=4 for the real OPFMACC.S;
// by default it times the scale-supply strategies H, P, S, R, W. Each pattern
// runs once to warm up, then for 1, 2, 3, 4, 6 and 8 times a base count of
// iterations, twice each; fit.py fits cycles = a + b * iterations, so b is the
// cost per iteration, free of start-up and drain, and the residuals show
// whether that cost is constant. A drain (tile row to a vector register, then
// to a scalar) makes the core wait for the vector unit before mcycle is read.
#include <stdio.h>
#include <stdint.h>
#include "util.h"
#include "rvv_mx.h"
#include "bme.h"

uint8_t a0[32] __attribute__((aligned(64)));
uint8_t b0[32] __attribute__((aligned(64)));
uint8_t a1[32] __attribute__((aligned(64)));
uint8_t b1[32] __attribute__((aligned(64)));
uint8_t s0[32] __attribute__((aligned(64)));

static void setvl8(void) { asm volatile("vsetvli x0, %0, e8, m1, ta, ma" : : "r"(32)); }

static unsigned long drain(void) {
    unsigned long x;
    asm volatile("vsetvli x0, %0, e32, m4, ta, ma" : : "r"(32));
    VMV_VR(v8, 0, m0);
    asm volatile("vmv.x.s %0, v8" : "=r"(x));
    setvl8();
    return x;
}

#define TIMED(name, body) \
    static unsigned long name(unsigned long reps) { \
        drain(); \
        unsigned long c = read_csr(mcycle); \
        for (unsigned long i = 0; i < reps; i++) { body } \
        drain(); \
        return read_csr(mcycle) - c; \
    }

#define LD(v, p) asm volatile("vle8.v " v ", (%0)" : : "r"(p))

// A: OPFMACC back to back, one tile
TIMED(pat_a, OPFMACC(m0, v2, v1);)
// B: alternating two tiles
TIMED(pat_b, OPFMACC(m0, v2, v1); OPFMACC(m1, v2, v1);)
// C: load A and B, then OPFMACC, same registers every k
TIMED(pat_c, LD("v1", a0); LD("v2", b0); OPFMACC(m0, v2, v1);)
// D: as C, alternating register pairs (two k per iteration)
TIMED(pat_d, LD("v1", a0); LD("v2", b0); OPFMACC(m0, v2, v1);
             LD("v3", a1); LD("v4", b1); OPFMACC(m0, v4, v3);)
// E: D plus one extra load per k
TIMED(pat_e, LD("v1", a0); LD("v2", b0); LD("v5", s0); OPFMACC(m0, v2, v1);
             LD("v3", a1); LD("v4", b1); LD("v6", s0); OPFMACC(m0, v4, v3);)
// D32, F: 32 k unrolled; F adds one extra load per 32 k (a scale load per
// MX block). No branch in the loop body, which would change how the core
// feeds the vector unit.
#define KK LD("v1", a0); LD("v2", b0); OPFMACC(m0, v2, v1); \
           LD("v3", a1); LD("v4", b1); OPFMACC(m0, v4, v3);
#define KK4 KK KK KK KK
TIMED(pat_d32, KK4 KK4 KK4 KK4)
TIMED(pat_f, LD("v5", s0); KK4 KK4 KK4 KK4)
// G: move all 32 rows of a tile out
TIMED(pat_g, asm volatile("vsetvli x0, %0, e32, m4, ta, ma" : : "r"(32));
             for (unsigned long r = 0; r < 32; r++) VMV_VR(v8, r, m0);
             setvl8();)


// ---- Scale-supply strategies on a 2x2 tile kernel, per 32-k block ----
// Tiles m0..m3 = (A0,B0) (A0,B1) (A1,B0) (A1,B1); A0, A1, B0, B1 in v2..v5,
// clear of the IME scale pair v0/v1. OPFMACC is unscaled: these
// patterns measure only the instructions each strategy adds around it.

uint8_t s1[32] __attribute__((aligned(64)));
float cbuf[4][32][32] __attribute__((aligned(64)));

#define X8(x) x x x x x x x x
#define X32(x) X8(x) X8(x) X8(x) X8(x)
#define K22 LD("v2", a0); LD("v3", a1); LD("v4", b0); LD("v5", b1); \
            OPFMACC(m0, v4, v2); OPFMACC(m1, v5, v2); OPFMACC(m2, v4, v3); OPFMACC(m3, v5, v3);
// IME paired layout for one (A, B) scale set: 32 16-bit (sA, sB) pairs
#define BUILD(dst) LD("v16", s0); LD("v17", s1); \
    asm volatile("vsetvli x0, %0, e16, m2, ta, ma" : : "r"(32)); \
    asm volatile("vzext.vf2 " dst ", v16\n\tvzext.vf2 v20, v17\n\t" \
                 "vsll.vi v20, v20, 8\n\tvor.vv " dst ", " dst ", v20"); setvl8();
#define MV(src) asm volatile("vmv2r.v v0, " src);
#define K22S LD("v2", a0); LD("v3", a1); LD("v4", b0); LD("v5", b1); \
             MV("v8") OPFMACC(m0, v4, v2); MV("v10") OPFMACC(m1, v5, v2); \
             MV("v12") OPFMACC(m2, v4, v3); MV("v14") OPFMACC(m3, v5, v3);
#define KT(m) LD("v2", a0); LD("v4", b0); OPFMACC(m, v4, v2);

// H: unscaled 2x2 block
TIMED(pat_h, X32(K22))
// P: pairing, one 32-B scale load per data register per block
TIMED(pat_p, LD("v24", s0); LD("v25", s0); LD("v26", s1); LD("v27", s1); X32(K22))
// P1: pairing, the 4 scale loads spread over the first 4 k (one per k)
#define K22L(s) LD("v2", a0); LD("v3", a1); LD("v4", b0); LD("v5", b1); LD(s, s0); \
            OPFMACC(m0, v4, v2); OPFMACC(m1, v5, v2); OPFMACC(m2, v4, v3); OPFMACC(m3, v5, v3);
#define X28(x) X8(x) X8(x) X8(x) x x x x
TIMED(pat_p1, K22L("v24") K22L("v25") K22L("v26") K22L("v27") X28(K22))
// S: IME layout, 2x2 order kept: 4 paired sets per block, v0/v1 swapped per OPFMACC
TIMED(pat_s, BUILD("v8") BUILD("v10") BUILD("v12") BUILD("v14") X32(K22S))
// R: IME layout, one tile at a time: the block's 32 k per tile, data reloaded
TIMED(pat_r, BUILD("v0") X32(KT(m0)) BUILD("v0") X32(KT(m1))
             BUILD("v0") X32(KT(m2)) BUILD("v0") X32(KT(m3)))
// W: software baseline: unscaled block, then drain each tile, scale by
// sA[r] * sB[:] in FP32 and add to a C tile in memory, then clear the tile
static void drain_scale(int t) {
    asm volatile("vsetvli x0, %0, e32, m4, ta, ma" : : "r"(32));
    for (unsigned long r = 0; r < 32; r++) {
        switch (t) {
            case 0: VMV_VR(v8, r, m0); break;
            case 1: VMV_VR(v8, r, m1); break;
            case 2: VMV_VR(v8, r, m2); break;
            default: VMV_VR(v8, r, m3); break;
        }
        asm volatile("vle32.v v16, (%0)\n\tvfmul.vv v8, v8, v12\n\t"
                     "vfmacc.vf v16, %1, v8\n\tvse32.v v16, (%0)"
                     : : "r"(cbuf[t][r]), "f"(1.0f) : "memory");
    }
    asm volatile("vmv.v.i v20, 0");
    switch (t) {
        case 0: OPMVINBCAST(m0, v20); break;
        case 1: OPMVINBCAST(m1, v20); break;
        case 2: OPMVINBCAST(m2, v20); break;
        default: OPMVINBCAST(m3, v20); break;
    }
    setvl8();
}
TIMED(pat_w, X32(K22) drain_scale(0); drain_scale(1); drain_scale(2); drain_scale(3);)

// ---- Pairing rules (-DRULES=1 or 2), each with the register allocation it forces ----
// K22G: the 2x2 k step on data registers A0 A1 B0 B1; K22GL adds one scale
// load into S; K22GS the same load strided (MX's natural layout: one row's
// blocks contiguous, so one block's 32 row scales are 128 B apart for K = 4096).
uint8_t sstr[32 * 128] __attribute__((aligned(64)));
#define LDS(v, p) asm volatile("vlse8.v " v ", (%0), %1" : : "r"(p), "r"(128L))
#define K22G(A0, A1, B0, B1) LD(#A0, a0); LD(#A1, a1); LD(#B0, b0); LD(#B1, b1); \
    OPFMACC(m0, B0, A0); OPFMACC(m1, B1, A0); OPFMACC(m2, B0, A1); OPFMACC(m3, B1, A1);
#define K22GL(A0, A1, B0, B1, S) LD(#A0, a0); LD(#A1, a1); LD(#B0, b0); LD(#B1, b1); LD(#S, s0); \
    OPFMACC(m0, B0, A0); OPFMACC(m1, B1, A0); OPFMACC(m2, B0, A1); OPFMACC(m3, B1, A1);
#define K22GS(A0, A1, B0, B1, S) LD(#A0, a0); LD(#A1, a1); LD(#B0, b0); LD(#B1, b1); LDS(#S, sstr); \
    OPFMACC(m0, B0, A0); OPFMACC(m1, B1, A0); OPFMACC(m2, B0, A1); OPFMACC(m3, B1, A1);
#define X28G(x) X8(x) X8(x) X8(x) x x x x
// OPSCALE stand-in: an OPFMACC reading the two scale registers (4 OPU cycles,
// an upper bound for a real OPSCALE, which reads the same 64 B)
#define OPSC4 OPFMACC(m0, v26, v24); OPFMACC(m1, v27, v24); OPFMACC(m2, v26, v25); OPFMACC(m3, v27, v25);
TIMED(pat_h1, X32(K22G(v1, v2, v3, v4)))
TIMED(pat_h2, X32(K22G(v2, v3, v4, v5)))
TIMED(pat_h4, X32(K22G(v4, v5, v6, v7)))
TIMED(pat_he, X32(K22G(v2, v4, v6, v8)))
// A: scales in vN+1, data in even registers
TIMED(pat_pa, K22GL(v2, v4, v6, v8, v3) K22GL(v2, v4, v6, v8, v5) K22GL(v2, v4, v6, v8, v7)
              K22GL(v2, v4, v6, v8, v9) X28G(K22G(v2, v4, v6, v8)))
// B: scales in vN+16
TIMED(pat_pb, K22GL(v1, v2, v3, v4, v17) K22GL(v1, v2, v3, v4, v18) K22GL(v1, v2, v3, v4, v19)
              K22GL(v1, v2, v3, v4, v20) X28G(K22G(v1, v2, v3, v4)))
// D: 4 scale loads + 4 OPSCALE at the block start
TIMED(pat_pd, LD("v24", s0); LD("v25", s0); LD("v26", s1); LD("v27", s1); OPSC4 X32(K22G(v1, v2, v3, v4)))
// D1: the scale loads in the previous block's last 4 k steps
TIMED(pat_pd1, OPSC4 X28G(K22G(v1, v2, v3, v4)) K22GL(v1, v2, v3, v4, v24) K22GL(v1, v2, v3, v4, v25)
               K22GL(v1, v2, v3, v4, v26) K22GL(v1, v2, v3, v4, v27))
// PS: pairing with strided scale loads, spread
TIMED(pat_ps, K22GS(v1, v2, v3, v4, v24) K22GS(v1, v2, v3, v4, v25) K22GS(v1, v2, v3, v4, v26)
              K22GS(v1, v2, v3, v4, v27) X28G(K22G(v1, v2, v3, v4)))

// IME again, with data in v4-v7 (v2-v5 is the one slow allocation found; see RULES=1)
#define K22S4 LD("v4", a0); LD("v5", a1); LD("v6", b0); LD("v7", b1); \
              MV("v8") OPFMACC(m0, v6, v4); MV("v10") OPFMACC(m1, v7, v4); \
              MV("v12") OPFMACC(m2, v6, v5); MV("v14") OPFMACC(m3, v7, v5);
#define KT4(m) LD("v4", a0); LD("v6", b0); OPFMACC(m, v6, v4);
TIMED(pat_s4, BUILD("v8") BUILD("v10") BUILD("v12") BUILD("v14") X32(K22S4))
TIMED(pat_r4, BUILD("v0") X32(KT4(m0)) BUILD("v0") X32(KT4(m1))
              BUILD("v0") X32(KT4(m2)) BUILD("v0") X32(KT4(m3)))

// ---- Real OPFMACC.S, rule A (-DRULES=4): scales in vN+1 ----
#define K22SS(A0, A1, B0, B1) LD(#A0, a0); LD(#A1, a1); LD(#B0, b0); LD(#B1, b1); \
    OPFMACCS(m0, B0, A0); OPFMACCS(m1, B1, A0); OPFMACCS(m2, B0, A1); OPFMACCS(m3, B1, A1);
#define K22SSL(A0, A1, B0, B1, S) LD(#A0, a0); LD(#A1, a1); LD(#B0, b0); LD(#B1, b1); LD(#S, s0); \
    OPFMACCS(m0, B0, A0); OPFMACCS(m1, B1, A0); OPFMACCS(m2, B0, A1); OPFMACCS(m3, B1, A1);
// RA0: scales never reloaded, every OPFMACC.S hits the scale cache
TIMED(pat_ra0, X32(K22SS(v2, v4, v6, v8)))
// RA: the block's 4 scale loads at its start, then its 32 k steps
TIMED(pat_ra, LD("v3", s0); LD("v5", s0); LD("v7", s1); LD("v9", s1); X32(K22SS(v2, v4, v6, v8)))
// RA2: two register sets alternate by block; each block loads the next
// block's scales in its first 4 k steps (two blocks per iteration)
TIMED(pat_ra2, K22SSL(v2, v4, v6, v8, v11) K22SSL(v2, v4, v6, v8, v13) K22SSL(v2, v4, v6, v8, v15)
               K22SSL(v2, v4, v6, v8, v17) X28G(K22SS(v2, v4, v6, v8))
               K22SSL(v10, v12, v14, v16, v3) K22SSL(v10, v12, v14, v16, v5) K22SSL(v10, v12, v14, v16, v7)
               K22SSL(v10, v12, v14, v16, v9) X28G(K22SS(v10, v12, v14, v16)))
// HE2: the same alternation, unscaled
TIMED(pat_he2, X32(K22G(v2, v4, v6, v8)) X32(K22G(v10, v12, v14, v16)))

static const unsigned long mult[] = {1, 2, 3, 4, 6, 8};

// Prints "tag iterations cycles" lines for fit.py
static void sweep(const char *tag, const char *name, const char *unit,
                  unsigned long (*f)(unsigned long), unsigned long base) {
    f(base);
    printf("# %s %s | per %s\n", tag, name, unit);
    for (int m = 0; m < 6; m++)
        for (int r = 0; r < 2; r++)
            printf("%s %lu %lu\n", tag, base * mult[m], f(base * mult[m]));
}

int main(void) {
    for (int i = 0; i < 32; i++) a0[i] = b0[i] = a1[i] = b1[i] = 0x38, s0[i] = 128;
    setvl8();
    asm volatile("vmv.v.i v1, 0\n\tvmv.v.i v2, 0");
#if RULES == 1
    sweep("H1", "unscaled, data v1-v4", "32-k block", pat_h1, 8);
    sweep("H2", "unscaled, data v2-v5", "32-k block", pat_h2, 8);
    sweep("H4", "unscaled, data v4-v7", "32-k block", pat_h4, 8);
    sweep("HE", "unscaled, data v2 v4 v6 v8", "32-k block", pat_he, 8);
    sweep("PA", "rule A: scales in vN+1", "32-k block", pat_pa, 8);
    return 0;
#elif RULES == 2
    sweep("H1", "unscaled, data v1-v4", "32-k block", pat_h1, 8);
    sweep("PB", "rule B: scales in vN+16", "32-k block", pat_pb, 8);
    sweep("PD", "rule D: loads + OPSCALE at block start", "32-k block", pat_pd, 8);
    sweep("PD1", "rule D: loads in previous block", "32-k block", pat_pd1, 8);
    sweep("PS", "pairing, strided scale loads", "32-k block", pat_ps, 8);
    return 0;
#elif RULES == 3
    sweep("H4", "unscaled, data v4-v7", "32-k block", pat_h4, 8);
    sweep("S4", "IME swap, data v4-v7", "32-k block", pat_s4, 8);
    sweep("R4", "IME tile by tile, data v4/v6", "32-k block", pat_r4, 8);
    return 0;
#elif RULES == 4
    sweep("HE", "unscaled, data v2 v4 v6 v8", "32-k block", pat_he, 8);
    sweep("RA0", "OPFMACC.S, scales cached", "32-k block", pat_ra0, 8);
    sweep("RA", "OPFMACC.S, scale loads at block start", "32-k block", pat_ra, 8);
    sweep("HE2", "unscaled, alternating sets", "2 blocks", pat_he2, 4);
    sweep("RA2", "OPFMACC.S, alternating sets, loads spread", "2 blocks", pat_ra2, 4);
    return 0;
#endif
#ifdef BASIC
    sweep("A", "opfmacc", "instruction", pat_a, 64);
    sweep("B", "two tiles", "2 instructions", pat_b, 64);
    sweep("C", "load+opfmacc", "k", pat_c, 64);
    sweep("D", "double-buffered", "2 k", pat_d, 64);
    sweep("E", "+1 load per k", "2 k", pat_e, 64);
    sweep("D32", "double-buffered, unrolled", "32 k", pat_d32, 16);
    sweep("F", "+1 load per 32 k", "32 k", pat_f, 16);
    sweep("G", "drain 32 rows", "tile", pat_g, 16);
#endif
    asm volatile("vsetvli x0, %0, e32, m4, ta, ma\n\tvfmv.v.f v12, %1" : : "r"(32), "f"(1.0f));
    setvl8();
    sweep("H", "2x2 unscaled", "32-k block", pat_h, 8);
    sweep("P", "pairing", "32-k block", pat_p, 8);
    sweep("P1", "pairing, scale loads spread", "32-k block", pat_p1, 8);
    sweep("S", "IME, swap per OPFMACC", "32-k block", pat_s, 8);
    sweep("R", "IME, tile by tile", "32-k block", pat_r, 8);
    sweep("W", "software baseline", "32-k block", pat_w, 8);
    return 0;
}
