// mmt4d_zvt_s8.c -- single-tile (mt0) Zvt s8xs8->s32 mmt4d kernel.
// M0 = tm, N0 = tn, K0 = KMAX = 4 (fixed -- see docs/NOTES.md: for SEW=8,
// widen=4, KMAX = max(1,32/SEW) = 4, and this is inherent to the hardware's
// int8 widening, not a free choice).
//
// Per docs/NOTES.md's register-layout finding: a single vtmms.tvv/vtmmu.tvv
// call consumes all tk=K0=4 k-rows at once, but those k-rows live in
// *separate* vector registers spaced kmaxmax(8)/kmax(4)=2 apart (v8,v10,v12,
// v14 for A; v16,v18,v20,v22 for B) -- NOT packed into one register's lanes.
// Since the lhs/rhs panels are packed [K1][M0][K0]/[K1][N0][K0] (K0-
// contiguous, matching IREE/the spec), gathering "all M0 values for one
// fixed k0" into a single register is a strided load (vlse8.v, stride=K0
// bytes), one per k0 value (4 for A, 4 for B per k1 group) -- not a plain
// unit-stride vle8.v.
//
// Sign choice: both operands are plain signed int8 (CLAUDE.md: "sign-extend
// before multiplying"), which per the spec/Spike's encoding is vtmms.tvv
// (fixes A's sign in the mnemonic itself) with vtype.altfmt=1 (selects B
// signed) -- see docs/NOTES.md's matmul operand table.
//
// The output tile is always TEW=32 (accumulator is s32 regardless of input
// width), so zero/accumulate-load/store use the same per-row pattern as
// mmt4d_zvt_f32.c -- including that file's v20 note: the per-row out<->tile
// moves go through a scratch vector register (v24 here, since v8-v23 are
// live for the k1 loop's operands) via vle32.v/vse32.v plus
// vtmv.t.v/vtmv.v.t, not vtle32/vtse32 (removed from the RTL after causing
// simulator hangs; see Saturn's `b78d933`/`766ed92`).
//
// Unlike mmt4d_zvt_f32.c, that substitution isn't a drop-in here: vtmv.t.v/
// vtmv.v.t require vtype e32w1 (per the spec), but this kernel's matmul runs
// in e8w4 -- and vtle32/vtse32 didn't have that dependency (self-describing
// by opcode), so the bug was invisible while they were still in use. So this
// kernel now does three separate msetmtypei configs (e32w1 for zero/
// accumulate-load, e8w4 for the k1 loop, e32w1 again for the store) instead
// of the single one-shot config the vtle32/vtse32 version got away with --
// each one the same sacrificial-double-call dance vme.h's vme_vsettnt_e32w1/
// vme_vsettnt_e8w4_alt use for any SEW change.
//
// One combined asm volatile block, fixed registers, full clobber list, per
// CLAUDE.md's inline-asm rules. Freestanding: no libc, no allocation, no
// includes outside kernels/include.
#include "mmt4d_params.h"
#include "zvt_insn.h"

void mmt4d_zvt_s8(void* out_tile, const void* lhs_panel,
                   const void* rhs_panel, const mmt4d_params_t* params) {
  long lhs_ptr = (long)lhs_panel;
  long rhs_ptr = (long)rhs_panel;
  long out_ptr = (long)out_tile;
  long m0 = params->M0;
  long n0 = params->N0;
  long k1 = params->K;
  long accumulate = (params->flags & MMT4D_FLAG_ACCUMULATE) != 0;

  asm volatile(
      "mv a2, %[lhsp]\n\t"
      "mv a3, %[rhsp]\n\t"
      "mv a4, %[outp]\n\t"
      "mv a7, %[m0v]\n\t"
      "mv t0, %[n0v]\n\t"
      "mv t1, %[k1v]\n\t"
      "mv t2, %[accv]\n\t"
      "slli t3, t0, 2\n\t" // t3 = n0_bytes_i32 = N0*4 (out tile row stride)
      "slli t4, a7, 2\n\t" // t4 = m0_bytes_i8  = M0*K0 = M0*4 (lhs per-k1 group)
      "slli t5, t0, 2\n\t" // t5 = n0_bytes_i8  = N0*K0 = N0*4 (rhs per-k1 group)
      "li s4, 4\n\t"       // s4 = K0 = stride (bytes) for the strided k0 loads

      // phase 1: e32w1, for zero/accumulate-load (widen=1, vsew2=2 (e32),
      // altfmt=0) -- sacrificial call first, same SEW-change dance as
      // vme_vsettnt_e32w1 in vme.h
      ".insn 4, " ZVT_STR(MATCH_MSETMTYPEI) " | (1 << 15) | (2 << 23) | (0 << 28)\n\t"
      ".insn 4, " ZVT_STR(MATCH_MSETMTYPEI) " | (1 << 15) | (2 << 23) | (0 << 28)\n\t"
      "mv a1, a7\n\t"
      ".insn 4, " ZVT_STR(MATCH_MSETTM) " | (11 << 15) | (0 << 7)\n\t"
      "li a1, 1\n\t"
      ".insn 4, " ZVT_STR(MATCH_MSETTK) " | (11 << 15) | (0 << 7)\n\t"
      "mv a1, t0\n\t"
      ".insn 4, " ZVT_STR(MATCH_MSETTN) " | (11 << 15) | (0 << 7)\n\t" // vl=N0

      "beqz t2, 3f\n\t"
      // accumulate: load existing out rows into mt0 (e32w1; vl already = N0)
      "li a6, 0\n\t"
      "1:\n\t"
      "beq a6, a7, 2f\n\t"
      "mul s2, a6, t3\n\t"
      "add t6, a4, s2\n\t"
      "vle32.v v24, (t6)\n\t"
      ".insn 4, " ZVT_STR(MATCH_VTMV_T_V) " | (16 << 15) | (24 << 20)\n\t" // rs1=a6(TSS,x16), rs2=v24
      "addi a6, a6, 1\n\t"
      "j 1b\n\t"
      "2:\n\t"
      "j 4f\n\t"
      "3:\n\t"
      ".insn 4, " ZVT_STR(MATCH_VTZERO) " | (0 << 8)\n\t"
      "4:\n\t"

      // phase 2: e8w4, for the matmul k1 loop (widen=3 -> widen=4, vsew2=0
      // (e8), altfmt=1 (B signed)) -- sacrificial call first
      ".insn 4, " ZVT_STR(MATCH_MSETMTYPEI) " | (3 << 15) | (0 << 23) | (1 << 28)\n\t"
      ".insn 4, " ZVT_STR(MATCH_MSETMTYPEI) " | (3 << 15) | (0 << 23) | (1 << 28)\n\t"
      "mv a1, a7\n\t"
      ".insn 4, " ZVT_STR(MATCH_MSETTM) " | (11 << 15) | (0 << 7)\n\t"
      "li a1, 4\n\t"
      ".insn 4, " ZVT_STR(MATCH_MSETTK) " | (11 << 15) | (0 << 7)\n\t"
      "mv a1, t0\n\t"
      ".insn 4, " ZVT_STR(MATCH_MSETTN) " | (11 << 15) | (0 << 7)\n\t" // vl=N0

      // k1 loop: one vtmms.tvv per k1 group (tk=K0=4 consumed per call)
      "li a5, 0\n\t"
      "5:\n\t"
      "beq a5, t1, 6f\n\t"
      "mv a1, a7\n\t" // vl=M0 for A's 4 strided loads
      ".insn 4, " ZVT_STR(MATCH_MSETTN) " | (11 << 15) | (0 << 7)\n\t"
      "addi s3, a2, 0\n\t"
      "vlse8.v v8, (s3), s4\n\t"
      "addi s3, a2, 1\n\t"
      "vlse8.v v10, (s3), s4\n\t"
      "addi s3, a2, 2\n\t"
      "vlse8.v v12, (s3), s4\n\t"
      "addi s3, a2, 3\n\t"
      "vlse8.v v14, (s3), s4\n\t"
      "add a2, a2, t4\n\t"
      "mv a1, t0\n\t" // vl=N0 for B's 4 strided loads and the matmul
      ".insn 4, " ZVT_STR(MATCH_MSETTN) " | (11 << 15) | (0 << 7)\n\t"
      "addi s3, a3, 0\n\t"
      "vlse8.v v16, (s3), s4\n\t"
      "addi s3, a3, 1\n\t"
      "vlse8.v v18, (s3), s4\n\t"
      "addi s3, a3, 2\n\t"
      "vlse8.v v20, (s3), s4\n\t"
      "addi s3, a3, 3\n\t"
      "vlse8.v v22, (s3), s4\n\t"
      "add a3, a3, t5\n\t"
      ".insn 4, " ZVT_STR(MATCH_VTMMS_TVV) " | (0 << 8) | (16 << 15) | (8 << 20)\n\t"
      "addi a5, a5, 1\n\t"
      "j 5b\n\t"
      "6:\n\t"

      // phase 3: e32w1 again, for the store -- same config as phase 1,
      // sacrificial call first (crossing back from e8w4)
      ".insn 4, " ZVT_STR(MATCH_MSETMTYPEI) " | (1 << 15) | (2 << 23) | (0 << 28)\n\t"
      ".insn 4, " ZVT_STR(MATCH_MSETMTYPEI) " | (1 << 15) | (2 << 23) | (0 << 28)\n\t"
      "mv a1, a7\n\t"
      ".insn 4, " ZVT_STR(MATCH_MSETTM) " | (11 << 15) | (0 << 7)\n\t"
      "li a1, 1\n\t"
      ".insn 4, " ZVT_STR(MATCH_MSETTK) " | (11 << 15) | (0 << 7)\n\t"
      "mv a1, t0\n\t"
      ".insn 4, " ZVT_STR(MATCH_MSETTN) " | (11 << 15) | (0 << 7)\n\t" // vl=N0

      // store mt0 rows -> out (e32w1; vl=N0)
      "li a6, 0\n\t"
      "7:\n\t"
      "beq a6, a7, 8f\n\t"
      "mul s2, a6, t3\n\t"
      "add t6, a4, s2\n\t"
      ".insn 4, " ZVT_STR(MATCH_VTMV_V_T) " | (24 << 7) | (16 << 15)\n\t" // vd=v24, rs1=a6(TSS,x16)
      "vse32.v v24, (t6)\n\t"
      "addi a6, a6, 1\n\t"
      "j 7b\n\t"
      "8:\n\t"

      ".insn 4, " ZVT_STR(MATCH_VTDISCARD) "\n\t"
      :
      : [lhsp] "r"(lhs_ptr), [rhsp] "r"(rhs_ptr), [outp] "r"(out_ptr),
        [m0v] "r"(m0), [n0v] "r"(n0), [k1v] "r"(k1), [accv] "r"(accumulate)
      : "a1", "a2", "a3", "a4", "a5", "a6", "a7", "t0", "t1", "t2", "t3", "t4",
        "t5", "t6", "s2", "s3", "s4", "v8", "v9", "v10", "v11", "v12", "v13",
        "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22", "v23",
        "v24", "v25", "v26", "v27", "vl", "vtype", "memory");
}
