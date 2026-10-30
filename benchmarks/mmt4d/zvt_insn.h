// zvt_insn.h -- the ONLY place Zvt encodings live.
//
// Every MATCH_* constant below is copied verbatim (never hand-derived) from
// riscv/encoding.h in the Zvt-capable Spike checkout used for this project:
//   /scratch/milesrusch/chipyard/toolchains/riscv-tools/riscv-isa-sim
//   (upstream riscv-software-src/riscv-isa-sim, commit 609dbe0b; Zvt added in
//   commit 72958551 "Add Zvt support"). See docs/NOTES.md for the full
//   inventory this header is built from (operand layouts, TSS encoding, tile
//   addressing, state-enable bits, etc).
//
// Each macro expands to a single `.insn 4, <expr>` instruction, where <expr>
// is `MATCH_x | (field << pos) | ...`, exactly as planned in CLAUDE.md 0.4.
// This syntax was confirmed to assemble correctly with this project's GCC
// (see docs/NOTES.md "Phase 0.4") -- no .word fallback was needed.
//
// Standard R-type field positions (riscv/decode.h: opcode=x(0,7), rd=x(7,5),
// funct3=x(12,3), rs1=x(15,5), rs2=x(20,5), funct7=x(25,7)) apply to every
// instruction here; none of them repurpose those positions except as noted
// per-instruction below.
//
// This header only emits raw instruction words -- it does not manage which
// physical register holds what value. Phase 1 kernels are responsible for
// binding real operands (e.g. via GCC register variables bound to fixed
// registers) to the register-number arguments taken here; see CLAUDE.md's
// "Inline-asm rules" section.

#ifndef ZVT_INSN_H
#define ZVT_INSN_H

#define ZVT_STR_(x) #x
#define ZVT_STR(x) ZVT_STR_(x)

// ---------------------------------------------------------------------------
// Matrix configuration instructions (Zvtbase). All funct3=0x7, like vsetvl.
// riscv/encoding.h:1598-1607

#define MATCH_MSETMTYPE   0x84007057 // riscv/encoding.h:1598
#define MASK_MSETMTYPE    0xfe00707f // riscv/encoding.h:1599
#define MATCH_MSETMTYPEI  0x88407057 // riscv/encoding.h:1600
#define MASK_MSETMTYPEI   0xee7e707f // riscv/encoding.h:1601
#define MATCH_MSETTK      0x88207057 // riscv/encoding.h:1602
#define MASK_MSETTK       0xfff0707f // riscv/encoding.h:1603
#define MATCH_MSETTM      0x88107057 // riscv/encoding.h:1604
#define MASK_MSETTM       0xfff0707f // riscv/encoding.h:1605
#define MATCH_MSETTN      0x88007057 // riscv/encoding.h:1606
#define MASK_MSETTN       0xfff0707f // riscv/encoding.h:1607

// msetmtype rd(=x0), rs1(=mtype value), rs2(=vtype value). No rd write
// (riscv/insns/msetmtype.h): rd is fixed 0 by MATCH_MSETMTYPE itself.
#define ZVT_MSETMTYPE(rs1, rs2) \
  asm volatile (".insn 4, " ZVT_STR(MATCH_MSETMTYPE) \
                " | ((" ZVT_STR(rs1) ") << 15) | ((" ZVT_STR(rs2) ") << 20)")

// msetmtypei: rs1's low 2 bits = mtwiden (0-3, widen code -- see NOTES.md
// "widen codes" table); vsew2 (0-3 = SEW 8/16/32/64) and altfmt (0/1) are
// packed into the instruction's I-immediate at bits 23 and 24 (vsew2) and
// bit 28 (altfmt) -- riscv/insns/msetmtypei.h: `vtype = insn.i_imm() & 0x118`.
#define ZVT_MSETMTYPEI(mtwiden, vsew2, altfmt) \
  asm volatile (".insn 4, " ZVT_STR(MATCH_MSETMTYPEI) \
                " | ((" ZVT_STR(mtwiden) ") << 15)" \
                " | ((" ZVT_STR(vsew2) ") << 23)" \
                " | ((" ZVT_STR(altfmt) ") << 28)")

// msettk/msettm/msettn rd, rs1: vsetvl-style, rd = clamped(rs1).
#define ZVT_MSETTK(rd, rs1) \
  asm volatile (".insn 4, " ZVT_STR(MATCH_MSETTK) \
                " | ((" ZVT_STR(rs1) ") << 15) | ((" ZVT_STR(rd) ") << 7)")
#define ZVT_MSETTM(rd, rs1) \
  asm volatile (".insn 4, " ZVT_STR(MATCH_MSETTM) \
                " | ((" ZVT_STR(rs1) ") << 15) | ((" ZVT_STR(rd) ") << 7)")
#define ZVT_MSETTN(rd, rs1) \
  asm volatile (".insn 4, " ZVT_STR(MATCH_MSETTN) \
                " | ((" ZVT_STR(rs1) ") << 15) | ((" ZVT_STR(rd) ") << 7)")

// ---------------------------------------------------------------------------
// Load/store tile subset to memory (Zvtbase for TEW=32; Zvt8e/16e/64e for
// the other widths). riscv/encoding.h:3700-3723.
//
// rs1 = memory base address, rs2 = TSS (Tile Subset Specifier) register.
// rd is unconstrained by MASK_VTLE*/VTSE* but unread by the handler
// (riscv/v_ext_macros.h ZVT_LDST) -- always emitted as 0 here.
//
// TSS bit layout (riscv/vector_unit.h tss_tile/tss_type/tss_index), build
// with zvt_tss() below: bits[30:27]=tile number, bit[24]=pattern
// (0=row-fixed/iterate-columns, 1=column-fixed/iterate-rows),
// bits up to ete(eew)-1 = the fixed row/column index.

#define MATCH_VTLE8   0x12007007 // riscv/encoding.h:3706
#define MASK_VTLE8    0xfe007fff // riscv/encoding.h:3707
#define MATCH_VTLE16  0x32007007 // riscv/encoding.h:3700
#define MASK_VTLE16   0xfe007fff // riscv/encoding.h:3701
#define MATCH_VTLE32  0x52007007 // riscv/encoding.h:3702
#define MASK_VTLE32   0xfe007fff // riscv/encoding.h:3703
#define MATCH_VTLE64  0x72007007 // riscv/encoding.h:3704
#define MASK_VTLE64   0xfe007fff // riscv/encoding.h:3705
#define MATCH_VTSE8   0x12007027 // riscv/encoding.h:3722
#define MASK_VTSE8    0xfe007fff // riscv/encoding.h:3723
#define MATCH_VTSE16  0x32007027 // riscv/encoding.h:3716
#define MASK_VTSE16   0xfe007fff // riscv/encoding.h:3717
#define MATCH_VTSE32  0x52007027 // riscv/encoding.h:3718
#define MASK_VTSE32   0xfe007fff // riscv/encoding.h:3719
#define MATCH_VTSE64  0x72007027 // riscv/encoding.h:3720
#define MASK_VTSE64   0xfe007fff // riscv/encoding.h:3721

#define ZVT_LDST_(match, addr, tss) \
  asm volatile (".insn 4, " ZVT_STR(match) \
                " | ((" ZVT_STR(addr) ") << 15) | ((" ZVT_STR(tss) ") << 20)")
#define ZVT_VTLE8(addr, tss)  ZVT_LDST_(MATCH_VTLE8,  addr, tss)
#define ZVT_VTLE16(addr, tss) ZVT_LDST_(MATCH_VTLE16, addr, tss)
#define ZVT_VTLE32(addr, tss) ZVT_LDST_(MATCH_VTLE32, addr, tss)
#define ZVT_VTLE64(addr, tss) ZVT_LDST_(MATCH_VTLE64, addr, tss)
#define ZVT_VTSE8(addr, tss)  ZVT_LDST_(MATCH_VTSE8,  addr, tss)
#define ZVT_VTSE16(addr, tss) ZVT_LDST_(MATCH_VTSE16, addr, tss)
#define ZVT_VTSE32(addr, tss) ZVT_LDST_(MATCH_VTSE32, addr, tss)
#define ZVT_VTSE64(addr, tss) ZVT_LDST_(MATCH_VTSE64, addr, tss)

// Build a TSS value. tile: logical tile index (floor-aligned to mt_align(eew)
// by hardware, so pass the exact tile index your TEW supports, e.g. 0/4/8/12
// for TEW=32). pattern: ZVT_TSS_ROW or ZVT_TSS_COL. index: fixed row/column
// index (< ete(eew)).
#define ZVT_TSS_ROW 0
#define ZVT_TSS_COL 1
static inline unsigned long zvt_tss(unsigned long tile, unsigned long pattern,
                                     unsigned long index) {
  return (tile << 27) | (pattern << 24) | index;
}

// ---------------------------------------------------------------------------
// Move tile subset between tile state and vector registers (Zvtbase for
// TEW=32; Zvt8e/16e/64e for the other widths). riscv/encoding.h:3712-3715.

#define MATCH_VTMV_T_V  0x5e006057 // riscv/encoding.h:3712
#define MASK_VTMV_T_V   0xfe007fff // riscv/encoding.h:3713
#define MATCH_VTMV_V_T  0x43f06057 // riscv/encoding.h:3714
#define MASK_VTMV_V_T   0xfff0707f // riscv/encoding.h:3715

// vtmv.t.v rs1(TSS dest), vs2(source vector reg). rd fixed 0 (unused).
#define ZVT_VTMV_T_V(tss, vs2) \
  asm volatile (".insn 4, " ZVT_STR(MATCH_VTMV_T_V) \
                " | ((" ZVT_STR(tss) ") << 15) | ((" ZVT_STR(vs2) ") << 20)")
// vtmv.v.t vd(dest vector reg), rs1(TSS source). rs2 fixed 0x1f (unused).
#define ZVT_VTMV_V_T(vd, tss) \
  asm volatile (".insn 4, " ZVT_STR(MATCH_VTMV_V_T) \
                " | ((" ZVT_STR(vd) ") << 7) | ((" ZVT_STR(tss) ") << 15)")

// ---------------------------------------------------------------------------
// Matrix arithmetic instructions (OP-VE major opcode 0x77, except vtzero/
// vtdiscard which stay on OP-V 0x57). riscv/encoding.h:3694-3725.
//
// rd encodes the destination tile td via `td = (rd >> 1) & (16 - 16/n_tiles)`
// (riscv/v_ext_macros.h ZVT_MTD) -- the inverse used here, `rd = td << 1`,
// holds for every n_tiles (4, 8, or 16; verified algebraically in
// docs/NOTES.md). vs2 is operand A (m-indexed, encoded as rs2 -- A's sign/
// format is fixed by which of the two sibling mnemonics is used, e.g.
// vtmmu vs vtmms). vs1 is operand B (n-indexed, encoded as rs1 -- B's sign/
// format is chosen dynamically by vtype.altfmt, set via ZVT_MSETMTYPEI).

#define MATCH_VTFMM_TVV      0xf2001077 // riscv/encoding.h:3698
#define MASK_VTFMM_TVV       0xfe0071ff // riscv/encoding.h:3699
#define MATCH_VTFMM_ALT_TVV  0xf20010f7 // riscv/encoding.h:3696
#define MASK_VTFMM_ALT_TVV   0xfe0071ff // riscv/encoding.h:3697
#define MATCH_VTMMU_TVV      0xf2000077 // riscv/encoding.h:3710
#define MASK_VTMMU_TVV       0xfe0071ff // riscv/encoding.h:3711
#define MATCH_VTMMS_TVV      0xf20000f7 // riscv/encoding.h:3708
#define MASK_VTMMS_TVV       0xfe0071ff // riscv/encoding.h:3709

#define ZVT_MM_(match, td, vs2, vs1) \
  asm volatile (".insn 4, " ZVT_STR(match) \
                " | ((" ZVT_STR(td)  ") << 8)" \
                " | ((" ZVT_STR(vs1) ") << 15)" \
                " | ((" ZVT_STR(vs2) ") << 20)")
// f32xf32->f32 (Zvtfmm), f64xf64->f64 (Zvtdmm), fp16/ofp8 (see NOTES.md).
#define ZVT_VTFMM_TVV(td, vs2, vs1)     ZVT_MM_(MATCH_VTFMM_TVV,     td, vs2, vs1)
#define ZVT_VTFMM_ALT_TVV(td, vs2, vs1) ZVT_MM_(MATCH_VTFMM_ALT_TVV, td, vs2, vs1)
// s8s8s32 (Zvti8i32mm): vtmmu = A(vs2) unsigned; vtmms = A(vs2) signed.
// B(vs1)'s sign comes from vtype.altfmt (0=unsigned, 1=signed).
#define ZVT_VTMMU_TVV(td, vs2, vs1)     ZVT_MM_(MATCH_VTMMU_TVV,     td, vs2, vs1)
#define ZVT_VTMMS_TVV(td, vs2, vs1)     ZVT_MM_(MATCH_VTMMS_TVV,     td, vs2, vs1)

// ---------------------------------------------------------------------------
// Matrix tile-zero and context-discard instructions (Zvtbase).

#define MATCH_VTZERO     0x43e06057 // riscv/encoding.h:3724
#define MASK_VTZERO      0xfffff0ff // riscv/encoding.h:3725
#define MATCH_VTDISCARD  0x43c06057 // riscv/encoding.h:3694
#define MASK_VTDISCARD   0xffffffff // riscv/encoding.h:3695

// vtzero mtd: zeroes the tm x tn submatrix of tile td. 16 addressable slots
// (n_tiles=16 in ZVT_MTD), same `rd = td << 1` rule as the matmul ops.
#define ZVT_VTZERO(td) \
  asm volatile (".insn 4, " ZVT_STR(MATCH_VTZERO) " | ((" ZVT_STR(td) ") << 8)")
// vtdiscard: no operands. Resets mstatus.MS (and vsstatus.MS if V=1) from
// Dirty back to Initial; does not clear tile storage.
#define ZVT_VTDISCARD() \
  asm volatile (".insn 4, " ZVT_STR(MATCH_VTDISCARD))

// mtype CSR (riscv/encoding.h:3936). Read-only from the Zvt instructions'
// point of view -- written only as a side effect of msetmtype(i)/msettm/
// msettk; exposed here for diagnostics (e.g. smoke_zvt.c).
#define CSR_MTYPE 0xc23

#endif // ZVT_INSN_H
