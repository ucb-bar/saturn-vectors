// vme.h: Xsfmm v0.6.6 (RISC-V VME stand-in) subset for the Saturn outer-product unit.
//
// GCC's assembler has no sf.* mnemonics, so each instruction is emitted with
// .insn using the exact Xsfmm encodings. Register operands are passed as the
// x-register with the same number (".insn r" only takes x names), e.g. vector
// register v8 is written "x8". Use the VME_V(n) / VME_MT(n) helpers below.
//
// Tiles (TEW = 32): mt0, mt4, mt8, mt12 -> tile index 0..3.
//
// Encodings (opcode OP-V = 0x57, OP-VE = 0x77):
//   sf.vsettnt rd, rs1, eX, wY   vsetvli rd, rs1, (vsew << 3) | (alt << 8) | (vtwiden << 9)
//   sf.vsettn/m/k rd, rs1        1 000010 {00000,00001,00010} rs1 111 rd 1010111
//   sf.mm.<a>.<b> mtd, vs2, vs1  int8: 11110a 1 vs2 vs1 000 tt00b 1110111
//                                fp8 : 11111a 1 vs2 vs1 001 tt00b 1110111
//   sf.vtmv.v.t vd, rs1          010000 1 11111 rs1 110 vd    1010111
//   sf.vtmv.t.v rs1, vs2         010111 1 vs2   rs1 110 00000 1010111
//   sf.vtzero.t mtd              010000 1 11110 00000 110 tttt0 1010111
//   sf.vtdiscard                 010000 1 11100 00000 110 00000 1010111
//   sf.vtle32 rs2(TSS), (rs1)    eee=010 1 00 1 rs2 rs1 111 00000 0000111
//   sf.vtse32 rs2(TSS), (rs1)    eee=010 1 00 1 rs2 rs1 111 00000 0100111
//
// Tile subset specifier (TSS): [30:27] tile specifier (0..15), [26:24] pattern
// (0 row, 1 column), [23:0] row/column index.

#ifndef VME_H
#define VME_H

#include <stddef.h>
#include <stdint.h>

#define VME_STR_(x) #x
#define VME_STR(x) VME_STR_(x)

// Register operand spellings for .insn
#define VME_V(n)  "x" VME_STR(n)          // vector register vn
#define VME_MT(t) "x" VME_STR(VME_MT_RD_##t) // tile rd field (tile << 3)
#define VME_MT_RD_0 0
#define VME_MT_RD_1 8
#define VME_MT_RD_2 16
#define VME_MT_RD_3 24
// rd field with the <b> operand-type bit (instruction bit 7) set
#define VME_MTB(t) "x" VME_STR(VME_MTB_RD_##t)
#define VME_MTB_RD_0 1
#define VME_MTB_RD_1 9
#define VME_MTB_RD_2 17
#define VME_MTB_RD_3 25

#define VME_TSS_ROW 0
#define VME_TSS_COL 1
static inline size_t vme_tss(size_t tile, size_t pattern, size_t index) {
  // tile index 0..3 -> tile specifier 0, 4, 8, 12
  return ((tile * 4) << 27) | (pattern << 24) | index;
}

// ---------------------------------------------------------------------------
// Configuration

// sf.vsettnt rd, rs1, e8, w4   (int8 / fp8 multiplicands, 32-bit accumulators)
static inline size_t vme_vsettnt_e8w4(size_t atn) {
  size_t tn;
  asm volatile(".insn i 0x57, 7, %0, %1, 0x600" : "=r"(tn) : "r"(atn));
  return tn;
}
// sf.vsettnt rd, rs1, e32, w1  (32-bit tile moves)
static inline size_t vme_vsettnt_e32w1(size_t atn) {
  size_t tn;
  asm volatile(".insn i 0x57, 7, %0, %1, 0x210" : "=r"(tn) : "r"(atn));
  return tn;
}
// sf.vsettn / sf.vsettm / sf.vsettk
static inline size_t vme_vsettn(size_t atn) {
  size_t r;
  asm volatile(".insn r 0x57, 7, 0x42, %0, %1, x0" : "=r"(r) : "r"(atn));
  return r;
}
static inline size_t vme_vsettm(size_t atm) {
  size_t r;
  asm volatile(".insn r 0x57, 7, 0x42, %0, %1, x1" : "=r"(r) : "r"(atm));
  return r;
}
static inline size_t vme_vsettk(size_t atk) {
  size_t r;
  asm volatile(".insn r 0x57, 7, 0x42, %0, %1, x2" : "=r"(r) : "r"(atk));
  return r;
}

// ---------------------------------------------------------------------------
// Matrix multiply: C[tm,tn] += A[tk,tm]^T * B[tk,tn], A rows in vs2, vs2+2, ...
// B rows in vs1, vs1+2, ... (e8, w4: KMAX = 4, LMUL = 1). vs mod 8 must be 0 or 1.

// sf.mm.u.u / s.u / u.s / s.s  (a = vs2 type, b = vs1 type)
#define VME_MM_U_U(t, vs2, vs1) asm volatile(".insn r 0x77, 0, 0x79, " VME_MT(t)  ", " VME_V(vs1) ", " VME_V(vs2))
#define VME_MM_S_U(t, vs2, vs1) asm volatile(".insn r 0x77, 0, 0x7b, " VME_MT(t)  ", " VME_V(vs1) ", " VME_V(vs2))
#define VME_MM_U_S(t, vs2, vs1) asm volatile(".insn r 0x77, 0, 0x79, " VME_MTB(t) ", " VME_V(vs1) ", " VME_V(vs2))
#define VME_MM_S_S(t, vs2, vs1) asm volatile(".insn r 0x77, 0, 0x7b, " VME_MTB(t) ", " VME_V(vs1) ", " VME_V(vs2))

// sf.mm.e5m2.e5m2 / e5m2.e4m3 / e4m3.e5m2 / e4m3.e4m3
#define VME_MM_E5_E5(t, vs2, vs1) asm volatile(".insn r 0x77, 1, 0x7d, " VME_MT(t)  ", " VME_V(vs1) ", " VME_V(vs2))
#define VME_MM_E5_E4(t, vs2, vs1) asm volatile(".insn r 0x77, 1, 0x7d, " VME_MTB(t) ", " VME_V(vs1) ", " VME_V(vs2))
#define VME_MM_E4_E5(t, vs2, vs1) asm volatile(".insn r 0x77, 1, 0x7f, " VME_MT(t)  ", " VME_V(vs1) ", " VME_V(vs2))
#define VME_MM_E4_E4(t, vs2, vs1) asm volatile(".insn r 0x77, 1, 0x7f, " VME_MTB(t) ", " VME_V(vs1) ", " VME_V(vs2))

// ---------------------------------------------------------------------------
// Tile state

// sf.vtzero.t mtd
#define VME_VTZERO(t) asm volatile(".insn r 0x57, 6, 0x21, " VME_MT(t) ", x0, x30")
// sf.vtdiscard
#define VME_VTDISCARD() asm volatile(".insn r 0x57, 6, 0x21, x0, x0, x28")
// sf.vtmv.v.t vd, rs1(TSS): tile row/column -> vector group (e32, w1: LMUL = 4)
#define VME_VTMV_V_T(vd, tss) asm volatile(".insn r 0x57, 6, 0x21, " VME_V(vd) ", %0, x31" : : "r"(tss))
// sf.vtmv.t.v rs1(TSS), vs2: vector group -> tile row/column
#define VME_VTMV_T_V(tss, vs2) asm volatile(".insn r 0x57, 6, 0x2f, x0, %0, " VME_V(vs2) : : "r"(tss))

// sf.vtle32 rs2(TSS), (rs1=addr): load a tile row/column from memory (e32, w1)
#define VME_VTLE32(addr, tss) asm volatile(".insn r 0x07, 7, 0x29, x0, %0, %1" : : "r"(addr), "r"(tss) : "memory")
// sf.vtse32 rs2(TSS), (rs1=addr): store a tile row/column to memory (e32, w1)
#define VME_VTSE32(addr, tss) asm volatile(".insn r 0x27, 7, 0x29, x0, %0, %1" : : "r"(addr), "r"(tss) : "memory")

#endif // VME_H
