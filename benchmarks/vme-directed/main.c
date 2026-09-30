// Directed tests for the Xsfmm v0.6.6 subset (RISC-V VME stand-in) on Saturn's OPU.
// Self-checking against scalar reference models; prints a checksum of all tile
// results so the same binary can be compared against SiFive's QEMU.

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "vme.h"

#define MAX_TE 64

static int failures = 0;
static uint32_t checksum = 0x811c9dc5u;

static void mix(uint32_t v) { checksum = (checksum ^ v) * 0x01000193u; }

#define CHECK(cond, ...) do { if (!(cond)) { failures++; printf("FAIL %s:%d: ", __FILE__, __LINE__); printf(__VA_ARGS__); printf("\n"); } } while (0)

static uint32_t lcg_state = 12345;
static uint32_t lcg(void) { lcg_state = lcg_state * 1664525u + 1013904223u; return lcg_state >> 8; }

static size_t read_vtype(void) { size_t v; asm volatile("csrr %0, vtype" : "=r"(v)); return v; }
static size_t read_vl(void) { size_t v; asm volatile("csrr %0, vl" : "=r"(v)); return v; }
static uint64_t read_cycle(void) { uint64_t v; asm volatile("rdcycle %0" : "=r"(v)); return v; }

// A rows (k = 0..3) go to v8, v10, v12, v14; B rows to v16, v18, v20, v22.
static int8_t a_mem[4][MAX_TE] __attribute__((aligned(64)));
static int8_t b_mem[4][MAX_TE] __attribute__((aligned(64)));
static int32_t c_mem[MAX_TE][MAX_TE] __attribute__((aligned(64)));
static int32_t c_ref[MAX_TE][MAX_TE];
static int32_t row_buf[MAX_TE] __attribute__((aligned(64)));
static int32_t mem_buf[MAX_TE] __attribute__((aligned(64)));

static void load_operands(size_t te) {
  asm volatile("vsetvli zero, %0, e8, m1, ta, ma" : : "r"(te));
  asm volatile("vle8.v v8,  (%0)" : : "r"(a_mem[0]) : "memory");
  asm volatile("vle8.v v10, (%0)" : : "r"(a_mem[1]) : "memory");
  asm volatile("vle8.v v12, (%0)" : : "r"(a_mem[2]) : "memory");
  asm volatile("vle8.v v14, (%0)" : : "r"(a_mem[3]) : "memory");
  asm volatile("vle8.v v16, (%0)" : : "r"(b_mem[0]) : "memory");
  asm volatile("vle8.v v18, (%0)" : : "r"(b_mem[1]) : "memory");
  asm volatile("vle8.v v20, (%0)" : : "r"(b_mem[2]) : "memory");
  asm volatile("vle8.v v22, (%0)" : : "r"(b_mem[3]) : "memory");
}

// Read tile t (rows 0..te-1) into c_mem using row moves (e32, w1).
static void store_tile_rows(size_t t, size_t te) {
  for (size_t r = 0; r < te; r++) {
    vme_vsettnt_e32w1(te);
    VME_VTMV_V_T(24, vme_tss(t, VME_TSS_ROW, r));
    asm volatile("vse32.v v24, (%0)" : : "r"(c_mem[r]) : "memory");
  }
}

// Read tile t into c_mem using column moves (transposing back into row-major).
static void store_tile_cols(size_t t, size_t te) {
  for (size_t c = 0; c < te; c++) {
    vme_vsettnt_e32w1(te);
    VME_VTMV_V_T(24, vme_tss(t, VME_TSS_COL, c));
    asm volatile("vse32.v v24, (%0)" : : "r"(row_buf) : "memory");
    for (size_t r = 0; r < te; r++) c_mem[r][c] = row_buf[r];
  }
}

// Write c_ref into tile t with row moves.
static void load_tile_rows(size_t t, size_t te) {
  vme_vsettnt_e32w1(te);
  for (size_t r = 0; r < te; r++) {
    asm volatile("vle32.v v24, (%0)" : : "r"(c_ref[r]) : "memory");
    VME_VTMV_T_V(vme_tss(t, VME_TSS_ROW, r), 24);
  }
}

static int32_t ext8(int8_t x, int is_signed) { return is_signed ? (int32_t)x : (int32_t)(uint8_t)x; }

static void ref_mm_i8(size_t tm, size_t tn, size_t tk, int sa, int sb) {
  for (size_t i = 0; i < tm; i++)
    for (size_t j = 0; j < tn; j++)
      for (size_t k = 0; k < tk; k++)
        c_ref[i][j] += ext8(a_mem[k][i], sa) * ext8(b_mem[k][j], sb);
}

static void compare_tile(const char* what, size_t tm, size_t tn) {
  for (size_t i = 0; i < tm; i++) {
    for (size_t j = 0; j < tn; j++) {
      mix((uint32_t)c_mem[i][j]);
      if (c_mem[i][j] != c_ref[i][j]) {
        CHECK(0, "%s: C[%lu][%lu] = 0x%x, expected 0x%x", what, i, j, c_mem[i][j], c_ref[i][j]);
        return;
      }
    }
  }
}

// ---------------------------------------------------------------------------

static size_t test_config(void) {
  size_t te = vme_vsettnt_e32w1((size_t)-1);
  printf("TE (tile edge) = %lu\n", te);
  CHECK(te >= 4 && te <= MAX_TE && (te & (te - 1)) == 0, "unexpected TE %lu", te);

  size_t vt = read_vtype();
  CHECK(((vt >> 9) & 3) == 1, "e32w1: vtwiden = %lu", (vt >> 9) & 3);
  CHECK(((vt >> 3) & 7) == 2, "e32w1: vsew = %lu", (vt >> 3) & 7);
  CHECK((vt & 7) == 2, "e32w1: LMUL field = %lu (expected m4)", vt & 7);
  CHECK(((vt >> 6) & 3) == 3, "e32w1: vta/vma must be 1");

  CHECK(vme_vsettk(7) == 1, "e32w1: KMAX must be 1");
  CHECK(vme_vsettm(1000) == te, "e32w1: tm clamps to TE");
  CHECK(((read_vtype() >> 16) & 0x3fff) == te, "tm field in vtype");

  size_t tn = vme_vsettnt_e8w4((size_t)-1);
  CHECK(tn == te, "e8w4: tn = %lu, expected TE", tn);
  vt = read_vtype();
  CHECK(((vt >> 9) & 3) == 3 && ((vt >> 3) & 7) == 0 && (vt & 7) == 0, "e8w4 vtype = 0x%lx", vt);
  CHECK(vme_vsettk(7) == 4, "e8w4: KMAX must be 4");
  CHECK(((read_vtype() >> 11) & 7) == 4, "tk field in vtype");
  CHECK(vme_vsettk(3) == 3, "e8w4: tk = 3");
  CHECK(vme_vsettn(5) == 5 && read_vl() == 5, "sf.vsettn sets vl");
  CHECK(vme_vsettm(te - 1) == te - 1, "sf.vsettm");

  // Unconfigured: sf.vsett* must set vill
  asm volatile("vsetvli zero, %0, e32, m1, ta, ma" : : "r"(te));
  vme_vsettm(4);
  CHECK((read_vtype() >> (__riscv_xlen - 1)) & 1, "sf.vsettm without matrix config must set vill");
  return te;
}

static void test_mm_int8(size_t te) {
  static const int sa_tab[4] = {0, 1, 0, 1};
  static const int sb_tab[4] = {0, 0, 1, 1};
  static const char* names[4] = {"mm.u.u", "mm.s.u", "mm.u.s", "mm.s.s"};
  size_t shapes[][3] = {{te, te, 4}, {te, te, 1}, {te - 1, te, 3}, {te, 1, 2}, {1, te - 3, 4}, {3, 5, 4}};
  for (int v = 0; v < 4; v++) {
    for (size_t s = 0; s < sizeof(shapes) / sizeof(shapes[0]); s++) {
      printf("mm_int8: v=%d s=%lu\n", v, s); // TEMP DIAG
      size_t tm = shapes[s][0], tn = shapes[s][1], tk = shapes[s][2];
      for (int k = 0; k < 4; k++)
        for (size_t i = 0; i < te; i++) {
          uint32_t r = lcg();
          // include the extremes
          a_mem[k][i] = (i == 0) ? (int8_t)0x80 : (i == 1) ? (int8_t)0x7f : (i == 2) ? (int8_t)0xff : (int8_t)r;
          b_mem[k][i] = (i == 0) ? (int8_t)0x80 : (i == 1) ? (int8_t)0xff : (int8_t)(r >> 8);
        }
      // start from a known, non-zero accumulator
      for (size_t i = 0; i < te; i++)
        for (size_t j = 0; j < te; j++)
          c_ref[i][j] = (int32_t)(lcg() & 0xffff) - 0x8000;
      load_tile_rows(1, te);
      load_operands(te);

      vme_vsettnt_e8w4(tn);
      vme_vsettm(tm);
      vme_vsettk(tk);
      switch (v) {
        case 0: VME_MM_U_U(1, 8, 16); break;
        case 1: VME_MM_S_U(1, 8, 16); break;
        case 2: VME_MM_U_S(1, 8, 16); break;
        default: VME_MM_S_S(1, 8, 16); break;
      }
      ref_mm_i8(tm, tn, tk, sa_tab[v], sb_tab[v]);
      store_tile_rows(1, te);
      compare_tile(names[v], tm, tn);
      // elements outside tm x tn are tail-agnostic; this implementation leaves them undisturbed
      for (size_t i = 0; i < te; i++)
        for (size_t j = 0; j < te; j++)
          if ((i >= tm || j >= tn) && c_mem[i][j] != c_ref[i][j])
            printf("note: %s tail element (%lu,%lu) changed\n", names[v], i, j);
    }
  }
}

static void test_moves(size_t te) {
  for (size_t i = 0; i < te; i++)
    for (size_t j = 0; j < te; j++)
      c_ref[i][j] = (int32_t)((i << 16) | j) ^ (int32_t)lcg();
  for (size_t t = 0; t < 4; t++) {
    printf("test_moves: t=%lu load_tile_rows\n", t); // TEMP DIAG
    load_tile_rows(t, te);
    printf("test_moves: t=%lu store_tile_cols\n", t); // TEMP DIAG
    store_tile_cols(t, te);
    printf("test_moves: t=%lu compare_tile\n", t); // TEMP DIAG
    compare_tile("row-in / column-out", te, te);
  }
  // column-in / row-out
  printf("test_moves: column-in/row-out loop\n"); // TEMP DIAG
  for (size_t c = 0; c < te; c++) {
    for (size_t r = 0; r < te; r++) row_buf[r] = c_ref[r][c];
    vme_vsettnt_e32w1(te);
    asm volatile("vle32.v v24, (%0)" : : "r"(row_buf) : "memory");
    VME_VTMV_T_V(vme_tss(2, VME_TSS_COL, c), 24);
  }
  printf("test_moves: column-in/row-out store_tile_rows\n"); // TEMP DIAG
  store_tile_rows(2, te);
  printf("test_moves: column-in/row-out compare_tile\n"); // TEMP DIAG
  compare_tile("column-in / row-out", te, te);
  printf("test_moves: partial move section\n"); // TEMP DIAG

  // partial-length move (vl < TE): elements [vl, TE) of the row are tail-agnostic
  size_t pl = te / 2 + 1;
  vme_vsettnt_e32w1(pl);
  asm volatile("vmv.v.i v24, 0");
  VME_VTMV_T_V(vme_tss(2, VME_TSS_ROW, 3), 24);
  for (size_t j = 0; j < pl; j++) c_ref[3][j] = 0;
  store_tile_rows(2, te);
  for (size_t j = 0; j < pl; j++)
    CHECK(c_mem[3][j] == 0, "partial row move: element %lu = 0x%x", j, c_mem[3][j]);
  for (size_t j = pl; j < te; j++)
    if (c_mem[3][j] != c_ref[3][j]) { printf("note: partial row move changed tail element %lu\n", j); break; }
  for (size_t r = 0; r < te; r++)
    if (r != 3)
      for (size_t j = 0; j < te; j++)
        CHECK(c_mem[r][j] == c_ref[r][j], "partial row move disturbed row %lu", r);
}

static void test_vtzero(size_t te) {
  for (size_t i = 0; i < te; i++)
    for (size_t j = 0; j < te; j++)
      c_ref[i][j] = (int32_t)lcg() | 1;
  printf("vtzero: load_tile_rows\n"); // TEMP DIAG
  load_tile_rows(3, te);
  printf("vtzero: after load_tile_rows c_ref[0][0]=0x%x\n", c_ref[0][0]); // TEMP DIAG
  size_t tm = te - 2, tn = 3;
  vme_vsettnt_e8w4(tn);
  vme_vsettm(tm);
  printf("vtzero: VME_VTZERO\n"); // TEMP DIAG
  VME_VTZERO(3);
  printf("vtzero: store_tile_rows\n"); // TEMP DIAG
  store_tile_rows(3, te);
  printf("vtzero: after store_tile_rows c_mem[0][0]=0x%x c_ref[0][0]=0x%x\n", c_mem[0][0], c_ref[0][0]); // TEMP DIAG
  for (size_t i = 0; i < tm; i++)
    for (size_t j = 0; j < tn; j++)
      c_ref[i][j] = 0;
  printf("vtzero: after zeroing c_ref c_ref[0][0]=0x%x c_mem[0][0]=0x%x\n", c_ref[0][0], c_mem[0][0]); // TEMP DIAG
  printf("vtzero: compare_tile\n"); // TEMP DIAG
  compare_tile("vtzero body", tm, tn);
  printf("vtzero: VTDISCARD\n"); // TEMP DIAG
  VME_VTDISCARD();
  printf("vtzero: done\n"); // TEMP DIAG
}

// sf.vtse32 / sf.vtle32: Load/Store Tile Subset to Memory (spec 1.1.6).
static void test_tile_mem(size_t te) {
  // Row round-trip: tile 1 row r -(vtse32)-> mem_buf -(vtle32)-> tile 2 row r,
  // read back through the existing (already-verified) sf.vtmv.v.t readout.
  for (size_t i = 0; i < te; i++)
    for (size_t j = 0; j < te; j++)
      c_ref[i][j] = (int32_t)((i << 16) | j) ^ (int32_t)lcg();
  printf("tile_mem: row round-trip load_tile_rows\n"); // TEMP DIAG
  load_tile_rows(1, te);
  vme_vsettnt_e32w1(te);
  printf("tile_mem: row round-trip loop\n"); // TEMP DIAG
  for (size_t r = 0; r < te; r++) {
    printf("  r=%lu vtse32 cycle=%llu\n", r, (unsigned long long)read_cycle()); // TEMP DIAG
    VME_VTSE32(mem_buf, vme_tss(1, VME_TSS_ROW, r));
    printf("  r=%lu vtse32 retired cycle=%llu\n", r, (unsigned long long)read_cycle()); // TEMP DIAG
    printf("  r=%lu vtle32\n", r); // TEMP DIAG
    VME_VTLE32(mem_buf, vme_tss(2, VME_TSS_ROW, r));
    printf("  r=%lu done\n", r); // TEMP DIAG
  }
  printf("tile_mem: row round-trip store_tile_rows\n"); // TEMP DIAG
  store_tile_rows(2, te);
  printf("tile_mem: row round-trip compare_tile\n"); // TEMP DIAG
  compare_tile("vtse32/vtle32 row round-trip", te, te);

  // Column round-trip: tile 0 column c -> mem_buf -> tile 3 column c.
  for (size_t i = 0; i < te; i++)
    for (size_t j = 0; j < te; j++)
      c_ref[i][j] = (int32_t)lcg();
  printf("tile_mem: column round-trip load_tile_rows\n"); // TEMP DIAG
  load_tile_rows(0, te);
  vme_vsettnt_e32w1(te);
  printf("tile_mem: column round-trip loop\n"); // TEMP DIAG
  for (size_t c = 0; c < te; c++) {
    VME_VTSE32(mem_buf, vme_tss(0, VME_TSS_COL, c));
    VME_VTLE32(mem_buf, vme_tss(3, VME_TSS_COL, c));
  }
  printf("tile_mem: column round-trip store_tile_cols\n"); // TEMP DIAG
  store_tile_cols(3, te);
  printf("tile_mem: column round-trip compare_tile\n"); // TEMP DIAG
  compare_tile("vtse32/vtle32 column round-trip", te, te);

  // sf.vtse32's bytes checked directly against the reference (not just round-tripped).
  for (size_t i = 0; i < te; i++)
    for (size_t j = 0; j < te; j++)
      c_ref[i][j] = (int32_t)lcg() ^ (int32_t)(i * te + j);
  printf("tile_mem: direct-bytes load_tile_rows\n"); // TEMP DIAG
  load_tile_rows(1, te);
  vme_vsettnt_e32w1(te);
  printf("tile_mem: direct-bytes loop\n"); // TEMP DIAG
  for (size_t r = 0; r < te; r++) {
    VME_VTSE32(mem_buf, vme_tss(1, VME_TSS_ROW, r));
    for (size_t j = 0; j < te; j++)
      mix((uint32_t)mem_buf[j]);
    for (size_t j = 0; j < te; j++)
      CHECK(mem_buf[j] == c_ref[r][j], "vtse32: mem[%lu][%lu] = 0x%x, expected 0x%x", r, j, mem_buf[j], c_ref[r][j]);
  }
  printf("tile_mem: direct-bytes done\n"); // TEMP DIAG

  // Partial-length store/load (vl < TE): elements [vl, TE) are tail-agnostic and must
  // not be touched in memory, mirroring test_moves' partial-length sf.vtmv.t.v case.
  printf("tile_mem: partial vtse32\n"); // TEMP DIAG
  size_t pl = te / 2 + 1;
  vme_vsettnt_e32w1(pl);
  for (size_t j = 0; j < te; j++) mem_buf[j] = (int32_t)0xdeadbeef;
  VME_VTSE32(mem_buf, vme_tss(1, VME_TSS_ROW, 2));
  printf("tile_mem: partial vtse32 done\n"); // TEMP DIAG
  for (size_t j = 0; j < pl; j++)
    CHECK(mem_buf[j] == c_ref[2][j], "partial vtse32: element %lu = 0x%x, expected 0x%x", j, mem_buf[j], c_ref[2][j]);
  for (size_t j = pl; j < te; j++)
    CHECK((uint32_t)mem_buf[j] == 0xdeadbeefu, "partial vtse32 touched element %lu beyond vl", j);

  // Partial-length load (vl < TE): elements [vl, TE) of the destination row are
  // tail-agnostic (same convention as sf.vtmv.t.v in test_moves) -- snapshot the row
  // before the partial load so the tail can be compared, not just assumed.
  printf("tile_mem: partial vtle32\n"); // TEMP DIAG
  int32_t tail_before[MAX_TE];
  vme_vsettnt_e32w1(te);
  VME_VTMV_V_T(24, vme_tss(2, VME_TSS_ROW, 2));
  asm volatile("vse32.v v24, (%0)" : : "r"(tail_before) : "memory");
  for (size_t j = 0; j < te; j++) mem_buf[j] = (int32_t)0xdeadbeef;
  vme_vsettnt_e32w1(pl);
  VME_VTLE32(mem_buf, vme_tss(2, VME_TSS_ROW, 2));
  vme_vsettnt_e32w1(te);
  VME_VTMV_V_T(24, vme_tss(2, VME_TSS_ROW, 2));
  asm volatile("vse32.v v24, (%0)" : : "r"(row_buf) : "memory");
  printf("tile_mem: partial vtle32 done\n"); // TEMP DIAG
  for (size_t j = 0; j < pl; j++)
    CHECK((uint32_t)row_buf[j] == 0xdeadbeefu, "partial vtle32: element %lu = 0x%x, expected sentinel", j, row_buf[j]);
  for (size_t j = pl; j < te; j++)
    if (row_buf[j] != tail_before[j]) printf("note: partial vtle32 changed tail element %lu\n", j);
  printf("tile_mem: done\n"); // TEMP DIAG
}

// OCP FP8 -> float
static inline float vme_pow2f(int e) {  // 2^e for -126 <= e <= 127
  union { uint32_t u; float f; } v; v.u = (uint32_t)(e + 127) << 23; return v.f;
}
static float fp8_to_float(uint8_t x, int e5m2) {
  int sign = x >> 7;
  int ebits = e5m2 ? 5 : 4, mbits = e5m2 ? 2 : 3, bias = e5m2 ? 15 : 7;
  int e = (x >> mbits) & ((1 << ebits) - 1);
  int m = x & ((1 << mbits) - 1);
  float v;
  if (e5m2 && e == 31) v = m ? (0.0f/0.0f) : (1.0f/0.0f);
  else if (!e5m2 && e == 15 && m == 7) v = (0.0f/0.0f);
  else if (e == 0) v = (float)m * vme_pow2f(1 - bias - mbits);
  else v = (float)(m | (1 << mbits)) * vme_pow2f(e - bias - mbits);
  return sign ? -v : v;
}

static void test_mm_fp8(size_t te) {
  static float cf_ref[MAX_TE][MAX_TE];
  const int fmt_a[4] = {1, 1, 0, 0}; // 1 = e4m3
  const int fmt_b[4] = {1, 0, 1, 0};
  const char* names[4] = {"mm.e4m3.e4m3", "mm.e4m3.e5m2", "mm.e5m2.e4m3", "mm.e5m2.e5m2"};
  for (int v = 0; v < 4; v++) {
    size_t tm = te, tn = te - 1, tk = 4;
    for (int k = 0; k < 4; k++)
      for (size_t i = 0; i < te; i++) {
        // finite, moderate magnitudes: clear the top exponent bit
        a_mem[k][i] = (int8_t)(lcg() & 0xbf);
        b_mem[k][i] = (int8_t)(lcg() & 0xbf);
      }
    for (size_t i = 0; i < te; i++)
      for (size_t j = 0; j < te; j++) {
        cf_ref[i][j] = (float)((int)(lcg() & 0xff) - 128) / 16.0f;
        memcpy(&c_ref[i][j], &cf_ref[i][j], 4);
      }
    load_tile_rows(0, te);
    load_operands(te);
    vme_vsettnt_e8w4(tn);
    vme_vsettm(tm);
    vme_vsettk(tk);
    switch (v) {
      case 0: VME_MM_E4_E4(0, 8, 16); break;
      case 1: VME_MM_E4_E5(0, 8, 16); break;
      case 2: VME_MM_E5_E4(0, 8, 16); break;
      default: VME_MM_E5_E5(0, 8, 16); break;
    }
    // Reference: exact FP8 products, accumulated in FP32 one k at a time (phase-1 hardware)
    for (size_t i = 0; i < tm; i++)
      for (size_t j = 0; j < tn; j++) {
        volatile float acc = cf_ref[i][j];
        for (size_t k = 0; k < tk; k++)
          acc = acc + fp8_to_float((uint8_t)a_mem[k][i], !fmt_a[v]) * fp8_to_float((uint8_t)b_mem[k][j], !fmt_b[v]);
        cf_ref[i][j] = acc;
      }
    store_tile_rows(0, te);
    int exact = 0, total = 0;
    for (size_t i = 0; i < tm; i++)
      for (size_t j = 0; j < tn; j++) {
        float got;
        memcpy(&got, &c_mem[i][j], 4);
        mix((uint32_t)c_mem[i][j]);
        float err = got - cf_ref[i][j];
        if (err < 0) err = -err;
        float mag = cf_ref[i][j] < 0 ? -cf_ref[i][j] : cf_ref[i][j];
        total++;
        if (got == cf_ref[i][j]) exact++;
        else if (err > 1e-5f * mag + 1e-30f) {
          uint32_t want; memcpy(&want, &cf_ref[i][j], 4);
          CHECK(0, "%s: C[%lu][%lu] = %08x, expected %08x", names[v], i, j, c_mem[i][j], want);
          i = tm; break;
        }
      }
    printf("%s: %d/%d bit-exact vs sequential-FP32 model\n", names[v], exact, total);
  }
}

static void diag_moves(size_t te) {
  for (size_t i = 0; i < te; i++)
    for (size_t j = 0; j < te; j++)
      c_ref[i][j] = (int32_t)((i << 16) | j);
  printf("diag: load_tile_rows(0)\n");
  load_tile_rows(0, te);
  printf("diag: store_tile_rows(0) [mvout tile0 row]\n");
  store_tile_rows(0, te);
  compare_tile("diag row0", te, te);
  printf("diag: store_tile_cols(0) [mvout tile0 col]\n");
  store_tile_cols(0, te);
  compare_tile("diag col0", te, te);
  printf("diag: load_tile_rows(1)\n");
  load_tile_rows(1, te);
  printf("diag: store_tile_cols(1) [mvout tile1 col]\n");
  store_tile_cols(1, te);
  compare_tile("diag col1", te, te);
  printf("diag: done\n");
}

int main(void) {
  size_t te = test_config();
  if (te > MAX_TE) { printf("TE too large\n"); exit(1); }
  #ifndef VME_NO_FP8
  test_mm_fp8(te);
  #endif
  test_moves(te);
  test_tile_mem(te);
  diag_moves(te);
  test_mm_int8(te);
  test_vtzero(te);
  printf("checksum 0x%08x\n", checksum);
  if (failures) {
    printf("FAILED (%d)\n", failures);
    exit(1);
  }
  printf("PASSED\n");
  return 0;
}
