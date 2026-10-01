// Exhaustive check of p3109.h against a direct transcription of the standard.
//
// p3109.h computes everything with bit tricks on code points. This file does
// the same work the slow, obvious way: decode each code point to a real value
// exactly as 4.7.2 says, then apply the operation exactly as its behavior table
// in 4.11 / 4.12 / 4.13 says, then compare.
//
// The formats are 8 bits wide (or 4), so every input is checked: 256 cases for
// a one-operand operation, 65536 for a two-operand one.
//
// Host-side only (benchmarks/common/*.c is compiled into every benchmark):
//   cc -O2 -Wall -o /tmp/p3109_test p3109_test.c && /tmp/p3109_test

#include <stdio.h>
#include <stdlib.h>
#include "../benchmarks/common/p3109.h"

// -----------------------------------------------------------------------------
// The reference: a closed extended real, as 3.1 defines it
// -----------------------------------------------------------------------------

typedef enum { R_NUM, R_PINF, R_NINF, R_NAN } rkind_t;
typedef struct { rkind_t kind; double v; } real_t;

static real_t r_num(double v) { real_t r = {R_NUM, v}; return r; }
static real_t r_nan(void)     { real_t r = {R_NAN, 0}; return r; }
static real_t r_pinf(void)    { real_t r = {R_PINF, 0}; return r; }
static real_t r_ninf(void)    { real_t r = {R_NINF, 0}; return r; }

static double ipow2(int e) {
  double r = 1.0;
  for (; e > 0; e--) r *= 2.0;
  for (; e < 0; e++) r *= 0.5;
  return r;
}

// 4.7.2 ωDecode, transcribed pattern by pattern, in the order given.
static real_t ref_decode(p3109_fmt_t f, p3109_t x) {
  uint32_t half = 1u << (f.k - 1), full = 1u << f.k;
  int bias = p3109_exponent_bias_of(f);

  if (f.is_signed && x == half) return r_nan();
  if (!f.is_signed && x == full - 1) return r_nan();
  if (f.is_signed && f.is_extended && x == half - 1) return r_pinf();
  if (f.is_signed && f.is_extended && x == full - 1) return r_ninf();
  if (!f.is_signed && f.is_extended && x == full - 2) return r_pinf();
  if (f.is_signed && x > half && x < full) {
    real_t n = ref_decode(f, x - half);
    if (n.kind == R_NUM) return r_num(-n.v);
    return n.kind == R_PINF ? r_ninf() : (n.kind == R_NINF ? r_pinf() : n);
  }

  uint32_t t = x % (1u << (f.p - 1));
  uint32_t e = x / (1u << (f.p - 1));
  double frac = (double)t * ipow2(1 - f.p);
  if (e == 0) return r_num(frac * ipow2(1 - bias));
  return r_num((1.0 + frac) * ipow2((int)e - bias));
}

static int r_is_nan(real_t a)  { return a.kind == R_NAN; }
static int r_is_inf(real_t a)  { return a.kind == R_PINF || a.kind == R_NINF; }
static int r_sign_minus(real_t a) {
  return a.kind == R_NINF || (a.kind == R_NUM && a.v < 0);
}

// Order over the closed extended reals, NaN excluded.
static int r_lt(real_t a, real_t b) {
  if (a.kind == R_NINF) return b.kind != R_NINF;
  if (b.kind == R_NINF) return 0;
  if (b.kind == R_PINF) return a.kind != R_PINF;
  if (a.kind == R_PINF) return 0;
  return a.v < b.v;
}
static int r_eq(real_t a, real_t b) {
  if (a.kind != b.kind) return 0;
  return a.kind == R_NUM ? a.v == b.v : 1;
}
static double r_abs(real_t a) { return a.v < 0 ? -a.v : a.v; }

// -----------------------------------------------------------------------------
// Harness
// -----------------------------------------------------------------------------

static int failures = 0;

static void fail(const char *op, p3109_fmt_t f, long x, long y, long got,
                 long want) {
  if (failures < 10)
    fprintf(stderr, "  %-28s K=%d P=%d %s%s  x=%ld y=%ld -> %ld, want %ld\n", op,
            f.k, f.p, f.is_signed ? "s" : "u", f.is_extended ? "e" : "f", x, y,
            got, want);
  failures++;
}

#define CHECK1(op, got, want) \
  if ((long)(got) != (long)(want)) fail(op, f, x, -1, (long)(got), (long)(want));
#define CHECK2(op, got, want) \
  if ((long)(got) != (long)(want)) fail(op, f, x, y, (long)(got), (long)(want));

// -----------------------------------------------------------------------------
// One-operand operations
// -----------------------------------------------------------------------------

static void check_unary(p3109_fmt_t f) {
  uint32_t n = 1u << f.k;
  for (uint32_t x = 0; x < n; x++) {
    real_t X = ref_decode(f, x);

    // 4.13 predicates
    CHECK1("IsNaN", p3109_is_nan(f, x), r_is_nan(X));
    CHECK1("IsInfinite", p3109_is_infinite(f, x), r_is_inf(X));
    CHECK1("IsFinite", p3109_is_finite(f, x), !r_is_nan(X) && !r_is_inf(X));
    CHECK1("IsZero", p3109_is_zero(f, x), X.kind == R_NUM && X.v == 0.0);
    CHECK1("IsOne", p3109_is_one(f, x), X.kind == R_NUM && X.v == 1.0);
    CHECK1("IsSignMinus", p3109_is_sign_minus(f, x), r_sign_minus(X));

    // 4.13: normal iff |X| >= MinNormalOf(f); subnormal is the rest.
    real_t min_norm = ref_decode(f, p3109_min_normal_of(f));
    int is_num = X.kind == R_NUM && X.v != 0.0;
    int want_normal = is_num && r_abs(X) >= min_norm.v;
    CHECK1("IsNormal", p3109_is_normal(f, x), want_normal);
    CHECK1("IsSubnormal", p3109_is_subnormal(f, x), is_num && !want_normal);

    // 4.10.1 Negate, Abs -- checked by decoding the result
    real_t N = ref_decode(f, p3109_negate(f, x));
    if (r_is_nan(X)) {
      CHECK1("Negate(NaN)", r_is_nan(N), 1);
    } else if (!f.is_signed && !(X.kind == R_NUM && X.v == 0.0)) {
      // -X is out of range for an unsigned format, so 4.7.5 gives NaN.
      CHECK1("Negate(unsigned)", r_is_nan(N), 1);
    } else if (X.kind == R_NUM) {
      CHECK1("Negate", r_eq(N, r_num(-X.v)), 1);
    } else {
      CHECK1("Negate", N.kind == (X.kind == R_PINF ? R_NINF : R_PINF), 1);
    }

    real_t A = ref_decode(f, p3109_abs(f, x));
    if (r_is_nan(X)) {
      CHECK1("Abs(NaN)", r_is_nan(A), 1);
    } else if (X.kind == R_NUM) {
      CHECK1("Abs", r_eq(A, r_num(r_abs(X))), 1);
    } else {
      CHECK1("Abs", A.kind == R_PINF, 1);
    }
  }
}

// -----------------------------------------------------------------------------
// 4.16  NextGreaterThan / NextLessThan
// -----------------------------------------------------------------------------
// Reference: the next code point up (or down) in *value* order, or NaN when
// there is none. Found by scanning, which is slow and obviously correct.

static void check_next(p3109_fmt_t f) {
  uint32_t n = 1u << f.k;
  for (uint32_t x = 0; x < n; x++) {
    real_t X = ref_decode(f, x);

    uint32_t want_up = p3109_nan_of(f), want_dn = p3109_nan_of(f);
    if (!r_is_nan(X)) {
      int have_up = 0, have_dn = 0;
      real_t best_up = r_nan(), best_dn = r_nan();
      for (uint32_t y = 0; y < n; y++) {
        real_t Y = ref_decode(f, y);
        if (r_is_nan(Y)) continue;
        if (r_lt(X, Y) && (!have_up || r_lt(Y, best_up))) {
          best_up = Y; want_up = y; have_up = 1;
        }
        if (r_lt(Y, X) && (!have_dn || r_lt(best_dn, Y))) {
          best_dn = Y; want_dn = y; have_dn = 1;
        }
      }
    }
    CHECK1("NextGreaterThan", p3109_next_greater_than(f, x), want_up);
    CHECK1("NextLessThan", p3109_next_less_than(f, x), want_dn);
  }
}

// -----------------------------------------------------------------------------
// Two-operand operations
// -----------------------------------------------------------------------------

static void check_binary(p3109_fmt_t f) {
  uint32_t n = 1u << f.k;
  for (uint32_t x = 0; x < n; x++) {
    for (uint32_t y = 0; y < n; y++) {
      real_t X = ref_decode(f, x), Y = ref_decode(f, y);
      int nx = r_is_nan(X), ny = r_is_nan(Y), any_nan = nx || ny;

      // 4.12 comparisons: NaN makes every one of them false
      CHECK2("CompareLess", p3109_compare_less(f, x, y),
             !any_nan && r_lt(X, Y));
      CHECK2("CompareLessEqual", p3109_compare_less_equal(f, x, y),
             !any_nan && (r_lt(X, Y) || r_eq(X, Y)));
      CHECK2("CompareEqual", p3109_compare_equal(f, x, y),
             !any_nan && r_eq(X, Y));
      CHECK2("CompareGreater", p3109_compare_greater(f, x, y),
             !any_nan && r_lt(Y, X));
      CHECK2("CompareGreaterEqual", p3109_compare_greater_equal(f, x, y),
             !any_nan && (r_lt(Y, X) || r_eq(X, Y)));

      // 4.12.1 total order
      CHECK2("TotalOrder", p3109_total_order(f, x, y),
             nx ? 1 : (ny ? 0 : (r_lt(X, Y) || r_eq(X, Y))));

      // 4.11.1 minimum / maximum, and the Number variants
      uint32_t nan = p3109_nan_of(f);
      uint32_t w_min = any_nan ? nan : (r_lt(X, Y) ? x : y);
      uint32_t w_max = any_nan ? nan : (r_lt(X, Y) ? y : x);
      CHECK2("Minimum", p3109_minimum(f, x, y), w_min);
      CHECK2("Maximum", p3109_maximum(f, x, y), w_max);

      uint32_t w_minn = (nx && ny) ? nan : (nx ? y : (ny ? x : w_min));
      uint32_t w_maxn = (nx && ny) ? nan : (nx ? y : (ny ? x : w_max));
      CHECK2("MinimumNumber", p3109_minimum_number(f, x, y), w_minn);
      CHECK2("MaximumNumber", p3109_maximum_number(f, x, y), w_maxn);

      // 4.11.2 magnitude variants: compare |X| with |Y|, ties broken by value
      uint32_t w_minm = nan, w_maxm = nan;
      if (!any_nan) {
        int ix = r_is_inf(X), iy = r_is_inf(Y);
        double ax = ix ? 0 : r_abs(X), ay = iy ? 0 : r_abs(Y);
        int lt_mag, gt_mag;
        if (ix || iy) {           // infinity has the larger magnitude
          lt_mag = !ix && iy;
          gt_mag = ix && !iy;
        } else {
          lt_mag = ax < ay;
          gt_mag = ax > ay;
        }
        w_minm = lt_mag ? x : (gt_mag ? y : w_min);
        w_maxm = gt_mag ? x : (lt_mag ? y : w_max);
      }
      CHECK2("MinimumMagnitude", p3109_minimum_magnitude(f, x, y), w_minm);
      CHECK2("MaximumMagnitude", p3109_maximum_magnitude(f, x, y), w_maxm);

      CHECK2("MinimumMagnitudeNumber", p3109_minimum_magnitude_number(f, x, y),
             ny ? x : (nx ? y : w_minm));
      CHECK2("MaximumMagnitudeNumber", p3109_maximum_magnitude_number(f, x, y),
             ny ? x : (nx ? y : w_maxm));

      // 4.11.3 finite variants: skip infinities the way Number skips NaN
      uint32_t w_minf, w_maxf;
      if (nx && ny) { w_minf = w_maxf = nan; }
      else if (nx)  { w_minf = w_maxf = y; }
      else if (ny)  { w_minf = w_maxf = x; }
      else {
        int ix = r_is_inf(X), iy = r_is_inf(Y);
        if (ix && iy)  { w_minf = w_min; w_maxf = w_max; }
        else if (ix)   { w_minf = w_maxf = y; }
        else if (iy)   { w_minf = w_maxf = x; }
        else           { w_minf = w_min; w_maxf = w_max; }
      }
      CHECK2("MinimumFinite", p3109_minimum_finite(f, x, y), w_minf);
      CHECK2("MaximumFinite", p3109_maximum_finite(f, x, y), w_maxf);
    }
  }
}

// -----------------------------------------------------------------------------
// 4.14  Format-level operations: check the returned code points decode to the
// values their names promise.
// -----------------------------------------------------------------------------

static void check_format_level(p3109_fmt_t f) {
  uint32_t n = 1u << f.k;
  uint32_t x = 0, y = 0;  // for the CHECK macros

  real_t maxf = ref_decode(f, p3109_max_finite_of(f));
  real_t minf = ref_decode(f, p3109_min_finite_of(f));
  real_t minp = ref_decode(f, p3109_min_positive_of(f));
  real_t minn = ref_decode(f, p3109_min_normal_of(f));

  CHECK2("MaxFiniteOf is finite", maxf.kind == R_NUM, 1);
  CHECK2("MinFiniteOf is finite", minf.kind == R_NUM, 1);
  CHECK2("MinPositiveOf > 0", minp.kind == R_NUM && minp.v > 0, 1);

  // No finite value exceeds MaxFiniteOf, and none is below MinFiniteOf.
  for (uint32_t c = 0; c < n; c++) {
    real_t C = ref_decode(f, c);
    if (C.kind != R_NUM) continue;
    CHECK2("MaxFiniteOf is the largest", C.v <= maxf.v, 1);
    CHECK2("MinFiniteOf is the smallest", C.v >= minf.v, 1);
    if (C.v > 0) CHECK2("MinPositiveOf is the smallest positive", C.v >= minp.v, 1);
  }

  CHECK2("MinNormalOf is normal", p3109_is_normal(f, p3109_min_normal_of(f)), 1);
  if (f.p > 1) {
    uint32_t ms = p3109_max_subnormal_of(f);
    CHECK2("MaxSubnormalOf is subnormal", p3109_is_subnormal(f, ms), 1);
    CHECK2("MaxSubnormalOf is just below MinNormalOf",
           ref_decode(f, ms).v < minn.v, 1);
  } else {
    CHECK2("MaxSubnormalOf is NaN when P=1",
           p3109_is_nan(f, p3109_max_subnormal_of(f)), 1);
  }
  (void)y;
}

// -----------------------------------------------------------------------------

int main(void) {
  struct { const char *name; p3109_fmt_t f; } formats[] = {
    {"Binary8p4se", P3109_BINARY8P4SE},
    {"Binary8p3se", P3109_BINARY8P3SE},
    {"Binary8p4sf", P3109_BINARY8P4SF},
    {"Binary8p3sf", P3109_BINARY8P3SF},
    {"Binary4p2sf", P3109_BINARY4P2SF},
    {"Binary8p1uf", P3109_BINARY8P1UF},
  };
  int nf = (int)(sizeof(formats) / sizeof(formats[0]));

  for (int i = 0; i < nf; i++) {
    int before = failures;
    p3109_fmt_t f = formats[i].f;
    uint32_t n = 1u << f.k;
    check_format_level(f);
    check_unary(f);
    check_next(f);
    check_binary(f);
    printf("%-12s  %6u unary, %8u binary cases ... %s\n", formats[i].name, n,
           n * n, failures == before ? "ok" : "FAILED");
  }

  if (failures) {
    printf("\n%d mismatches\n", failures);
    return 1;
  }
  printf("\nall formats exhaustively verified\n");
  return 0;
}
