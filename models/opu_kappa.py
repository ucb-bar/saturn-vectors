"""Accuracy of OPFMACC.S as an implementation of P3109 BlockDotProduct.

    opu_kappa.py [--check]

BlockDotProduct (P3109 Interim Report v4.0.3, 5.6.2) sums a block's scaled
products exactly and rounds once. B OPFMACC.S on a cleared tile round into
the binary32 accumulator after every product. This compares the two, as
kappa (4.4): the number of binary32 values between the OPU's result and the
defined one.

The OPU model is the one opu-scaled-gemm's reference uses (exact product,
one rounding per step), which the RTL matches bit for bit.

--check compares round_fp32 with fma_ref's projection first.
"""
import os
import random
import struct
import sys
from fractions import Fraction
from multiprocessing import Pool

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "benchmarks", "common-data-gen"))

from gfloat_ref import FP8_STANDARDS  # noqa: E402
from fma_ref import exact  # noqa: E402

INF = float("inf")
EMAX, EMIN, P = 127, -126, 24
OVERFLOW = Fraction(2) ** 128 * (1 - Fraction(1, 2 ** 25))  # RNE rounds |x| >= this to Inf


def _rne(y):
    """Round a non-negative Fraction to an integer, ties to even."""
    f = y.numerator // y.denominator
    r = y - f
    return f + 1 if r > Fraction(1, 2) or (r == Fraction(1, 2) and f & 1) else f


def round_fp32(x):
    """Round an exact value (Fraction, or +-Inf) to binary32, RNE, SatNone."""
    if isinstance(x, float):
        return x
    if x == 0:
        return Fraction(0)
    s = -1 if x < 0 else 1
    a = abs(x)
    if a >= OVERFLOW:
        return s * INF
    e = a.numerator.bit_length() - a.denominator.bit_length()
    if Fraction(2) ** e > a:
        e -= 1
    q = (P - 1) - max(e, EMIN)                     # subnormals share EMIN's quantum
    m = _rne(a * Fraction(2) ** q)
    return s * Fraction(m) / Fraction(2) ** q


def key(v):
    """Position of a finite binary32 value in value order (+0 and -0 are one value)."""
    bits = struct.unpack("<I", struct.pack("<f", float(v)))[0]
    return bits if bits < 0x80000000 else -(bits & 0x7FFFFFFF)


def opu(products):
    """B OPFMACC.S on a cleared tile: round after every product."""
    c = Fraction(0)
    for p in products:
        if isinstance(c, float) or isinstance(p, float):
            cf, pf = float(c), float(p)
            c = cf + pf                             # Inf arithmetic; may give NaN
            if c == c and abs(c) != INF:
                c = round_fp32(Fraction(c))
        else:
            c = round_fp32(c + p)
    return c


def defined(products):
    """BlockDotProduct: exact sum, one rounding."""
    if any(isinstance(p, float) for p in products):
        return round_fp32(sum(float(p) for p in products))
    return round_fp32(sum(products))


def kappa(a, d):
    if a != a or d != d:
        return None if (a != a) == (d != d) else "nan-mismatch"
    if isinstance(a, float) or isinstance(d, float):
        return None if a == d else "inf-mismatch"
    return abs(key(a) - key(d))


def scale(code):
    return Fraction(0) if code == 0 else Fraction(2) ** (code - 128)


def run(args):
    """kappa over random blocks: (scenario, format, B, n, seed) -> list of kappa."""
    scen, std, altfmt, b, n, seed = args
    fi = FP8_STANDARDS[std][f"altfmt{altfmt}"]
    rng = random.Random(seed)
    finite = [c for c in range(256) if exact(fi, c)[0] == "num" and exact(fi, c)[2] != 0]
    pos = [c for c in finite if exact(fi, c)[1] == 0]
    out = []
    for _ in range(n):
        sx, sy = rng.randint(100, 156), rng.randint(100, 156)
        pool = pos if scen == "same-sign" else finite
        xs = [rng.choice(pool) for _ in range(b)]
        ys = [rng.choice(pool) for _ in range(b)]
        ps = []
        for x, y in zip(xs, ys):
            vx, vy = exact(fi, x), exact(fi, y)
            v = scale(sx) * scale(sy) * vx[2] * vy[2]
            ps.append(-v if vx[1] ^ vy[1] else v)
        out.append(kappa(opu(ps), defined(ps)))
    return scen, std, altfmt, b, out


def absorption(b):
    """A large first product, then B - 1 products of just under half its ulp."""
    big = Fraction(2) ** 8
    small = Fraction(15, 16) * Fraction(2) ** (8 - 24)   # 1.111b x 2^-17: binary8p4 x binary8p4
    ps = [big] + [small] * (b - 1)
    return kappa(opu(ps), defined(ps)), ps


def cancellation(b):
    """+L, B - 2 absorbed small products, -L: the exact sum is the small ones."""
    big = Fraction(2) ** 8
    small = Fraction(15, 16) * Fraction(2) ** (8 - 24)
    ps = [big] + [small] * (b - 2) + [-big]
    return kappa(opu(ps), defined(ps)), ps


def overflow_cases():
    """Partial sums overflow although the defined result does not, or is -Inf."""
    huge = Fraction(2) ** 127 * Fraction(3, 2)
    inf_case = [huge, huge, -huge]                     # defined finite, OPU +Inf
    nan_case = [huge, huge, -INF]                      # defined -Inf, OPU NaN
    return ((opu(inf_case), defined(inf_case), kappa(opu(inf_case), defined(inf_case))),
            (opu(nan_case), defined(nan_case), kappa(opu(nan_case), defined(nan_case))))


def check_round():
    from gfloat.formats import format_info_binary32 as FP32
    from gfloat import RoundMode
    from fma_ref import project
    rng = random.Random(1)
    for _ in range(200000):
        e = rng.randint(-160, 140)
        x = Fraction(rng.getrandbits(40) + 1) * Fraction(2) ** (e - 40)
        if rng.getrandbits(1):
            x = -x
        r = round_fp32(x)
        want = project(FP32, ("num", int(x < 0), abs(x)), RoundMode.TiesToEven)
        got = struct.unpack("<I", struct.pack("<f", float(r)))[0]
        if got != want and not (got & 0x7FFFFFFF == 0 and want & 0x7FFFFFFF == 0):
            sys.exit(f"round_fp32 mismatch at {x}: got {got:08x}, want {want:08x}")
    print("round_fp32 agrees with fma_ref.project on 200,000 values")


if __name__ == "__main__":
    if "--check" in sys.argv:
        check_round()
    jobs = []
    for scen in ("same-sign", "mixed-sign"):
        for altfmt in (0, 1):
            for b in (16, 32, 128):
                for part in range(4):
                    seed = (scen == "mixed-sign") << 12 | altfmt << 10 | b << 2 | part
                    jobs.append((scen, "p3109", altfmt, b, 2500 if b < 128 else 625, seed))
    agg = {}
    with Pool() as pool:
        for scen, std, altfmt, b, ks in pool.map(run, jobs):
            agg.setdefault((scen, altfmt, b), []).extend(ks)
    print(f"{'scenario':12} {'fmt':4} {'B':>4} {'blocks':>7} {'max k':>6} {'mean k':>7} {'k=0':>6} {'bound 2B':>9}")
    for (scen, altfmt, b), ks in sorted(agg.items()):
        nums = [k for k in ks if isinstance(k, int)]
        bad = [k for k in ks if not isinstance(k, int) and k is not None]
        print(f"{scen:12} p{4 - altfmt:<3} {b:>4} {len(ks):>7} {max(nums):>6} {sum(nums) / len(nums):>7.3f} "
              f"{sum(k == 0 for k in nums) / len(nums):>6.1%} {2 * b:>9}" + (f"  mismatches {len(bad)}" if bad else ""))
    for b in (16, 32, 128):
        print(f"absorption   B={b:<4} kappa {absorption(b)[0]}  (bound {2 * b})")
        print(f"cancellation B={b:<4} kappa {cancellation(b)[0]}")
    (ai, di, ki), (an, dn, kn) = overflow_cases()
    print(f"overflow: OPU {ai}, defined {float(di):.3e} -> {ki}")
    print(f"overflow then -Inf: OPU {an}, defined {dn} -> {kn}")
