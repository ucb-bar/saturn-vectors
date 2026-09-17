"""Reference model for the FMA unit's arithmetic on 8- and 16-bit floats.

Used to generate test vectors for vec-mx-binary.  It replaces mx_data_gen.c, which
computed its answers on Spike, and which could only reach FP8 arithmetic by
converting to BF16, computing there and converting back: for add and subtract with
an 8-bit result that rounds twice.  This model rounds once, as the hardware does.

How it works
------------
gfloat defines the formats and knows how to round into them, but it has no
arithmetic operations, and round_float() only accepts a Python float.  The exact
result of an operation can need more than a float's 53 bits.  So:

  1. operands are decoded exactly (8/16-bit values are always exact floats),
  2. the operation is carried out exactly, on fractions,
  3. the exact result is replaced by a float "stand-in" that sits in the same place
     relative to the destination format's two neighbouring numbers: on one of them,
     below the halfway point, on it, or above it.  Rounding depends only on that
     position, so gfloat rounds the stand-in to exactly the answer the exact result
     would get.  The stand-in is always representable as a float.

Special cases follow IEEE 754: NaN in gives NaN out; +Inf + -Inf, Inf - Inf and
0 x Inf give NaN; otherwise an infinity wins.  An exact zero sum of
opposite-signed operands is +0, or -0 when rounding toward negative.

Validated against the Spike-generated vec-mx-binary golden data by
vec-mx-binary/gen_data/validate_fma_ref.py.
"""

import math
from fractions import Fraction

from gfloat import RoundMode, decode_float, encode_float, round_float

from gfloat_ref import canonical_nan

# An exact value: ("nan",) | ("inf", sign) | ("num", sign, magnitude as a Fraction)


def exact(fi, bits):
    """Decode a bit pattern in format fi exactly."""
    v = decode_float(fi, bits).fval
    if math.isnan(v):
        return ("nan",)
    sign = 1 if math.copysign(1.0, v) < 0 else 0
    if math.isinf(v):
        return ("inf", sign)
    return ("num", sign, Fraction(abs(v)))


def _negate(x):
    return x if x[0] == "nan" else (x[0], 1 - x[1]) + x[2:]


def _is_zero(x):
    return x[0] == "num" and x[2] == 0


def mul(x, y, rnd):
    if "nan" in (x[0], y[0]):
        return ("nan",)
    sign = x[1] ^ y[1]
    if (x[0] == "inf" and _is_zero(y)) or (y[0] == "inf" and _is_zero(x)):
        return ("nan",)                                   # 0 x Inf
    if "inf" in (x[0], y[0]):
        return ("inf", sign)
    return ("num", sign, x[2] * y[2])


def add(x, y, rnd):
    if "nan" in (x[0], y[0]):
        return ("nan",)
    if x[0] == "inf" and y[0] == "inf":
        return ("nan",) if x[1] != y[1] else x            # +Inf + -Inf
    if x[0] == "inf":
        return x
    if y[0] == "inf":
        return y
    v = (-x[2] if x[1] else x[2]) + (-y[2] if y[1] else y[2])
    if v != 0:
        return ("num", int(v < 0), abs(v))
    if _is_zero(x) and _is_zero(y) and x[1] == y[1]:
        return ("num", x[1], Fraction(0))                 # (-0) + (-0) = -0
    return ("num", int(rnd == RoundMode.TowardNegative), Fraction(0))


def sub(x, y, rnd):
    return add(x, _negate(y), rnd)


OPS = {"mul": mul, "add": add, "sub": sub}


def _floor_log2(a):
    k = a.numerator.bit_length() - a.denominator.bit_length()
    while Fraction(2) ** k > a:
        k -= 1
    while Fraction(2) ** (k + 1) <= a:
        k += 1
    return k


def _stand_in(fi, a):
    """A float in the same position as `a` (> 0) relative to fi's neighbouring grid points."""
    emin = round(math.log2(fi.smallest_normal))
    ulp = Fraction(2) ** (max(_floor_log2(a), emin) - (fi.precision - 1))
    n = a / ulp
    lo = (n.numerator // n.denominator) * ulp
    if a == lo:
        return lo
    mid = lo + ulp / 2
    return lo + ulp / 4 if a < mid else (mid if a == mid else lo + 3 * ulp / 4)


def project(fi, r, rnd, sat=False, check=False):
    """Round an exact result once into format fi and encode it.

    check=True also rounds the exact value directly whenever it is itself a float,
    and raises if that disagrees with the stand-in.
    """
    if r[0] == "nan":
        return canonical_nan(fi)
    if r[0] == "inf":
        v = round_float(fi, -math.inf if r[1] else math.inf, rnd=rnd, sat=sat)
    elif r[2] == 0:
        v = round_float(fi, -0.0 if r[1] else 0.0, rnd=rnd, sat=sat)
    else:
        s = _stand_in(fi, r[2])
        f = float(s)
        assert Fraction(f) == s, "stand-in is not exactly a float"
        v = round_float(fi, -f if r[1] else f, rnd=rnd, sat=sat)
        if check and Fraction(float(r[2])) == r[2]:
            d = round_float(fi, -float(r[2]) if r[1] else float(r[2]), rnd=rnd, sat=sat)
            same = (math.isnan(d) and math.isnan(v)) or (d == v and math.copysign(1, d) == math.copysign(1, v))
            if not same:
                raise AssertionError(f"stand-in disagrees with direct rounding for {r} in {fi.name}")
    return canonical_nan(fi) if math.isnan(v) else encode_float(fi, v)


def binary(op, src_fi, dst_fi, a_bits, b_bits, rnd=RoundMode.TiesToEven, sat=False, check=False):
    """One FMA-unit binary operation (mul/add/sub) as the hardware should perform it."""
    return project(dst_fi, OPS[op](exact(src_fi, a_bits), exact(src_fi, b_bits), rnd), rnd, sat, check)


# ---------------------------------------------------------------------------
# Choosing test inputs
# ---------------------------------------------------------------------------
# Every operand pair is sorted by where its EXACT result lands in the destination
# format.  The category does not depend on the rounding mode, so the same inputs
# serve all five modes, and a failure then points at the mode, not the operands.
# Picking evenly from each category gives rare but delicate cases (overflow,
# ties, the top binade) the same share as ordinary ones.

import random

CATEGORIES = (            # (name, most taken per array; None = no limit)
    ("NaN operand", 3),
    ("invalid: 0*Inf or Inf-Inf", 4),
    ("infinite result", 6),
    ("exact zero", 8),
    ("overflow", None),
    ("rounds to zero", None),
    ("tie", None),
    ("top binade", None),
    ("subnormal", None),
    ("inexact", None),
    ("exact", None),
)


def _position(fi, a):
    """'exact', 'tie' or 'inexact': where a (> 0) sits between fi's neighbouring numbers.

    Measured as the fractional part of a in units of fi's spacing there: 0 means
    a is one of fi's numbers, 1/2 means exactly halfway between two of them.
    """
    emin = round(math.log2(fi.smallest_normal))
    ulp = Fraction(2) ** (max(_floor_log2(a), emin) - (fi.precision - 1))
    n = a / ulp
    frac = n - n.numerator // n.denominator
    return "exact" if frac == 0 else ("tie" if frac == Fraction(1, 2) else "inexact")


def classify(op, x, y, dst_fi):
    """The category of an operand pair (exact values), for destination format dst_fi."""
    if x[0] == "nan" or y[0] == "nan":
        return "NaN operand"
    r = OPS[op](x, y, RoundMode.TiesToEven)
    if r[0] == "nan":
        return "invalid: 0*Inf or Inf-Inf"
    if r[0] == "inf":
        return "infinite result"
    a = r[2]
    if a == 0:
        return "exact zero"
    if a > Fraction(dst_fi.max):
        return "overflow"
    if a < Fraction(dst_fi.smallest_subnormal):
        return "rounds to zero"
    pos = _position(dst_fi, a)
    if pos == "tie":
        return "tie"
    if a >= Fraction(2) ** dst_fi.emax:
        return "top binade"
    if a < Fraction(dst_fi.smallest_normal):
        return "subnormal"
    return pos


def _code(fi, sign, exp_field, fraction):
    """Bit pattern of an IEEE-style 16-bit format from its fields."""
    return (sign << (fi.k - 1)) | (exp_field << (fi.precision - 1)) | fraction


def _representable(fi, v):
    """Bit pattern of the real value v in fi, or None if fi cannot hold it exactly."""
    try:
        bits = encode_float(fi, round_float(fi, float(v)))
    except (OverflowError, ValueError):
        return None
    x = exact(fi, bits)
    return bits if x[0] == "num" and x[2] == abs(Fraction(v)) and (x[1] == (v < 0) or x[2] == 0) else None


def _pool_16bit(op, src_fi, dst_fi, rng):
    """Candidate operand pairs for a 16-bit source format, whose 2^32 pairs cannot all be tried."""
    p, ebits = src_fi.precision, src_fi.k - src_fi.precision
    top_exp, bias, fmask = (1 << ebits) - 2, (1 << (ebits - 1)) - 1, (1 << (p - 1)) - 1

    def rand_code():
        return _code(src_fi, rng.randrange(2), rng.randrange(top_exp + 1), rng.randrange(fmask + 1))

    # The format's edges, both signs: exponents at the bottom, middle and top, with
    # fractions of all zeros, one, half, and all ones.  Plus the specials.
    exps = sorted({0, 1, 2, bias - 1, bias, bias + 1, top_exp - 1, top_exp})
    fracs = sorted({0, 1, 1 << (p - 2), fmask - 1, fmask})
    edges = {_code(src_fi, s, e, f) for s in (0, 1) for e in exps for f in fracs}
    inf = _code(src_fi, 0, top_exp + 1, 0)
    edges |= {inf, inf | (1 << (src_fi.k - 1)), canonical_nan(src_fi)}
    pool = {(a, b) for a in edges for b in edges}

    pool |= {(rand_code(), rand_code()) for _ in range(20000)}

    # Constructed ties at the destination's precision.
    pd = dst_fi.precision
    emin_d = round(math.log2(dst_fi.smallest_normal))
    for _ in range(3000):
        if op in ("add", "sub"):
            xb = rand_code()
            x = exact(src_fi, xb)
            if x[2] == 0:
                continue
            half_ulp = Fraction(2) ** (max(_floor_log2(x[2]), emin_d) - (pd - 1)) / 2
            yb = _representable(src_fi, half_ulp if rng.randrange(2) else -half_ulp)
            if yb is not None:
                pool.add((xb, yb))
        else:
            t = rng.randrange(emin_d - pd, dst_fi.emax)
            j = rng.randrange(t - 20, t + 20)
            xv = Fraction(2) ** j * (1 + Fraction(1, 2 ** (pd - 1)))
            yv = Fraction(2) ** (t - j) * Fraction(3, 2)
            xb, yb = _representable(src_fi, xv), _representable(src_fi, yv)
            if xb is not None and yb is not None:
                pool.add((xb, yb))

    # Near-cancellation for add/sub: x and -x, and x and its neighbour.
    if op in ("add", "sub"):
        for _ in range(2000):
            xb = rand_code()
            nb = xb + 1 if (xb & fmask) != fmask else xb
            flip = 1 << (src_fi.k - 1)
            pool |= {(xb, xb ^ flip), (xb, nb ^ flip)} if op == "add" else {(xb, xb), (xb, nb)}
    return sorted(pool)


def binary_inputs(op, src_fi, dst_fi, count=128, seed=0):
    """Operand pairs for one array, picked evenly across CATEGORIES.

    8-bit sources: every one of the 65,536 pairs is classified.
    16-bit sources: a pool of edges, random values, constructed ties and
    near-cancelling pairs is classified instead.

    Returns (pairs, {category: number taken}).
    """
    rng = random.Random(f"{seed}/{op}/{src_fi.name}/{dst_fi.name}")
    if src_fi.k == 8:
        pool = [(a, b) for a in range(256) for b in range(256)]
    else:
        pool = _pool_16bit(op, src_fi, dst_fi, rng)
    buckets = {name: [] for name, _ in CATEGORIES}
    for a, b in pool:
        buckets[classify(op, exact(src_fi, a), exact(src_fi, b), dst_fi)].append((a, b))
    for lst in buckets.values():
        rng.shuffle(lst)

    picked, taken = [], {name: 0 for name, _ in CATEGORIES}
    while len(picked) < count:
        progress = False
        for name, cap in CATEGORIES:
            if len(picked) == count:
                break
            if (cap is None or taken[name] < cap) and taken[name] < len(buckets[name]):
                picked.append(buckets[name][taken[name]])
                taken[name] += 1
                progress = True
        if not progress:                      # every bucket exhausted: repeat what we have
            picked += picked[:count - len(picked)]
    return picked, taken
