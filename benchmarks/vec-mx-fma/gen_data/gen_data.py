#!/usr/bin/env python3
"""Generate vec-mx-fma/data.S from the fma_ref reference model.

  ./gen_data.py -o ../data.S              # OCP FP8 build
  ./gen_data.py --std p3109 -o ../data.S  # P3109 build (also p3109-finite)

vec-mx-binary tests the two-operand .vv forms. This benchmark tests the rest
of the FMA unit's forms:

  * three operands, .vv: macc, nmacc, msac, nmsac, madd, and widening wmacc;
  * a scalar operand, .vf: mul, add, rsub, wmul, wadd, macc, wmacc.

In the .vf arrays, b is constant over each block of BLOCK elements: main.c
moves it into f0 once per block. The third operand c is in the result's
format and is chosen against the product a x b: zero, its negation (massive
cancellation), a nearby value (alignment and rounding), the smallest
subnormal (sticky bit), the largest finite value, a special, or random.

Every array is repeated per rounding mode (_rne .. _rmm) with the same inputs.
The expected result is computed exactly and rounded once.
"""

import argparse
import math
import os
import random
import sys

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "common-data-gen"))

from gfloat import RoundMode, decode_float, encode_float, round_float  # noqa: E402
from gfloat.formats import format_info_bfloat16, format_info_binary16, format_info_binary32  # noqa: E402
from fma_ref import add, binary_inputs, exact, mul, project, sub  # noqa: E402
from gfloat_ref import FP8_STANDARDS, FRM, canonical_nan, print_array, print_header, print_uint32  # noqa: E402

COUNT = 128
BLOCK = 16   # .vf: elements per scalar; main.c's BLOCK


def neg(x):
    return x if x[0] == "nan" else (x[0], 1 - x[1]) + x[2:]


# name -> (scalar b, has c, wide result, exact result from (a, b, c, rnd))
OPS = {
    "macc":     (False, True,  False, lambda a, b, c, r: add(mul(a, b, r), c, r)),
    "nmacc":    (False, True,  False, lambda a, b, c, r: sub(neg(mul(a, b, r)), c, r)),
    "msac":     (False, True,  False, lambda a, b, c, r: sub(mul(a, b, r), c, r)),
    "nmsac":    (False, True,  False, lambda a, b, c, r: add(neg(mul(a, b, r)), c, r)),
    "madd":     (False, True,  False, lambda a, b, c, r: add(mul(a, c, r), b, r)),   # vd = vs1 * vd + vs2
    "wmacc":    (False, True,  True,  lambda a, b, c, r: add(mul(a, b, r), c, r)),
    "mul_vf":   (True,  False, False, lambda a, b, c, r: mul(a, b, r)),
    "add_vf":   (True,  False, False, lambda a, b, c, r: add(a, b, r)),
    "rsub_vf":  (True,  False, False, lambda a, b, c, r: sub(b, a, r)),
    "wmul_vf":  (True,  False, True,  lambda a, b, c, r: mul(a, b, r)),
    "wadd_vf":  (True,  False, True,  lambda a, b, c, r: add(a, b, r)),
    "macc_vf":  (True,  True,  False, lambda a, b, c, r: add(mul(a, b, r), c, r)),
    "wmacc_vf": (True,  True,  True,  lambda a, b, c, r: add(mul(a, b, r), c, r)),
}


# name -> (operand format, widened result format)
def formats(std):
    return {
        "fp16": (format_info_binary16, format_info_binary32),
        "bf16": (format_info_bfloat16, format_info_binary32),
        "e4m3": (FP8_STANDARDS[std]["altfmt0"], format_info_bfloat16),
        "e5m2": (FP8_STANDARDS[std]["altfmt1"], format_info_bfloat16),
    }


def code(fi, v):
    """fi's code for the real v rounded to nearest, or None if that is not finite."""
    r = round_float(fi, v, rnd=RoundMode.TiesToEven)
    return encode_float(fi, r) if math.isfinite(r) else None


def random_finite(fi, rng):
    while True:
        c = rng.randrange(1 << fi.k)
        if math.isfinite(decode_float(fi, c).fval):
            return c


def specials(fi):
    """NaN and, where the format has them, the infinities."""
    out = [canonical_nan(fi)]
    for v in (math.inf, -math.inf):
        r = round_float(fi, v, rnd=RoundMode.TiesToEven)
        if math.isinf(r):
            out.append(encode_float(fi, r))
    return out


def addend(fi, p, i, rng):
    """The i-th third operand, in format fi, for the exact product p."""
    pv = float(p[2]) * (-1 if p[1] else 1) if p[0] == "num" else None
    kind = i % 7
    c = None
    if kind == 0:
        c = code(fi, 0.0)
    elif kind == 1 and pv is not None:
        c = code(fi, -pv)
    elif kind == 2 and pv:
        c = code(fi, pv * 2.0 ** rng.randint(-fi.precision - 2, fi.precision + 2) * rng.choice((1, -1)) * rng.uniform(1, 2))
    elif kind == 3:
        c = code(fi, rng.choice((1, -1)) * fi.smallest_subnormal)
    elif kind == 4:
        c = code(fi, rng.choice((1, -1)) * fi.max)
    elif kind == 5:
        c = rng.choice(specials(fi))
    return c if c is not None else random_finite(fi, rng)


def scalars(fi, rng, n):
    """n block scalars: 1, 0, NaN, +-maxFinite or Inf, the smallest subnormal, random."""
    sp = specials(fi)
    s = [code(fi, 1.0), code(fi, 0.0), sp[0], sp[1] if len(sp) > 1 else code(fi, fi.max),
         code(fi, -fi.max), code(fi, -fi.smallest_subnormal)]
    while len(s) < n:
        s.append(random_finite(fi, rng))
    return s[:n]


def tests(std, count):
    """Every array: (name, operand format, result format, has c, a, b, c, {mode: expected})."""
    for fname, (src, wide_fmt) in formats(std).items():
        for op, (vf, has_c, wide, fn) in OPS.items():
            dst = wide_fmt if wide else src
            rng = random.Random(f"{fname}/{op}")
            pairs, _ = binary_inputs("mul", src, dst, count)
            a = [p[0] for p in pairs]
            if vf:
                blk = scalars(src, rng, -(-count // BLOCK))
                b = [blk[i // BLOCK] for i in range(count)]
            else:
                b = [p[1] for p in pairs]
            c = None
            if op == "madd":     # c multiplies a, b is the addend
                c = b
                b = [addend(src, mul(exact(src, x), exact(src, y), RoundMode.TiesToEven), i, rng) for i, (x, y) in enumerate(zip(a, c))]
            elif has_c:
                c = [addend(dst, mul(exact(src, x), exact(src, y), RoundMode.TiesToEven), i, rng) for i, (x, y) in enumerate(zip(a, b))]
            res = {mode: [project(dst, fn(exact(src, x), exact(src, y), exact(dst, z) if has_c else None, rnd), rnd)
                          for x, y, z in zip(a, b, c if has_c else a)]
                   for mode, (_frm, rnd) in FRM.items()}
            yield f"{fname}_{op}", src, dst, has_c, a, b, c, res


def emit(out, std, count):
    opt = "" if std == "ocp" else f" --std {std}"
    print_header(out, f"Generated by gen_data.py{opt} (fma_ref reference). Do not edit.")
    print_uint32(out, "N", count)
    for name, src, dst, has_c, a, b, c, res in tests(std, count):
        ssz, dsz = src.k // 8, dst.k // 8
        for mode, r in res.items():
            print_array(out, f"{name}_{mode}", "_a", a, ssz)
            print_array(out, f"{name}_{mode}", "_b", b, ssz)
            if has_c:
                print_array(out, f"{name}_{mode}", "_c", c, dsz)
            print_array(out, f"{name}_{mode}", "_out", r, dsz)


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--std", choices=sorted(FP8_STANDARDS), default="ocp", help="8-bit formats of the build (default: ocp)")
    ap.add_argument("-n", "--count", type=int, default=COUNT, help=f"elements per array (default: {COUNT})")
    ap.add_argument("-o", "--output", help="write here instead of stdout")
    args = ap.parse_args()
    out = open(args.output, "w") if args.output else sys.stdout
    try:
        emit(out, args.std, args.count)
    finally:
        if args.output:
            out.close()


if __name__ == "__main__":
    main()
