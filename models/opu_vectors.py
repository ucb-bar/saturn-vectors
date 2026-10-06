"""Test vectors for the 8-bit multiply-accumulate of one OuterProductCell.

    opu_vectors.py <outdir> <std: ocp | p3109 | p3109-finite>

Writes opu.bin for tb_opu_cell.cpp, one 12-byte record per multiply-accumulate,

    u8 altfmt, u8 a, u8 b, u8 0, u32 c, u32 expect

c is the FP32 accumulator before, expect after: c + a x b, rounded once to
FP32 with round-to-nearest-even, which the cell always uses.

Every (a, b) pair of both formats, each with:
  * fixed accumulators: +-0, +-1, +-maxFinite, the smallest subnormal, +-Inf, NaN
  * one random accumulator within 2^+-26 of the product, which makes the
    addition round.
"""
import math
import os
import random
import struct
import sys
from multiprocessing import Pool

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "benchmarks", "common-data-gen"))

from gfloat import RoundMode  # noqa: E402
from gfloat.formats import format_info_binary32 as FP32  # noqa: E402
from gfloat_ref import FP8_STANDARDS  # noqa: E402
from fma_ref import add, exact, mul, project  # noqa: E402

RNE = RoundMode.TiesToEven
FIXED_C = (0x00000000, 0x80000000, 0x3F800000, 0xBF800000, 0x7F7FFFFF,
           0xFF7FFFFF, 0x00000001, 0x7F800000, 0xFF800000, 0x7FC00000)


def near_c(rng, p):
    """A random FP32 value within 2^+-26 of the product p."""
    # a product of two 8-bit values is exact in a float
    e = math.frexp(float(p[2]))[1] - 1 if p[0] == "num" and p[2] != 0 else 0
    e = max(-126, min(127, e + rng.randint(-26, 26)))
    return rng.getrandbits(1) << 31 | (e + 127) << 23 | rng.getrandbits(23)


def records(args):
    std, altfmt, a_hi = args
    fi = FP8_STANDARDS[std][f"altfmt{altfmt}"]
    rng = random.Random(altfmt << 8 | a_hi)
    out = bytearray()
    for a in range(a_hi << 4, (a_hi + 1) << 4):
        for b in range(256):
            p = mul(exact(fi, a), exact(fi, b), RNE)
            for c in FIXED_C + (near_c(rng, p),):
                want = project(FP32, add(p, exact(FP32, c), RNE), RNE)
                out += struct.pack("<BBBxII", altfmt, a, b, c, want)
    return bytes(out)


if __name__ == "__main__":
    if len(sys.argv) != 3 or sys.argv[2] not in FP8_STANDARDS:
        sys.exit(f"usage: opu_vectors.py <outdir> <{' | '.join(FP8_STANDARDS)}>")
    with Pool() as pool, open(os.path.join(sys.argv[1], "opu.bin"), "wb") as f:
        for chunk in pool.map(records, [(sys.argv[2], f, a_hi) for f in (0, 1) for a_hi in range(16)]):
            f.write(chunk)
