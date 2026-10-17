"""How an FMA core hands its result to the 8-bit rounder.

Shared by validate_rounder_fma.py (which checks the Python rounder model) and
dump_fma_expected.py (which writes vectors for the RTL), so both build the raw
number the same way.
"""
from fractions import Fraction

from flags_ref import floor_log2

# Core type -> (exponent width, significand width).  The rounder is handed a
# RawFloat(exp, sig+2): the multiply-add result before any rounding.
CORES = {
    "FP64": (11, 53),
    "FP32": (8, 24),
    "FP16": (5, 11),
    "BF16": (8, 8),
    "E5M3": (5, 4),
}


def raw_from_exact(r, exp_width, sig_width, unnormalized=False):
    """Build the RawFloat an FMA core would hand the rounder for exact result r.

    sig is (sig_width+1) bits with everything below it OR-ed into the low bit,
    which is what MulAddRecFNToRaw_postMul produces.  With unnormalized=True the
    same value is presented one binade lower, so sig lands in [2,4) and the
    rounder must take the doShiftSigDown1 path.
    """
    base = dict(isNaN=False, isInf=False, isZero=False, sign=0, sExp=0, sig=0)
    if r[0] == "nan":
        return {**base, "isNaN": True}
    if r[0] == "inf":
        return {**base, "isInf": True, "sign": r[1]}
    _, sign, mag = r
    if mag == 0:
        return {**base, "isZero": True, "sign": sign}

    e = floor_log2(mag) - (1 if unnormalized else 0)
    scaled = (mag / Fraction(2) ** e) * (1 << (sig_width - 1))   # in [1,2) or [2,4)
    sig = scaled.numerator // scaled.denominator
    if sig * scaled.denominator != scaled.numerator:
        sig |= 1                                                  # sticky
    return {**base, "sign": sign, "sExp": e + (1 << exp_width), "sig": sig}
