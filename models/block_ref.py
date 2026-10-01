"""Reference for the two P3109 block conversion operations, built on gfloat.

This is the slow, obvious transcription of sections 5.4 and 5.5 of the standard.
The RTL computes the same thing by shifting an exponent field; this computes it
by decoding to a real number, multiplying or dividing, and rounding.

    ConvertFromBlock (5.5.1)  8-bit code + scale -> BF16
        Z = omegaBlockDecode(s, x) = Multiply(decode(s), decode(x))
        r = omegaProject(Z)

    ConvertToBlock   (5.5.2)  BF16 + scale -> 8-bit code
        Z = omegaBlockProject: NaN if either is NaN, 0 if the scale is 0,
                               otherwise Divide(X, S)
        r = omegaProject(Z)

The scale format is Binary8p1uf (4.5, Fs): 8 bits, unsigned, finite, precision
1, so bias = 2^(K-P) = 128 and the code point is the biased exponent itself.
"""
import math

from gfloat import RoundMode, decode_float, encode_float, round_float
from gfloat_ref import BF16, canonical_nan, p3109_format

SCALE_BIAS = 128
SCALE_NAN = 255


def scale_value(s):
    """Decode a Binary8p1uf code. Returns None for NaN, else a float."""
    if s == SCALE_NAN:
        return None
    if s == 0:
        return 0.0
    return 2.0 ** (s - SCALE_BIAS)


def convert_from_block(code, scale, precision, finite,
                       rnd=RoundMode.TiesToEven):
    """ConvertFromBlock: an 8-bit element and its scale, widened to BF16."""
    src = p3109_format(precision, finite)
    X = decode_float(src, code).fval
    S = scale_value(scale)

    # omegaBlockDecode is a Multiply, so Multiply's special cases apply (4.10.4).
    if S is None or math.isnan(X):
        return canonical_nan(BF16)
    if S == 0.0:
        if math.isinf(X):
            return canonical_nan(BF16)       # Multiply(Inf, 0) -> NaN
        # Zero absorbs, but keeps the element's sign: BF16 has a signed zero.
        return encode_float(BF16, math.copysign(0.0, X))

    Z = X * S
    r = round_float(BF16, Z, rnd=rnd)
    if math.isnan(r):
        return canonical_nan(BF16)
    return encode_float(BF16, r)


def convert_to_block(bits, scale, precision, finite,
                     rnd=RoundMode.TiesToEven, sat=False):
    """ConvertToBlock: a BF16 value and a scale, narrowed to an 8-bit element."""
    dst = p3109_format(precision, finite)
    X = decode_float(BF16, bits).fval
    S = scale_value(scale)

    # omegaBlockProject (5.4.2), in the order the standard lists the cases.
    if S is None or math.isnan(X):
        return canonical_nan(dst)
    if S == 0.0:
        return encode_float(dst, 0.0)        # NOTE 1: a zero scale flattens all
    # S is never infinite (Binary8p1uf's domain is finite), so the third case of
    # 5.4.2 cannot arise. Divide's own special cases are reached by ordinary
    # arithmetic: Inf / finite -> Inf, 0 / finite -> 0.
    Z = X / S

    r = round_float(dst, Z, rnd=rnd, sat=sat)
    if math.isnan(r):
        return canonical_nan(dst)
    return encode_float(dst, r)
