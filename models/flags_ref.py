"""Exception flags of a rounding, from exact arithmetic.

IEEE 754 / RISC-V semantics, with tininess detected after rounding (as in
hardfloat):

  overflow   the value rounded to the format's precision with an unbounded
             exponent is larger in magnitude than the largest finite value
  underflow  that unbounded result is below the smallest normal, and the
             delivered result is inexact
  inexact    the delivered result differs from the exact value
  invalid    passed in by the caller (a signalling NaN operand, 0 x Inf, ...)

Infinities, NaNs and exact zeros raise nothing here. Saturation changes the
delivered code but not the flags, so it is not an argument.

    flags(x, fi, mode, invalid=False) -> (invalid, False, overflow, underflow, inexact)

x is an exact value in fma_ref's form: ("nan",) | ("inf", sign) |
("num", sign, magnitude as a Fraction). fi is a gfloat format. mode is
hardfloat's rounding-mode code.
"""
from fractions import Fraction

RNE, RTZ, RDN, RUP, RMM, RODD = 0, 1, 2, 3, 4, 6
FRM_MODES = (RNE, RTZ, RDN, RUP, RMM)


def floor_log2(a):
    e = a.numerator.bit_length() - a.denominator.bit_length()
    if Fraction(2) ** e > a:
        e -= 1
    return e


def _round_to_grid(mag, sign, q, mode):
    """Round mag (> 0) to a multiple of 2^q."""
    s = mag / Fraction(2) ** q
    whole = s.numerator // s.denominator
    rest = s - whole
    if rest:
        up = {RNE: rest > Fraction(1, 2) or (rest == Fraction(1, 2) and whole % 2 == 1),
              RMM: rest >= Fraction(1, 2),
              RTZ: False,
              RDN: sign == 1,
              RUP: sign == 0,
              RODD: whole % 2 == 0}[mode]
        whole += 1 if up else 0
    return whole * Fraction(2) ** q


def flags(x, fi, mode, invalid=False):
    if invalid:
        return (True, False, False, False, False)
    if x[0] != "num" or x[2] == 0:
        return (False, False, False, False, False)
    _, sign, mag = x
    P = fi.precision
    min_normal = Fraction(fi.smallest_normal)

    unbounded = _round_to_grid(mag, sign, floor_log2(mag) - P + 1, mode)
    overflow = unbounded > Fraction(fi.max)
    tiny = unbounded < min_normal
    bounded = _round_to_grid(mag, sign, max(floor_log2(mag), floor_log2(min_normal)) - P + 1, mode)
    inexact = overflow or bounded != mag
    return (False, False, overflow, tiny and inexact, inexact)


def pack(f):
    """(NV, DZ, OF, UF, NX) -> the 5-bit exceptionFlags encoding."""
    return sum(on << (4 - i) for i, on in enumerate(f))


def bf16_exact(bits):
    """A BF16 pattern as an exact value in fma_ref's form."""
    exp, frac, sign = (bits >> 7) & 0xFF, bits & 0x7F, bits >> 15
    if exp == 0xFF:
        return ("nan",) if frac else ("inf", sign)
    mag = Fraction(frac, 128) * Fraction(2) ** -126 if exp == 0 else \
          Fraction(128 + frac, 128) * Fraction(2) ** (exp - 127)
    return ("num", sign, mag)


def bf16_is_snan(bits):
    return (bits >> 7) & 0xFF == 0xFF and bits & 0x7F != 0 and not (bits >> 6) & 1
