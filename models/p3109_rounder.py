"""Bit-accurate Python model of P3109Rounder.scala.

Rounds a hardfloat RawFloat to an 8-bit P3109 code, binary8p4 ("p4") or
binary8p3 ("p3"). The steps and names follow hardfloat's RoundAnyRawFNToRecFN
and the Chisel module, so the two can be read side by side. Checked against
gfloat, rto_ref and flags_ref by the validate_*.py scripts.
"""

from flags_ref import RNE, RDN, RUP, RMM, RODD  # hardfloat rounding-mode codes


# ---------------------------------------------------------------------------
# hardfloat primitives, ported
# ---------------------------------------------------------------------------


def low_mask(val, width, top_bound, bottom_bound):
    """Port of hardfloat's lowMask (primitives.scala:45).

    Returns a (|top-bottom|)-bit thermometer mask.  Only the base case is
    transcribed; the recursive divide-and-conquer branch above 64 values is a
    synthesis optimisation that computes the same function.
    """
    num_in_vals = 1 << width
    if top_bound < bottom_bound:
        return low_mask(~val & (num_in_vals - 1), width,
                        num_in_vals - 1 - top_bound, num_in_vals - 1 - bottom_bound)
    shift = (-1 << num_in_vals) >> val          # Python >> on negatives is arithmetic
    hi, lo = num_in_vals - 1 - bottom_bound, num_in_vals - top_bound
    field = (shift >> lo) & ((1 << (hi - lo + 1)) - 1)
    out, n = 0, hi - lo + 1                     # Reverse()
    for i in range(n):
        if field & (1 << i):
            out |= 1 << (n - 1 - i)
    return out


def raw_from_fn(bits, exp_width, sig_width):
    """Port of rawFloatFromFN: an IEEE bit pattern -> hardfloat's RawFloat.

    sig is (sig_width+1) bits, '0 ## 1 ## fraction' for a normal number, with
    subnormals normalised by the shift below.  sExp is biased by 2^exp_width.
    """
    sign = (bits >> (exp_width + sig_width - 1)) & 1
    exp_in = (bits >> (sig_width - 1)) & ((1 << exp_width) - 1)
    fract_in = bits & ((1 << (sig_width - 1)) - 1)

    is_zero_exp = exp_in == 0
    is_zero_fract = fract_in == 0

    norm_dist = (sig_width - 1) - fract_in.bit_length() if fract_in else sig_width - 1
    subnorm_fract = ((fract_in << norm_dist) & ((1 << (sig_width - 2)) - 1)) << 1
    adjusted_exp = (
        (norm_dist ^ ((1 << (exp_width + 1)) - 1)) if is_zero_exp else exp_in
    ) + ((1 << (exp_width - 1)) | (2 if is_zero_exp else 1))

    is_zero = is_zero_exp and is_zero_fract
    is_special = (adjusted_exp >> (exp_width - 1)) & 3 == 3

    return dict(
        isNaN=is_special and not is_zero_fract,
        isInf=is_special and is_zero_fract,
        isZero=is_zero,
        sign=sign,
        sExp=adjusted_exp & ((1 << (exp_width + 1)) - 1),
        sig=(0 if is_zero else 1 << (sig_width - 1))
            | (subnorm_fract if is_zero_exp else fract_in),
    )


# ---------------------------------------------------------------------------
# the rounder
# ---------------------------------------------------------------------------


SIG_INT = 4        # datapath significand width: binary8p4's precision
B_INT = 256        # internal exponent bias, BF16's
EXP_SLICE = 10     # bits of exponent fed to the mask decoder

# The mask decoder covers binary8p4's subnormal range; binary8p3 reaches it via delta
MASK_BOTTOM = B_INT - 7
MASK_TOP = MASK_BOTTOM - SIG_INT - 1

# Exponents biased by B_INT. delta moves the subnormal threshold to the format's
# smallest normal; prec_shift is the precision the format lacks against SIG_INT.
FMT = {
    "p4": dict(
        frac_bits=3,
        min_norm=B_INT - 7,        # emin = -7
        min_nonzero=B_INT - 10,    # emin - (P-1) = -10, hardfloat's outMinNonzeroExp
        emax=B_INT + 7,
        delta=0, prec_shift=0,     # the datapath is built for this format
    ),
    "p3": dict(
        frac_bits=2,
        min_norm=B_INT - 15,       # emin = -15
        min_nonzero=B_INT - 17,
        emax=B_INT + 15,
        delta=8, prec_shift=1,     # 249 - 241 = 8, and one fewer fraction bit
    ),
}


def p3109_round(raw, fmt, mode, sat=False, finite=False,
                in_exp_width=8, in_sig_width=8,
                invalid_exc=False, sig_msb_always_zero=True):
    """Round a hardfloat RawFloat to an 8-bit P3109 code.

    fmt is "p4" or "p3" (altfmt), finite selects the domain, sat the SatFinite
    variant. Returns (code, flags), flags = (invalid, infinite, overflow,
    underflow, inexact).
    """
    f = FMT[fmt]

    near_even, near_max = mode == RNE, mode == RMM
    odd = mode == RODD
    round_mag_up = (mode == RDN and raw["sign"]) or (mode == RUP and not raw["sign"])

    # --- re-bias the exponent (hardfloat: sAdjustedExp) --------------------
    s_adjusted_exp = raw["sExp"] + (B_INT - (1 << in_exp_width))

    # --- align the significand (hardfloat: adjustedSig) --------------------
    # [top][hidden][3 fraction][guard][sticky]
    if in_sig_width <= SIG_INT + 2:
        adjusted_sig = raw["sig"] << (SIG_INT - in_sig_width + 2)
    else:
        keep = raw["sig"] >> (in_sig_width - SIG_INT - 1)
        rest = raw["sig"] & ((1 << (in_sig_width - SIG_INT - 1)) - 1)
        adjusted_sig = (keep << 1) | (1 if rest else 0)

    do_shift_down1 = 0 if sig_msb_always_zero else (adjusted_sig >> (SIG_INT + 2)) & 1

    # --- the round mask: subnormal widening moved by delta, then prec_shift --
    mask_exp = (s_adjusted_exp + f["delta"]) & ((1 << EXP_SLICE) - 1)
    mask_body = low_mask(mask_exp, EXP_SLICE, MASK_TOP, MASK_BOTTOM) | do_shift_down1
    mask_body = (mask_body << f["prec_shift"]) | ((1 << f["prec_shift"]) - 1)
    mask_body &= (1 << (SIG_INT + 1)) - 1
    round_mask = (mask_body << 2) | 0b11

    shifted_round_mask = round_mask >> 1
    round_pos_mask = ~shifted_round_mask & round_mask
    round_pos_bit = (adjusted_sig & round_pos_mask) != 0
    any_round_extra = (adjusted_sig & shifted_round_mask) != 0
    any_round = round_pos_bit or any_round_extra

    round_incr = ((near_even or near_max) and round_pos_bit) or (round_mag_up and any_round)
    if round_incr:
        rounded_sig = ((adjusted_sig | round_mask) >> 2) + 1
        if near_even and round_pos_bit and not any_round_extra:
            rounded_sig &= ~(round_mask >> 1)          # ties-to-even: clear the LSB
    else:
        rounded_sig = (adjusted_sig & ~round_mask) >> 2
        if odd and any_round:
            rounded_sig |= round_pos_mask >> 1

    s_rounded_exp = s_adjusted_exp + (rounded_sig >> SIG_INT)

    # binary8p4's fraction field; binary8p3's low bit is always masked off
    frac_out = ((rounded_sig >> 1) if do_shift_down1 else rounded_sig) & 0b111
    assert fmt == "p4" or frac_out & 1 == 0
    frac = frac_out >> (3 - f["frac_bits"])

    # --- range checks: against the format's largest finite value ------------
    max_frac = (1 << f["frac_bits"]) - (1 if finite else 2)
    common_overflow = s_rounded_exp > f["emax"] or (
        s_rounded_exp == f["emax"] and frac > max_frac)
    common_total_underflow = s_rounded_exp < f["min_nonzero"]

    # Tininess after rounding, at the format's last kept bit
    lsb = (1 if do_shift_down1 else 0) + f["prec_shift"]
    round_carry = (rounded_sig >> (SIG_INT + 1 if do_shift_down1 else SIG_INT)) & 1
    ur_round_pos_bit = (adjusted_sig >> (lsb + 1)) & 1
    ur_any_round = (adjusted_sig & ((1 << (lsb + 2)) - 1)) != 0
    ur_round_incr = ((near_even or near_max) and ur_round_pos_bit) or (round_mag_up and ur_any_round)
    # hardfloat also requires s_adjusted_exp <= min_norm; the clamped mask
    # exponent makes the mask bit imply it
    tiny_mask_bit = (round_mask >> (lsb + 2)) & 1
    assert not tiny_mask_bit or s_adjusted_exp <= f["min_norm"]
    common_underflow = common_total_underflow or (
        any_round and tiny_mask_bit
        and (((round_mask >> (lsb + 3)) & 1)
             or not (round_carry and round_pos_bit and ur_round_incr)))
    common_inexact = common_total_underflow or any_round

    # --- specials and flags (hardfloat's tail) -----------------------------
    is_nan_out = invalid_exc or raw["isNaN"]
    is_inf_in = raw["isInf"]
    common_case = not is_nan_out and not is_inf_in and not raw["isZero"]
    overflow = common_case and common_overflow
    underflow = common_case and common_underflow
    inexact = overflow or (common_case and common_inexact)

    # Round-to-odd overflows to Inf/NaN too (P3109 4.7.5)
    overflow_round_mag_up = near_even or near_max or round_mag_up or odd
    # Read only in the total-underflow branch below, as in the RTL
    peg_min_nonzero = round_mag_up or odd

    # --- encode the 8-bit P3109 code ---------------------------------------
    sign = raw["sign"]
    sb = sign << 7
    nan_code = 0x80
    max_finite_code = sb | (0x7F if finite else 0x7E)
    inf_code = nan_code if finite else (sb | 0x7F)

    if is_nan_out:
        code = nan_code
    elif is_inf_in:
        # A true infinity clamps only when saturating; otherwise it stays an
        # infinity, or becomes NaN in the finite domain, which has none.
        code = max_finite_code if sat else inf_code
    elif raw["isZero"]:
        code = 0x00                                   # P3109 has no -0
    elif overflow:
        # sat forces the clamp; otherwise inward rounding clamps and the
        # nearest/outward modes go to the overflow code point.
        code = max_finite_code if (sat or not overflow_round_mag_up) else inf_code
    elif common_total_underflow:
        code = (sb | 1) if peg_min_nonzero else 0x00  # -0 flushes to +0
    elif s_rounded_exp < f["min_norm"]:
        # Subnormal: the widened mask already rounded to the subnormal grid,
        # so this shift only moves the bits into place and loses nothing.
        shift = f["min_norm"] - s_rounded_exp
        sig_with_hidden = (1 << f["frac_bits"]) | frac
        assert sig_with_hidden & ((1 << shift) - 1) == 0, "subnormal shift dropped a bit"
        field = sig_with_hidden >> shift
        assert field != 0, "not total underflow, so the field keeps the hidden bit"
        code = sb | field
    else:
        exp_field = s_rounded_exp - f["min_norm"] + 1
        code = sb | (exp_field << f["frac_bits"]) | frac

    return code, (invalid_exc, False, overflow, underflow, inexact)


def convert_bf16(bits, fmt, mode, sat=False, finite=False):
    """BF16 bit pattern -> P3109 code, the whole conversion path."""
    return p3109_round(raw_from_fn(bits, 8, 8), fmt, mode, sat, finite)[0]
