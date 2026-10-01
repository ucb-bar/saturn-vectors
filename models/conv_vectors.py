"""Test vectors for the P3109 conversions in FPConvBlock, codes and flags.

    conv_vectors.py <outdir> <finite: 0|1> <block: 0|1>

Writes conv.bin for tb_conv.cpp: one 48-byte record per cycle, four lanes each,

    u8 fmt, u8 frm, u8 flags, u8 in_eew, u32 0, u64 in, u64 scale, u64 expect, u64 mask, u64 exc

flags: bit0 widen, bit1 narrow, bit2 round-to-odd, bit3 sat. exc holds each
lane's expected exception flags in bytes 0..3. A lane's Binary8p1uf scale sits
in the same byte as its 8-bit element; the other scale bytes hold junk the unit
must ignore.

Without block scaling (block = 0), every lane gets scale 2^0:
  * widening: every code of both formats, five rounding modes
  * narrowing: every BF16 pattern, both formats, the five frm modes and
    round-to-odd, SatNone and SatFinite
With block scaling (block = 1), each lane has its own scale:
  * widening: every code x every scale, both formats, five rounding modes
  * narrowing: every BF16 pattern x every scale at (RNE, SatNone), the
    projection P3109 4.5 requires; and the other modes, round-to-odd and
    SatFinite on 8 scales

Codes come from block_ref (gfloat) and rto_ref, flags from flags_ref.
"""
import os
import struct
import sys
from fractions import Fraction
from multiprocessing import Pool

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "benchmarks", "common-data-gen"))
sys.path.insert(0, HERE)

from gfloat import decode_float  # noqa: E402
from gfloat_ref import BF16, FRM, p3109_format  # noqa: E402
from block_ref import SCALE_BIAS, SCALE_NAN, convert_from_block, convert_to_block  # noqa: E402
from flags_ref import flags, pack, bf16_exact, bf16_is_snan, RODD  # noqa: E402
from rto_ref import project_odd  # noqa: E402

WIDEN, NARROW, RTO, SAT = 1, 2, 4, 8
REC = struct.Struct("<BBBBIQQQQQ")
JUNK = 0x5A
ONE = SCALE_BIAS                                   # 2^0
SPREAD = (0, 1, 64, 127, 128, 129, 200, 254)       # 255 (NaN) is in the exhaustive sweep
RND = {frm: rnd for frm, rnd in FRM.values()}
FMTS = ((0, 4), (1, 3))                            # (altfmt, precision)


def scale_word(scales):
    return sum((s << (16 * l)) | (JUNK << (16 * l + 8)) for l, s in enumerate(scales))


def scaled(x, s, divide):
    """Exact value x times (or divided by) the Binary8p1uf scale s; s is not NaN or 0."""
    if x[0] != "num":
        return x
    f = Fraction(2) ** (s - SCALE_BIAS)
    return ("num", x[1], x[2] / f if divide else x[2] * f)


def widen(code, s, P, finite, frm):
    fi = p3109_format(P, finite)
    out = convert_from_block(code, s, P, finite, RND[frm])
    v = decode_float(fi, code).fval
    if s in (0, SCALE_NAN) or v != v:
        return out, 0
    x = ("num", int(v < 0), abs(Fraction(v))) if abs(v) != float("inf") else ("inf", int(v < 0))
    return out, pack(flags(scaled(x, s, False), BF16, frm))


def narrow(bits, s, P, finite, frm, sat):
    fi = p3109_format(P, finite)
    snan = bf16_is_snan(bits)
    x = bf16_exact(bits)
    if frm == RODD:
        if s == SCALE_NAN or x[0] == "nan":
            out = 0x80
        elif s == 0:
            out = 0
        else:
            out = project_odd(scaled(x, s, True), fi, sat)
    else:
        out = convert_to_block(bits, s, P, finite, RND[frm], sat)
    if s in (0, SCALE_NAN) or x[0] == "nan":
        return out, pack((snan, False, False, False, False))
    return out, pack(flags(scaled(x, s, True), fi, frm, invalid=snan))


def record(fmt, frm, kind, sat, ins, scales, outs, excs):
    lanes = range(len(ins))
    narrowing = kind == NARROW
    rec_flags = kind | (RTO if frm == RODD else 0) | (SAT if sat else 0)
    return REC.pack(fmt, 0 if frm == RODD else frm, rec_flags, 1 if narrowing else 0, 0,
                    sum(v << (16 * l) for l, v in zip(lanes, ins)),
                    scale_word(scales),
                    sum(o << (16 * l) for l, o in zip(lanes, outs)),
                    0x00FF00FF00FF00FF if narrowing else 0xFFFFFFFFFFFFFFFF,
                    sum(e << (8 * l) for l, e in zip(lanes, excs)))


def widen_job(args):
    fmt, P, finite, frm, scales = args
    cases = [(c, s) for c in range(256) for s in scales]
    out = bytearray()
    for i in range(0, len(cases), 4):
        lanes = cases[i:i + 4]
        res = [widen(c, s, P, finite, frm) for c, s in lanes]
        out += record(fmt, frm, WIDEN, False, [c for c, _ in lanes], [s for _, s in lanes],
                      [r[0] for r in res], [r[1] for r in res])
    return bytes(out)


def narrow_job(args):
    fmt, P, finite, frm, sat, scales, lo, hi = args
    out = bytearray()
    for b in range(lo, hi):
        for i in range(0, len(scales), 4):
            ss = scales[i:i + 4]
            vals = [(b + l) & 0xFFFF for l in range(len(ss))]
            res = [narrow(v, s, P, finite, frm, sat) for v, s in zip(vals, ss)]
            out += record(fmt, frm, NARROW, sat, vals, ss, [r[0] for r in res], [r[1] for r in res])
    return bytes(out)


def jobs(finite, block):
    modes = list(RND)
    if not block:
        one = [ONE] * 4
        yield widen_job, [(fmt, P, finite, frm, [ONE]) for fmt, P in FMTS for frm in modes]
        yield narrow_job, [(fmt, P, finite, frm, sat, one, 0, 1 << 16)
                           for fmt, P in FMTS for frm in modes + [RODD] for sat in (False, True)]
        return
    yield widen_job, [(fmt, P, finite, frm, list(range(256))) for fmt, P in FMTS for frm in modes]
    yield narrow_job, [(fmt, P, finite, 0, False, list(range(256)), lo, lo + 2048)
                       for fmt, P in FMTS for lo in range(0, 1 << 16, 2048)]
    yield narrow_job, [(fmt, P, finite, frm, sat, list(SPREAD), 0, 1 << 16)
                       for fmt, P in FMTS for frm in modes + [RODD] for sat in (False, True)
                       if (frm, sat) != (0, False)]


if __name__ == "__main__":
    if len(sys.argv) != 4:
        sys.exit("usage: conv_vectors.py <outdir> <finite: 0|1> <block: 0|1>")
    outdir, finite, block = sys.argv[1], sys.argv[2] == "1", sys.argv[3] == "1"
    os.makedirs(outdir, exist_ok=True)
    path = os.path.join(outdir, "conv.bin")
    n = 0
    with Pool(os.cpu_count() or 1) as pool, open(path, "wb") as f:
        for fn, args in jobs(finite, block):
            for chunk in pool.imap(fn, args):
                f.write(chunk)
                n += len(chunk)
    print(f"{path}: {n // REC.size} cycles")
