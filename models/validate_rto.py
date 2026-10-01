"""Check the rounder model's round-to-odd (vfncvt.rod) against rto_ref.

Every BF16 pattern x {binary8p4, binary8p3} x {extended, finite} x
{SatNone, SatFinite}. Also checks rto_ref against the defining property of
round-to-odd: an inexact result inside the finite range has an odd last bit.
"""
import math
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "benchmarks", "common-data-gen"))
sys.path.insert(0, HERE)

from gfloat import decode_float                      # noqa: E402
from gfloat_ref import BF16, p3109_format                          # noqa: E402
from rto_ref import convert_bf16_odd                 # noqa: E402
from p3109_rounder import convert_bf16, RODD        # noqa: E402

if __name__ == "__main__":
    total_bad, total = 0, 0
    for finite in (False, True):
        for fmt, P in (("p4", 4), ("p3", 3)):
            fi = p3109_format(P, finite)
            for sat in (False, True):
                bad, kinds, prop_bad = 0, {}, 0
                for b in range(1 << 16):
                    want = convert_bf16_odd(b, fi, sat)
                    got = convert_bf16(b, fmt, RODD, sat, finite)
                    total += 1

                    # the reference's own sanity: inexact and in range -> odd
                    x = decode_float(BF16, b).fval
                    y = decode_float(fi, want).fval
                    if (math.isfinite(x) and math.isfinite(y) and x != y
                            and abs(x) <= fi.max and want & 1 == 0):
                        prop_bad += 1

                    if got != want:
                        bad += 1
                        k = ("overflow" if math.isfinite(x) and abs(x) > fi.max
                             else "infinite input" if math.isinf(x) else "other")
                        kinds[k] = kinds.get(k, 0) + 1
                        if kinds[k] == 1:
                            print(f"   first {k:14} {fi.name} sat={sat}: bf16 0x{b:04X} "
                                  f"({x:g}) model 0x{got:02X} "
                                  f"({decode_float(fi, got).fval:g})  standard 0x{want:02X} ({y:g})")
                total_bad += bad
                print(f"{fi.name:14} sat={sat!s:5}: {65536 - bad:5}/65536 agree"
                      f"{'  ' + str(kinds) if kinds else ''}"
                      f"{'   REFERENCE PROPERTY FAILS: ' + str(prop_bad) if prop_bad else ''}",
                      flush=True)

    print(f"\nTOTAL DISAGREEMENTS: {total_bad} of {total}")
    sys.exit(1 if total_bad else 0)
