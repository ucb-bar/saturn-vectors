"""Check the rounder model's codes against gfloat.

Every BF16 pattern x the five frm modes x {binary8p4, binary8p3} x
{extended, finite} x {SatNone, SatFinite}.
"""
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "benchmarks", "common-data-gen"))
sys.path.insert(0, HERE)

from gfloat_ref import BF16, FRM, convert, p3109_format  # noqa: E402
from p3109_rounder import convert_bf16  # noqa: E402

if __name__ == "__main__":
    total_bad = 0
    for finite in (False, True):
        for fmt, P in (("p4", 4), ("p3", 3)):
            fi = p3109_format(P, finite)
            for sat in (False, True):
                for name, (frm, rnd) in FRM.items():
                    bad = []
                    for b in range(1 << 16):
                        got = convert_bf16(b, fmt, frm, sat, finite)
                        want = convert(BF16, fi, b, rnd, sat)
                        if got != want:
                            bad.append((b, got, want))
                    total_bad += len(bad)
                    extra = ""
                    if bad:
                        extra = "   e.g. bf16 0x%04X got 0x%02X want 0x%02X (%d cases)" % (
                            bad[0][0], bad[0][1], bad[0][2], len(bad))
                    print(f"{fi.name:16} {'sat' if sat else '   '} {name}: "
                          f"{65536 - len(bad):5}/65536{extra}", flush=True)

    print("\nTOTAL MISMATCHES:", total_bad)
    sys.exit(1 if total_bad else 0)
