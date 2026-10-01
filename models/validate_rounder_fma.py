"""Check the rounder model on FMA-shaped inputs, against fma_ref.

The FMA hands the rounder a raw result whose significand can be in [2,4)
(hardfloat's doShiftSigDown1), in the shape of whichever core the lane runs on.

  1. every operand pair x {mul, add, sub} x both formats, on all five core
     shapes, RNE;
  2. selected pairs x five modes x both domains x SatNone/SatFinite, with the
     significand presented in [1,2) and in [2,4).
"""
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "benchmarks", "common-data-gen"))
sys.path.insert(0, HERE)

from gfloat_ref import FRM, p3109_format  # noqa: E402
from fma_ref import OPS, exact, project, binary_inputs  # noqa: E402
from p3109_rounder import p3109_round  # noqa: E402
from fma_raw import CORES, raw_from_exact  # noqa: E402


def model(r, fmt, core, mode, sat, finite, unnormalized=False):
    ew, sw = CORES[core]
    raw = raw_from_exact(r, ew, sw + 2, unnormalized)
    return p3109_round(raw, fmt, mode, sat, finite,
                       in_exp_width=ew, in_sig_width=sw + 2,
                       sig_msb_always_zero=False)[0]


FMTS = {"p4": 4, "p3": 3}
OPNAMES = ("mul", "add", "sub")
if __name__ == "__main__":
    fail = 0

    # --- 1. cross-core agreement, exhaustive ----------------------------------
    print("1. every operand pair, every op, both formats, all five cores  (RNE, extended)")
    for fmt, P in FMTS.items():
        fi = p3109_format(P, False)
        for op in OPNAMES:
            bad, n = 0, 0
            for a in range(256):
                xa = exact(fi, a)
                for b in range(256):
                    r = OPS[op](xa, exact(fi, b), 0)
                    want = project(fi, r, FRM["rne"][1], False)
                    n += 1
                    for core in CORES:
                        got = model(r, fmt, core, 0, False, False)
                        if got != want:
                            bad += 1
                            if bad == 1:
                                print(f"   MISMATCH {fmt} {op} core={core} "
                                      f"a=0x{a:02X} b=0x{b:02X} "
                                      f"got 0x{got:02X} want 0x{want:02X}")
            fail += bad
            print(f"   {fi.name} {op:4}: {n} pairs x 5 cores, {bad} mismatches", flush=True)

    # --- 2. modes, domains, saturation on selected pairs ----------------------
    print("\n2. selected operand pairs x 5 modes x 2 domains x sat on/off x 5 cores")
    for finite in (False, True):
        for fmt, P in FMTS.items():
            fi = p3109_format(P, finite)
            for op in OPNAMES:
                pairs, _cats = binary_inputs(op, fi, fi, 512, seed=1)
                bad = 0
                for a, b in pairs:
                    for name, (frm, rnd) in FRM.items():
                        r = OPS[op](exact(fi, a), exact(fi, b), frm)
                        for sat in (False, True):
                            want = project(fi, r, rnd, sat)
                            for core in CORES:
                                # both normalisations, against the reference: this is
                                # where doShiftSigDown1 gets checked against truth
                                for shifted in (False, True):
                                    got = model(r, fmt, core, frm, sat, finite, shifted)
                                    if got != want:
                                        bad += 1
                                        if bad == 1:
                                            print(f"   MISMATCH {fi.name} {op} {name} "
                                                  f"sat={sat} core={core} shifted={shifted} "
                                                  f"a=0x{a:02X} b=0x{b:02X} "
                                                  f"got 0x{got:02X} want 0x{want:02X}")
                fail += bad
                print(f"   {fi.name} {op:4}: {len(pairs)} pairs, {bad} mismatches", flush=True)

    print("\nTOTAL MISMATCHES:", fail)
    sys.exit(1 if fail else 0)
