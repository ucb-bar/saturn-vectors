#!/usr/bin/env python3
"""Cross-check the gfloat reference model against Spike.

The committed vec-mx-unary/data.S was produced by Spike with
--isa=..._zvfofp8min, so it is an independent golden model for the OCP FP8
conversions.  This replays its inputs through gfloat and compares outputs.

Run it before trusting gfloat as the reference, and again after any gfloat
upgrade.  It needs no RISC-V toolchain and no simulator -- just the committed
data.S -- so it works as a plain CI check.

  ./validate_gfloat.py [path/to/data.S]   # defaults to ../data.S.spike-golden
"""

import math
import os
import re
import sys

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "common-data-gen"))

from gfloat import decode_float  # noqa: E402
from gfloat_ref import BF16, FP8, convert  # noqa: E402

E4M3 = FP8["altfmt0"]
E5M2 = FP8["altfmt1"]

# name, src format, src bytes, dst format, dst bytes, gfloat sat flag
CASES = [
    ("e4m3_narrow",     BF16, 2, E4M3, 1, False),
    ("e5m2_narrow",     BF16, 2, E5M2, 1, False),
    ("e4m3_narrow_sat", BF16, 2, E4M3, 1, True),
    ("e5m2_narrow_sat", BF16, 2, E5M2, 1, True),
    ("e4m3_widen",      E4M3, 1, BF16, 2, False),
    ("e5m2_widen",      E5M2, 1, BF16, 2, False),
]


def parse(path):
    arrays, cur = {}, None
    with open(path) as f:
        for line in f:
            m = re.match(r"^\.global\s+(\S+)", line)
            if m:
                cur = m.group(1)
                arrays[cur] = []
                continue
            m = re.match(r"^\s+\.word\s+0x([0-9A-Fa-f]+)", line)
            if m and cur:
                arrays[cur].append(int(m.group(1), 16))
    return arrays


def unpack(words, esize):
    per = 4 // esize
    mask = (1 << (esize * 8)) - 1
    return [(w >> (j * esize * 8)) & mask for w in words for j in range(per)]


def main():
    # Default to the Spike-generated file, not data.S: data.S is now produced by
    # gen_data.py from gfloat, so comparing against it would be circular.
    path = sys.argv[1] if len(sys.argv) > 1 else \
        os.path.join(os.path.dirname(__file__), "..", "data.S.spike-golden")
    arrays = parse(path)
    if "N" not in arrays:
        sys.exit(f"no arrays parsed from {path}")
    n = arrays["N"][0]

    print(f"gfloat vs Spike, {os.path.relpath(path)}, {n} elements per array\n")
    failures = 0
    checked = 0
    for name, sfi, ssz, dfi, dsz, sat in CASES:
        if name not in arrays:
            print(f"  {name:22} SKIPPED (not present)")
            continue
        checked += 1
        inp = unpack(arrays[name], ssz)[:n]
        exp = unpack(arrays[name + "_out"], dsz)[:n]
        bad = [(i, b, e, convert(sfi, dfi, b, sat=sat))
               for i, (b, e) in enumerate(zip(inp, exp))
               if e != convert(sfi, dfi, b, sat=sat)]
        failures += len(bad)
        print(f"  {name:22} {n - len(bad):3}/{n}" + ("  ok" if not bad else "  MISMATCH"))
        for i, b, e, g in bad[:5]:
            v = decode_float(sfi, b).fval
            print(f"       [{i:3}] in 0x{b:0{ssz * 2}X} ({v:>14.7g})"
                  f"  spike 0x{e:0{dsz * 2}X}  gfloat 0x{g:0{dsz * 2}X}")

    print()
    if checked == 0:
        sys.exit("FAILED: no comparable arrays found -- wrong data.S?")
    if checked < len(CASES):
        sys.exit(f"FAILED: only {checked}/{len(CASES)} arrays present")
    if failures:
        sys.exit(f"FAILED: {failures} mismatches -- do not trust gfloat as reference yet")
    print("PASS: gfloat agrees with Spike on every OCP FP8 conversion.")


if __name__ == "__main__":
    main()
