#!/usr/bin/env python3
"""Cross-check the FMA reference model (fma_ref.py) against Spike's golden data.

data.S.spike-golden was produced by gen_data.c on Spike.  Its FP16 and BF16 results
come from the real instructions; its FP8 results were emulated through BF16, which
is exact for multiplication and all widening operations, and rounds twice for 8-bit
add/sub (a rare source of wrong golden values -- none occur in this file).

Run it before trusting fma_ref.py, and again after any change to it.

  ./validate_fma_ref.py [path/to/data.S]   # defaults to ../data.S.spike-golden
"""

import os
import re
import sys

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "common-data-gen"))

from gfloat import RoundMode  # noqa: E402
from gfloat.formats import format_info_bfloat16, format_info_binary16, format_info_binary32  # noqa: E402
from fma_ref import binary  # noqa: E402
from gfloat_ref import FP8  # noqa: E402

FORMATS = {   # name: (operand format, operand bytes, widened result format, its bytes)
    "fp16": (format_info_binary16, 2, format_info_binary32, 4),
    "bf16": (format_info_bfloat16, 2, format_info_binary32, 4),
    "e4m3": (FP8["altfmt0"], 1, format_info_bfloat16, 2),
    "e5m2": (FP8["altfmt1"], 1, format_info_bfloat16, 2),
}
OPS = ("mul", "add", "sub", "wmul", "wadd", "wsub")


def parse(path):
    arrays, cur = {}, None
    with open(path) as f:
        for line in f:
            m = re.match(r"^(\w+):\s*$", line)
            if m:
                cur = m.group(1)
                arrays[cur] = []
                continue
            m = re.match(r"\s+\.word\s+(0x[0-9A-Fa-f]+)", line)
            if m and cur:
                arrays[cur].append(int(m.group(1), 16))
    return arrays


def unpack(words, esize):
    per = 4 // esize
    return [(w >> (8 * esize * j)) & ((1 << (8 * esize)) - 1) for w in words for j in range(per)]


def main():
    path = sys.argv[1] if len(sys.argv) > 1 else \
        os.path.join(os.path.dirname(__file__), "..", "data.S.spike-golden")
    arrays = parse(path)
    n = arrays["N"][0]
    print(f"fma_ref vs Spike, {os.path.relpath(path)}, {n} elements per array\n")
    checked = failures = 0
    for fname, (fi, esz, wfi, wesz) in FORMATS.items():
        for op in OPS:
            name = f"{fname}_{op}"
            if name + "_out" not in arrays:
                print(f"  {name:12} SKIPPED (not present)")
                continue
            wide = op.startswith("w")
            dst, desz = (wfi, wesz) if wide else (fi, esz)
            a = unpack(arrays[name + "_a"], esz)[:n]
            b = unpack(arrays[name + "_b"], esz)[:n]
            want = unpack(arrays[name + "_out"], desz)[:n]
            got = [binary(op[1:] if wide else op, fi, dst, x, y, RoundMode.TiesToEven, check=True) for x, y in zip(a, b)]
            bad = [i for i in range(n) if got[i] != want[i]]
            checked += 1
            failures += bool(bad)
            status = "ok" if not bad else f"MISMATCH at {bad[:5]}"
            print(f"  {name:12} {n - len(bad):3}/{n}  {status}")
    expected = len(FORMATS) * len(OPS)
    if checked < expected:
        sys.exit(f"\nFAILED: only {checked}/{expected} arrays present")
    if failures:
        sys.exit(f"\nFAILED: {failures} array(s) disagree with Spike")
    print(f"\nPASS: fma_ref agrees with Spike on all {checked} arrays.")


if __name__ == "__main__":
    main()
