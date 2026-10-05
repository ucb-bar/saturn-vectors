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
import sys

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "common-data-gen"))
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from gfloat import RoundMode  # noqa: E402
from fma_ref import binary  # noqa: E402
from gfloat_ref import read_data_s, unpack_words  # noqa: E402
from gen_data import FORMATS, OPS  # noqa: E402  the arrays the generator writes




def main():
    path = sys.argv[1] if len(sys.argv) > 1 else \
        os.path.join(os.path.dirname(__file__), "..", "data.S.spike-golden")
    arrays = read_data_s(path)
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
            a = unpack_words(arrays[name + "_a"], esz)[:n]
            b = unpack_words(arrays[name + "_b"], esz)[:n]
            want = unpack_words(arrays[name + "_out"], desz)[:n]
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
