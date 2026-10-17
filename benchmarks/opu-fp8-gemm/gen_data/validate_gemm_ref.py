#!/usr/bin/env python3
"""Check gen_data.py's GEMM reference against the Spike-generated data.S.

  ./validate_gemm_ref.py [data.S]   (default: ../data.S)

Recomputes every expected C array of the checked-in OCP data.S from its A and
B arrays and compares bit for bit.
"""

import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from gen_data import M, N, K, MODES, gemm  # noqa: E402
from gfloat_ref import FP8_STANDARDS  # noqa: E402


def read_data_s(path):
    """The arrays of a data.S file: {label: [32-bit words]}."""
    arrays, cur = {}, None
    with open(path) as f:
        for line in f:
            m = re.match(r"^(\w+):\s*$", line)
            if m:
                cur = arrays.setdefault(m.group(1), [])
                continue
            m = re.match(r"\s+\.word\s+0x([0-9A-Fa-f]+)", line)
            if m and cur is not None:
                cur.append(int(m.group(1), 16))
    return arrays


def main():
    path = sys.argv[1] if len(sys.argv) > 1 else os.path.join(os.path.dirname(__file__), "..", "data.S")
    d = read_data_s(path)
    assert (d["M"][0], d["N"][0], d["K"][0]) == (M, N, K), "M, N, K differ from gen_data.py"
    bad = 0
    for mode in MODES:
        for fname, alt in (("e4m3", "altfmt0"), ("e5m2", "altfmt1")):
            name = f"{fname}_rand_{mode}"
            at = [(w >> (8 * j)) & 0xFF for w in d[name + "_at"] for j in range(4)]
            b = [(w >> (8 * j)) & 0xFF for w in d[name + "_b"] for j in range(4)]
            want = d[name + "_c"]
            got = gemm(FP8_STANDARDS["ocp"][alt], at, b)
            diff = sum(g != w for g, w in zip(got, want))
            print(f"{name:20} {len(want) - diff}/{len(want)} agree")
            bad += diff
    if bad:
        sys.exit(f"FAIL: {bad} mismatches")
    print("PASS: the reference matches Spike on every element")


if __name__ == "__main__":
    main()
