#!/usr/bin/env python3
"""Check gen_data.py's FP16 expected results against Spike.

  ./validate_spike.py      (needs riscv64-unknown-elf-gcc, spike and pk: source env.sh)

Spike computes with Berkeley softfloat, an implementation independent of
fma_ref. For every FP16 array, each element is recomputed with the scalar
Zfh/F instruction of the same meaning (fmadd.h, fnmadd.h, ...; widening forms
convert to FP32 first, which is exact) in all five rounding modes, and
compared bit for bit. Spike has no scalar BF16 or 8-bit arithmetic, so FP16
stands in for the shared reference code.
"""

import os
import subprocess
import sys
import tempfile

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from gen_data import COUNT, OPS, tests  # noqa: E402

NARROW = {   # op -> C expression on half-precision registers a, b, c
    "macc": "fmadd.h %0, %1, %2, %3",  "nmacc": "fnmadd.h %0, %1, %2, %3",
    "msac": "fmsub.h %0, %1, %2, %3",  "nmsac": "fnmsub.h %0, %1, %2, %3",
    "madd": "fmadd.h %0, %1, %3, %2",  "mul_vf": "fmul.h %0, %1, %2",
    "add_vf": "fadd.h %0, %1, %2",     "rsub_vf": "fsub.h %0, %2, %1",
    "macc_vf": "fmadd.h %0, %1, %2, %3",
}
WIDE = {     # op -> on single-precision registers, a and b converted from half
    "wmacc": "fmadd.s %0, %1, %2, %3", "wmacc_vf": "fmadd.s %0, %1, %2, %3",
    "wmul_vf": "fmul.s %0, %1, %2",    "wadd_vf": "fadd.s %0, %1, %2",
}
assert set(NARROW) | set(WIDE) == set(OPS)

PRELUDE = r"""
#include <stdio.h>
#include <stdint.h>
static uint32_t narrow(int op, uint32_t a, uint32_t b, uint32_t c) {
    uint64_t r; register double fa, fb, fc, fr;
    asm volatile("fmv.h.x %0, %1" : "=f"(fa) : "r"(a));
    asm volatile("fmv.h.x %0, %1" : "=f"(fb) : "r"(b));
    asm volatile("fmv.h.x %0, %1" : "=f"(fc) : "r"(c));
    switch (op) {
%NARROW%
    }
    asm volatile("fmv.x.h %0, %1" : "=r"(r) : "f"(fr));
    return r & 0xFFFF;
}
static uint32_t wide(int op, uint32_t a, uint32_t b, uint32_t c) {
    uint64_t r; register double fa, fb, fc, fr;
    asm volatile("fmv.h.x %0, %1\n\tfcvt.s.h %0, %0" : "=f"(fa) : "r"(a));
    asm volatile("fmv.h.x %0, %1\n\tfcvt.s.h %0, %0" : "=f"(fb) : "r"(b));
    asm volatile("fmv.w.x %0, %1" : "=f"(fc) : "r"(c));
    switch (op) {
%WIDE%
    }
    asm volatile("fmv.x.w %0, %1" : "=r"(r) : "f"(fr));
    return r & 0xFFFFFFFF;
}
"""


def c_source(arrays):
    def cases(table):
        return "\n".join(f'        case {i}: asm volatile("{ins}" : "=f"(fr) : "f"(fa), "f"(fb), "f"(fc)); break;'
                         for i, ins in enumerate(table.values()))
    src = PRELUDE.replace("%NARROW%", cases(NARROW)).replace("%WIDE%", cases(WIDE))
    body = []
    for k, (name, op, a, b, c) in enumerate(arrays):
        wide = op in WIDE
        idx = list((WIDE if wide else NARROW)).index(op)
        cc = c if c is not None else [0] * len(a)
        src += f"static const uint32_t a{k}[] = {{{', '.join(map(str, a))}}};\n"
        src += f"static const uint32_t b{k}[] = {{{', '.join(map(str, b))}}};\n"
        src += f"static const uint32_t c{k}[] = {{{', '.join(map(str, cc))}}};\n"
        body.append(f"""    for (int m = 0; m < 5; m++) {{
        asm volatile("fsrm %0" :: "r"(m));
        printf("{name} %d", m);
        for (int i = 0; i < {len(a)}; i++) printf(" %x", {'wide' if wide else 'narrow'}({idx}, a{k}[i], b{k}[i], c{k}[i]));
        printf("\\n");
    }}""")
    return src + "int main() {\n" + "\n".join(body) + "\n    return 0;\n}\n"


def main():
    arrays, expected = [], {}
    for name, src, dst, has_c, a, b, c, res in tests("ocp", COUNT):
        if not name.startswith("fp16_"):
            continue
        arrays.append((name, name[len("fp16_"):], a, b, c))
        for m, r in enumerate(res.values()):
            expected[(name, m)] = r
    with tempfile.TemporaryDirectory() as d:
        with open(os.path.join(d, "t.c"), "w") as f:
            f.write(c_source(arrays))
        subprocess.run(["riscv64-unknown-elf-gcc", "-O1", "-march=rv64gc_zfh", "-o", os.path.join(d, "t"),
                        os.path.join(d, "t.c")], check=True)
        out = subprocess.run(["spike", "--isa=rv64gc_zfh", "pk", os.path.join(d, "t")],
                             check=True, capture_output=True, text=True).stdout
    bad = total = 0
    for line in out.splitlines():
        f = line.split()
        if len(f) < 3 or (f[0], int(f[1])) not in expected:
            continue
        got = [int(x, 16) for x in f[2:]]
        want = expected[(f[0], int(f[1]))]
        diff = [i for i, (g, w) in enumerate(zip(got, want)) if g != w]
        total += len(want)
        bad += len(diff)
        for i in diff[:3]:
            print(f"   MISMATCH {f[0]} frm={f[1]} element {i}: spike {got[i]:x}, model {want[i]:x}")
    if total != len(expected) * COUNT:
        sys.exit(f"FAIL: Spike returned {total} of {len(expected) * COUNT} results")
    print(f"TOTAL MISMATCHES: {bad}  (of {total})")
    if bad:
        sys.exit(1)


if __name__ == "__main__":
    main()
