"""Write the expected P3109 code and exception flags of every conversion.

Two bytes per case, code then flags (NV DZ OF UF NX in bits 4..0, the order of
the rounder's exceptionFlags), in this order -- the testbench walks the same
loops:

    for domain in (extended, finite):          # a separate file each
      for sat in (0, 1):
        for altfmt in (0, 1):                  # binary8p4, binary8p3
          for mode in (0, 1, 2, 3, 4, 6):      # RNE RTZ RDN RUP RMM, round-to-odd
            for bits in 0 .. 65535:            # every BF16 pattern

Codes come from gfloat (rto_ref for round-to-odd) and flags from flags_ref,
not from the rounder model, so the RTL is compared against the references.
"""
import os
import sys
from multiprocessing import Pool

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "benchmarks", "common-data-gen"))
sys.path.insert(0, HERE)

from gfloat_ref import BF16, FRM, convert, p3109_format             # noqa: E402
from flags_ref import flags, pack, bf16_exact, bf16_is_snan, FRM_MODES, RODD  # noqa: E402
from rto_ref import convert_bf16_odd                  # noqa: E402

MODES = FRM_MODES + (RODD,)
RND = {frm: rnd for frm, rnd in FRM.values()}


def job(args):
    finite, sat, P, mode = args
    fi = p3109_format(P, finite)
    out = bytearray()
    for bits in range(1 << 16):
        code = (convert_bf16_odd(bits, fi, sat) if mode == RODD
                else convert(BF16, fi, bits, RND[mode], sat))
        exc = flags(bf16_exact(bits), fi, mode, invalid=bf16_is_snan(bits))
        out += bytes((code, pack(exc)))
    return bytes(out)


if __name__ == "__main__":
    out_dir = sys.argv[1] if len(sys.argv) > 1 else "."
    with Pool(os.cpu_count() or 1) as pool:
        for domain, finite in (("ext", False), ("fin", True)):
            jobs = [(finite, sat, P, mode) for sat in (False, True) for P in (4, 3) for mode in MODES]
            path = os.path.join(out_dir, f"expected_{domain}.bin")
            with open(path, "wb") as f:
                for chunk in pool.imap(job, jobs):
                    f.write(chunk)
            print(f"wrote {path}  ({len(jobs) * 65536} cases)")
