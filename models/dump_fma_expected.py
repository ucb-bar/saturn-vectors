"""Write raw-number test vectors for the FMA's 8-bit rounder, one file per core.

An 8-bit multiply-add can run on any of the five FMA core sizes (FP64/FP32,
FP16, BF16, E5M3 -- see ftype_used_for in FPFMAPipe.scala), and each hands the
rounder a RawFloat of its own shape. So every case below is presented to every
core, built exactly as that core would build it (raw_from_exact), in both
normalisations: significand in [1,2), and the same value one binade lower with
the significand in [2,4) -- the doShiftSigDown1 branch only the FMA can reach.

The expected code comes from fma_ref.project and the expected exception flags
from flags_ref -- exact rational references, not the Python rounder model.

Cases, per domain:
  A. every operand pair, x {mul, add, sub}, x {binary8p4, binary8p3}, RNE
  B. fma_ref's selected pairs (binary_inputs, 512 per op), x all 5 modes

sat is always off: RVV has no saturating multiply-add.

Record, 16 bytes little-endian (read by tb_p3109_fma_round.cpp):
  u8  flags   isNaN | isInf<<1 | isZero<<2 | sign<<3 | altfmt<<4
  u8  rm      rounding mode, RISC-V frm encoding
  u8  want    expected 8-bit code
  u8  wantExc expected exception flags, NV DZ OF UF NX in bits 4..0
  i32 sExp
  u64 sig
"""
import os
import struct
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "benchmarks", "common-data-gen"))
sys.path.insert(0, HERE)

from gfloat_ref import FRM, p3109_format                            # noqa: E402
from fma_ref import OPS, exact, project, binary_inputs  # noqa: E402
from fma_raw import CORES, raw_from_exact             # noqa: E402
from flags_ref import flags, pack                     # noqa: E402

REC = struct.Struct("<BBBBiQ")
FMTS = (("p4", 4, 0), ("p3", 3, 1))        # (name, precision, altfmt)
RNE_FRM, RNE_RND = FRM["rne"]


def cases(fi):
    """Yield (frm, exact result, expected code) for one format."""
    for op in ("mul", "add", "sub"):
        xs = [exact(fi, c) for c in range(256)]
        for a in range(256):
            for b in range(256):
                r = OPS[op](xs[a], xs[b], RNE_RND)
                yield RNE_FRM, r, project(fi, r, RNE_RND, False)
    for op in ("mul", "add", "sub"):
        pairs, _ = binary_inputs(op, fi, fi, 512, seed=1)
        for a, b in pairs:
            for frm, rnd in FRM.values():
                r = OPS[op](exact(fi, a), exact(fi, b), rnd)
                yield frm, r, project(fi, r, rnd, False)


if __name__ == "__main__":
    out_dir = sys.argv[1] if len(sys.argv) > 1 else "."
    for dom_name, finite in (("ext", False), ("fin", True)):
        bufs = {core: bytearray() for core in CORES}
        for fmt, P, altfmt in FMTS:
            fi = p3109_format(P, finite)
            n = 0
            for frm, r, want in cases(fi):
                n += 1
                want_exc = pack(flags(r, fi, frm))
                for core, (ew, sw) in CORES.items():
                    for shifted in (False, True):
                        raw = raw_from_exact(r, ew, sw + 2, shifted)
                        raw_bits = (raw["isNaN"] | raw["isInf"] << 1 | raw["isZero"] << 2
                                 | raw["sign"] << 3 | altfmt << 4)
                        bufs[core] += REC.pack(raw_bits, frm, want, want_exc, raw["sExp"], raw["sig"])
            print(f"  {dom_name} {fmt}: {n} cases x {len(CORES)} cores x 2 normalisations",
                  flush=True)
        for core, buf in bufs.items():
            path = os.path.join(out_dir, f"fma_{core}_{dom_name}.bin")
            with open(path, "wb") as f:
                f.write(buf)
            print(f"wrote {path}  ({len(buf) // REC.size} records)", flush=True)
