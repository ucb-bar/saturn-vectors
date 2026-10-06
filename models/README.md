# P3109 verification

## Scope

This directory checks Saturn's support for the IEEE P3109 8-bit formats
(binary8p4 and binary8p3, Interim Report v4.0.3), outside of any full-chip
simulation. It covers:

- the rounder `P3109Rounder` (used by the conversion unit and the FMA), alone;
- the conversion unit `FPConvBlock`, including block scale factors;
- one cell of the outer-product unit, `OuterProductCell`;
- the software helpers in `benchmarks/common/p3109.h`.

Every check is exhaustive or close to it, and compares both result codes and
exception flags. The expected values come from independent references
(gfloat and exact rational arithmetic), never from our own model of the
design.

The end-to-end tests, which run programs on the whole chip, are the
`vec-mx-unary`, `vec-mx-binary` and `opu-fp8-gemm` benchmarks; see
`benchmarks/common-data-gen/README.md`.

## Files

References (what the right answer is):

| File | Contents |
|---|---|
| `rto_ref.py` | Round-to-odd, transcribed from P3109 4.7.3–4.7.5 (gfloat has none) |
| `flags_ref.py` | Exception flags, computed exactly (IEEE 754 / RISC-V rules) |
| `block_ref.py` | Block scale conversions, from P3109 5.4–5.5 |

Model of the design:

| File | Contents |
|---|---|
| `p3109_rounder.py` | Bit-accurate Python model of `P3109Rounder.scala` |
| `fma_raw.py` | The unrounded result an FMA core hands the rounder |

Checks (each one exits non-zero on any mismatch):

| File | Checks |
|---|---|
| `validate_rounder.py` | Model codes against gfloat, every BF16 input |
| `validate_rto.py` | Model round-to-odd against `rto_ref.py` |
| `validate_flags.py` | Model overflow, underflow and inexact flags against `flags_ref.py` |
| `validate_rounder_fma.py` | Model on FMA results of every core type |
| `run_rounder_check.sh` | `P3109Rounder` RTL against the references |
| `run_conv_check.sh` | `FPConvBlock` RTL against the references |
| `run_opu_check.sh` | `OuterProductCell` RTL against the references |
| `p3109_test.c` | `p3109.h` against the standard's definitions |

Used by the `run_*.sh` scripts, not run directly:

| File | Used by | Contents |
|---|---|---|
| `dump_expected.py`, `dump_fma_expected.py` | `run_rounder_check.sh` | Expected results for the rounder |
| `tb_p3109_rounder.cpp`, `tb_p3109_fma_round.cpp` | `run_rounder_check.sh` | Verilator testbenches for the rounder |
| `conv_vectors.py` | `run_conv_check.sh` | Expected results for `FPConvBlock` |
| `tb_conv.cpp` | `run_conv_check.sh` | Verilator testbench for `FPConvBlock` |
| `opu_vectors.py` | `run_opu_check.sh` | Expected results for `OuterProductCell` |
| `tb_opu_cell.cpp` | `run_opu_check.sh` | Verilator testbench for `OuterProductCell` |
| `collect_hier.py` | all three | Lists a module's files in the generated Verilog |

The rounder wrappers that `run_rounder_check.sh` tests are in
`src/test/scala/P3109TestWrappers.scala`.

## How to run

Run all commands from the Chipyard root directory.

### 1. Set up (once)

gfloat needs Python 3.10 or newer, in its own environment:

```bash
python3.12 -m venv ~/venvs/gfloat
~/venvs/gfloat/bin/pip install -r generators/saturn/benchmarks/common-data-gen/requirements.txt
```

The scripts look for it at `~/venvs/gfloat/bin/python`. To use another
interpreter, set `GFLOAT_PYTHON` to it.

### 2. Check the model (no RTL, a few minutes)

```bash
cd generators/saturn/models
~/venvs/gfloat/bin/python validate_rounder.py
~/venvs/gfloat/bin/python validate_rto.py
~/venvs/gfloat/bin/python validate_rounder_fma.py
~/venvs/gfloat/bin/python validate_flags.py
cc -O2 -Wall -o /tmp/p3109_test p3109_test.c && /tmp/p3109_test
cd -
```

Pass: each validator ends with `TOTAL MISMATCHES: 0`, and `p3109_test` ends
with `all formats exhaustively verified`.

### 3. Check the rounder RTL (about 5 minutes)

```bash
source env.sh
generators/saturn/models/run_rounder_check.sh /tmp/p3109-rounder
```

Pass: 12 lines of `TOTAL MISMATCHES: 0`, one for the conversion rounder and
one for each of the five FMA core types, in each domain. Full logs are in
`/tmp/p3109-rounder`.

### 4. Check the conversion unit RTL (about 10 minutes per config)

For each config, generate its Verilog, then check it. For the block config:

```bash
source env.sh
make -C sims/verilator verilog CONFIG=P3109BlockV256D128ShuttleConfig
generators/saturn/models/run_conv_check.sh P3109BlockV256D128ShuttleConfig /tmp/p3109-conv
```

Do the same steps for `P3109V256D128ShuttleConfig`, `P3109FiniteV256D128ShuttleConfig`
and `P3109BlockFiniteV256D128ShuttleConfig`. A block config writes about
600 MB of vectors to the work directory.

Pass: `TOTAL MISMATCHES: 0` on the last line.

### 5. Check the outer-product unit RTL (about 1 minute per config)

Every (a, b) pair of both formats, against 11 accumulator values each:

```bash
source env.sh
make -C sims/verilator verilog CONFIG=OPUV256D128P3109ShuttleConfig
generators/saturn/models/run_opu_check.sh OPUV256D128P3109ShuttleConfig /tmp/p3109-opu
```

`OPUV256D128MxShuttleConfig` runs the same check on OCP FP8.

Pass: `TOTAL MISMATCHES: 0` on the last line.
