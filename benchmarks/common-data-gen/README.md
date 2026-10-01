# Reference models for the FP8 benchmarks

## Scope

Two benchmarks test Saturn's 8-bit floating point end to end, running a
program on the simulated chip and comparing its results with expected values:

| Benchmark | Tests |
|---|---|
| `vec-mx-unary` | Conversions to and from 8-bit formats (BF16 ↔ FP8), every rounding mode, with and without saturation |
| `vec-mx-binary` | Multiply, add and subtract on FP8, FP16 and BF16, every rounding mode |

The expected values in each benchmark's `data.S` are computed by the Python
reference models in this directory. Each array covers a few hundred inputs
chosen at the edges (overflow threshold, top binade, subnormal boundary, exact
ties), with the same inputs repeated for every rounding mode.

The same benchmarks run on two kinds of build:

| Standard | 8-bit formats (altfmt = 0 / 1) | Simulator config |
|---|---|---|
| `ocp` (default) | OCP E4M3 / E5M2 | `MXV256D128ShuttleConfig` |
| `p3109` | IEEE P3109 binary8p4 / binary8p3 | `P3109V256D128ShuttleConfig` |
| `p3109-finite` | the same, finite domain (no infinities) | `P3109FiniteV256D128ShuttleConfig` |

The checked-in `data.S` files are for `ocp`. Exhaustive P3109 checks, outside
the full chip, are in `generators/saturn/models/`.

## Files

In this directory:

| File | Contents |
|---|---|
| `gfloat_ref.py` | Conversions, via the [gfloat](https://github.com/graphcore-research/gfloat) library |
| `fma_ref.py` | Arithmetic computed exactly, then rounded once (gfloat has no arithmetic) |
| `requirements.txt` | Python packages for the models |

In each benchmark directory (`vec-mx-unary/`, `vec-mx-binary/`):

| File | Contents |
|---|---|
| `main.c` | The benchmark |
| `data.S` | Inputs and expected outputs, generated |
| `gen_data/gen_data.py` | Writes `data.S` |
| `gen_data/validate_*.py` | Checks the model against the original Spike output |
| `data.S.spike-golden` | That Spike output, kept because it can no longer be regenerated |
| `run_baseline.sh` | Shortcut for `../run_fp8_test.sh <this benchmark>` |

And `benchmarks/run_fp8_test.sh` runs one benchmark end to end.

## How to run

All commands below are run from the Chipyard root.

### 1. Set up (once)

gfloat needs Python 3.10 or newer (it declares 3.8.1 but uses `match`), in its
own environment:

```bash
python3.12 -m venv ~/venvs/gfloat
~/venvs/gfloat/bin/pip install -r generators/saturn/benchmarks/common-data-gen/requirements.txt
```

The scripts look for it at `~/venvs/gfloat/bin/python`. To use another
interpreter, set `GFLOAT_PYTHON` to it.

Build the simulator for the standard you want to test, from the table above:

```bash
source env.sh
make -C sims/verilator CONFIG=MXV256D128ShuttleConfig
```

### 2. Run a benchmark

```bash
generators/saturn/benchmarks/run_fp8_test.sh vec-mx-unary                 # ocp
generators/saturn/benchmarks/run_fp8_test.sh vec-mx-binary p3109          # P3109
generators/saturn/benchmarks/run_fp8_test.sh vec-mx-unary p3109-finite    # P3109, finite domain
```

Each run validates the model, generates `data.S` for the chosen standard,
compiles the benchmark and runs it on the simulator: about 20 minutes for
`vec-mx-unary`, 45 for `vec-mx-binary`. Add `--gen-only` to stop before the
simulation.

Pass: the run ends with `All tests passed`; a failure prints `Test failed`
with the failing element and exits non-zero. Logs are in
`<benchmark>/results/`, with `latest.log` pointing at the most recent.

A `p3109` or `p3109-finite` run leaves its vectors in the checked-in `data.S`.
Restore it afterwards with
`git checkout generators/saturn/benchmarks/vec-mx-unary/data.S` (or
`vec-mx-binary`), or by running the benchmark again with `ocp`.

### 3. Check the models only (seconds, no simulator)

```bash
~/venvs/gfloat/bin/python generators/saturn/benchmarks/vec-mx-unary/gen_data/validate_gfloat.py
~/venvs/gfloat/bin/python generators/saturn/benchmarks/vec-mx-binary/gen_data/validate_fma_ref.py
```

Pass: each ends with a `PASS:` line. These compare against the OCP Spike data,
the only golden data there is; the P3109 references are checked in
`generators/saturn/models/`.

### 4. Regenerate the checked-in vectors

```bash
~/venvs/gfloat/bin/python generators/saturn/benchmarks/vec-mx-unary/gen_data/gen_data.py -n 256 -o generators/saturn/benchmarks/vec-mx-unary/data.S
~/venvs/gfloat/bin/python generators/saturn/benchmarks/vec-mx-binary/gen_data/gen_data.py -n 128 -o generators/saturn/benchmarks/vec-mx-binary/data.S
```

The generators are deterministic, so this reproduces the checked-in files
exactly. `--std p3109` generates for a P3109 build; `--summary` on
`vec-mx-binary` lists the input categories of each array.

## Background

- **Why not Spike.** Spike has no P3109 support, drove only one rounding mode,
  and computed 8-bit arithmetic through BF16, rounding twice where the
  hardware rounds once. Over all E4M3 operand pairs, that double rounding
  gives a different add result for 312 pairs under round-to-nearest-even and
  316 under round-to-nearest-max-magnitude.
- **How `fma_ref.py` rounds once.** It computes the exact result with
  `fractions.Fraction`, then hands gfloat a float that sits in the same place
  relative to the destination's neighbouring values (on one, below the
  midpoint, on it, or above it), which rounds identically.
- **`.sat`.** Cross-checking with Spike showed that the RVV `.sat` conversions
  clamp infinities as well as finite overflows, which is gfloat's `sat=True`.
- **No toolchain support needed.** `common/rvv_mx.h` emits the FP8
  instructions as raw `.insn` encodings, so `-march=rv64gcv_zfh_zvfh` works.
