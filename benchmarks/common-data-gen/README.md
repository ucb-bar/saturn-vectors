# Reference models for the FP8 benchmarks

`vec-mx-unary` (conversions) and `vec-mx-binary` (FMA add / sub / mul) took
their expected values from **Spike**, via `gen_data.c` + `gen_data.sh`. Both now
generate their vectors from a reference model in Python instead:

| Benchmark | Model | Validator |
|---|---|---|
| `vec-mx-unary` | [gfloat](https://github.com/graphcore-research/gfloat) | `gen_data/validate_gfloat.py` |
| `vec-mx-binary` | `fma_ref.py` (exact arithmetic, gfloat for the rounding) | `gen_data/validate_fma_ref.py` |

Two things this buys:

* **Every rounding mode.** The Spike flow drove one mode, so directed rounding
  was never checked. Each array is now emitted five times, once per `frm`, with
  identical operands, so a mismatch isolates the mode.
* **Inputs chosen for the edges** — the overflow threshold, the top binade, the
  subnormal boundary, exact ties, values that round to zero — instead of
  uniform random values in a narrow range.

The original Spike output is preserved in each benchmark as
`data.S.spike-golden`, because it cannot be regenerated once the vectors change.
The validators check the models against it.

## Setup

gfloat declares `requires_python >= 3.8.1` but uses `match` statements and
**actually needs 3.10+**; it imports and then fails with a `SyntaxError` in
`round.py`. The default conda env is older, so use a separate venv:

```bash
python3.12 -m venv ~/venvs/gfloat
~/venvs/gfloat/bin/pip install -r generators/saturn/benchmarks/common-data-gen/requirements.txt
```

## Trusting the models

Both validators run in a second, need no RISC-V toolchain and no simulator, so
they work as plain CI checks:

```bash
cd generators/saturn/benchmarks
~/venvs/gfloat/bin/python vec-mx-unary/gen_data/validate_gfloat.py
~/venvs/gfloat/bin/python vec-mx-binary/gen_data/validate_fma_ref.py
```

```
  e4m3_narrow            128/128  ok
  ...
  PASS: gfloat agrees with Spike on every OCP FP8 conversion.

  PASS: fma_ref agrees with Spike on all 24 arrays.
```

One thing the conversion validator established empirically: the RVV `.sat`
instruction variant clamps **infinities** as well as finite out-of-range
values, which is gfloat's `sat=True`. The plain variant is `sat=False`.

### Why the FMA needs its own model

gfloat rounds a value into a format; it has no arithmetic. Spike could only
produce 8-bit arithmetic by computing in BF16 and narrowing, which **rounds
twice**. The hardware rounds once: `MulAddRecFNPipeUnrounded` hands out an
unrounded result and the 8-bit rounding happens after it.

The two differ. Over all 65,536 E4M3 operand pairs, double rounding gives a
different answer from correct rounding for **312 add pairs** under
round-to-nearest-even and 316 under round-to-nearest-max-magnitude. (E5M2 and
the directed modes are unaffected.)

So `fma_ref.py` computes `a * b` or `a ± b` exactly with `fractions.Fraction`,
then rounds once via gfloat. Since `round_float` will not accept a `Fraction`,
it is handed a float placed at the same position relative to the destination's
neighbouring grid points — exact, below the midpoint, on it, or above it — which
rounds identically.

## Regenerating the vectors

```bash
cd generators/saturn/benchmarks
~/venvs/gfloat/bin/python vec-mx-unary/gen_data/gen_data.py  -n 256 -o vec-mx-unary/data.S
~/venvs/gfloat/bin/python vec-mx-binary/gen_data/gen_data.py -n 128 -o vec-mx-binary/data.S
```

Those are the counts the checked-in files use. `gen_data.py --summary` on the
binary benchmark prints which input categories each array covers.

## Running a benchmark end to end

`run_fp8_test.sh` validates the model, regenerates the vectors, cross-compiles
and runs the simulator:

```bash
cd generators/saturn/benchmarks
./run_fp8_test.sh vec-mx-unary
./run_fp8_test.sh vec-mx-binary --gen-only   # stop before the simulation
```

It needs a simulator built for a config with `useMxConversion` /`useMxFPFMA`:

```bash
make -C $CHIPYARD/sims/verilator CONFIG=MXV256D128ShuttleConfig
```

Runs are logged under `<benchmark>/results/`, with `latest.log` pointing at the
most recent. Expect roughly 18 minutes for `vec-mx-unary` and 45 for
`vec-mx-binary` on Verilator.

No toolchain extension support is needed: `common/rvv_mx.h` emits the FP8
instructions as raw `.insn` encodings, so plain `-march=rv64gcv_zfh_zvfh` works.

## Scope

This is the integration tier: a few hundred vectors per operation, exercising
the full path from `vsetvli` through the vector register file to the functional
unit. It is not exhaustive and should not be the only tier — an exhaustive sweep
belongs in a chiseltest unit test driving `FPConvBlock` directly, where all
65,536 BF16 patterns take gfloat about 0.1 s to produce and every rounding and
saturation mode is reachable without going through `vtype`. That needs
chiseltest added to the `saturn` project in `build.sbt`, which it does not
currently have.
