# OPFMACC.S: block-scaled outer product

`OPFMACC.S` is `OPFMACC` with IEEE P3109 block scales. Each data register's
scales sit in the register after it, so the instruction encoding is the same
as `OPFMACC`'s and no new architectural state is added.

Implemented in the OPU (configs with `useMxOPU`), commit b4af0b1.

## Encoding

| Field | Bits | Value |
|---|---|---|
| funct6 | 31:26 | `100110` (`OPFMACC` is `100101`) |
| vm | 25 | `1`; `0` is reserved |
| vs2 | 24:20 | B data register, even |
| vs1 | 19:15 | A data register, even |
| funct3 | 14:12 | `001` (OPFVV) |
| vd | 11:7 | tile `md`, 0 to 3 |
| opcode | 6:0 | `1010111` (OP-V) |

Assembly: `opfmacc.s md, vs2, vs1`. C: `OPFMACCS(md, vs2, vs1)` in `benchmarks/common/bme.h`.

## Operands

With `R = VLEN/8` (32 at VLEN 256):

| Register | Contents |
|---|---|
| `vs1` | `a[0..R-1]`: column k of A, one 8-bit value per tile row |
| `vs1+1` | `sA[0..R-1]`: the row scales, one byte per tile row |
| `vs2` | `b[0..R-1]`: row k of B, one 8-bit value per tile column |
| `vs2+1` | `sB[0..R-1]`: the column scales, one byte per tile column |
| tile `md` | `C[0..R-1][0..R-1]`, binary32 accumulators |

As for `OPFMACC`:

- `vtype.altfmt` selects the format of `a` and `b`: 0 is binary8p4 and 1 is binary8p3 in a P3109 build; 0 is E4M3 and 1 is E5M2 in an OCP build.
- `vtype` must have SEW = 8 and LMUL = 1. Other settings are reserved.
- `vl` is ignored: the whole tile is updated.

## Scale format

Each scale is P3109 Binary8p1uf (8-bit, unsigned, finite, precision 1, bias 128):

| Code | Value |
|---|---|
| 0 | 0 |
| 1 to 254 | 2^(code − 128) |
| 255 | NaN |

## Operation

```
for i in 0..R-1, j in 0..R-1:
    x = decode(sA[i]) × a[i]        # exact
    y = decode(sB[j]) × b[j]        # exact
    C[i][j] = round(C[i][j] + x × y)
```

`x × y + C[i][j]` is computed exactly, then rounded once to binary32:

- The rounding is always round-to-nearest, ties to even, whatever `frm` holds.
- A result beyond binary32's largest finite value becomes ±Inf.
- Results below binary32's normal range round to subnormals.
- Exception flags are not recorded: `fflags` is unchanged.

Special values:

- A NaN anywhere (scale code 255, NaN `a` or `b`, NaN accumulator) gives NaN.
- A zero scale times an infinite `a` or `b` gives NaN; a zero scale times a finite value gives zero.
- Other infinities and signed zeros follow IEEE 754 multiplication and addition.
- Every NaN result is the canonical binary32 NaN, `0x7FC00000`.

With both scales 2^0 (code 128), `OPFMACC.S` computes exactly what `OPFMACC` computes.

## Constraints

- An odd `vs1` or `vs2` is reserved. Saturn does not trap it.
- `vs1 = vs2` is allowed: the A and B scales are then the same register.
- `OPFMACC.S` reads four vector registers (`vs1`, `vs1+1`, `vs2`, `vs2+1`). Ordering with other instructions follows the usual program-order rules for vector register sources.
- `v0`/`v1` may hold an operand, since the instruction is unmasked. A masked form would exclude this pair.

## Use with block-scaled formats

- **Block size.** Each instruction is one step k. Software reloads `vs1+1` and `vs2+1` every B steps, so any block size B works. The hardware does not see B.
- **Scale layout.** The scales of one block for `R` consecutive rows must be contiguous in memory, packed like A, so that one unit-stride load fills a scale register. This is a performance requirement, not a correctness one: a strided scale load measured +42% per block.
- **Coarser scales.** A coarser scale, for example one per 128×128 weight block, is written into all `R` bytes of the scale register.

## Relation to IEEE P3109

- Each per-element update is P3109's ScaledMultiply followed by an exact addition and one rounding, which is BlockFMA with block size 1 (Interim Report v4.0.3, §5.7–5.8).
- A sequence of `OPFMACC.S` over the k steps of a block rounds after every step. It is therefore an approximate implementation of BlockDotProduct (§5.6.2), which rounds once per block. Such an implementation must declare its accuracy κ (§4.4, Annex D.2).
- Declared κ, for B instructions on a cleared tile, with T = Σ|p_k| over the block's products p_k:

  | Inputs | κ |
  |---|---|
  | Finite elements, no NaN scale, nonzero products of one sign each ≥ 2^−126, T·(1 + γ_B) below binary32's overflow threshold | 2B |
  | Other finite inputs that cannot overflow | 2^32 − 2^24 − 1 |
  | All other inputs | NaN |

  The NaN row exists because partial sums can overflow when the exact sum does not. `models/opu_kappa.py` derives and measures this: the same-sign maximum is 3 at B = 32 for random blocks, and 15 for a constructed worst case.

## Saturn implementation

None of this is architectural:

- **Hazards.** The frontend gives `OPFMACC.S` EMUL 2 (`PipelinedFaultCheck.scala`). Every hazard check, in the issue queues and in the OPU sequencer, then covers `vs1+1` and `vs2+1`.
- **Scale cache.** The OPU has two register-file read ports, both used by the data. The OPU sequencer therefore keeps the last four scale registers it read (`OuterProductSequencer.scala`):
  - A miss takes a free entry first, so a kernel with at most four scale registers never evicts.
  - Missing pieces are read through the two data ports, in fill cycles before the data reads.
  - Every register-file write is reported (`RegisterFile.write_snoop`) and drops the written register's cached copy.
- **Cell.** Each cell (`OuterProductUnit.scala`) works as follows:
  1. It widens `a`, `b` and the accumulator to a recoded format with a 10-bit exponent, where a scaled operand cannot overflow.
  2. It adds the scales to the exponents of `a` and `b`.
  3. It rounds the unrounded `a × b + c` once, to binary32.
- **Cost, measured on the 2×2 kernel.**
  - `OPFMACC.S` itself adds no cycles over `OPFMACC`.
  - Reloading the scales costs +1.0% to +2.9% per 32-step block.
  - +3.9% OPU area in Sky130 synthesis.
  - The OPU's critical path stays below the vector FMA's.
- **Tests.** `models/run_opu_check.sh` checks one cell against an exact reference: 2,228,224 multiply-accumulates, 786,432 of them scaled. `benchmarks/opu-scaled-gemm` checks three 64×64×96 scaled GEMMs bit for bit, on the P3109, P3109-finite and OCP configs: block reloads, scale-cache eviction on every instruction, and `vs1 = vs2`.
