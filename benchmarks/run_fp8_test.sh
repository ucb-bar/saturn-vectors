#!/usr/bin/env bash
#
# Run one FP8 benchmark end to end: validate its reference model, regenerate its
# test vectors, cross-compile it, and run it on Verilator.
#
#   ./run_fp8_test.sh vec-mx-unary              # conversions
#   ./run_fp8_test.sh vec-mx-binary             # FMA add / sub / mul
#   ./run_fp8_test.sh vec-mx-binary --gen-only  # stop before the (long) simulation
#   ./run_fp8_test.sh vec-mx-unary p3109        # P3109 build (also p3109-finite)
#
# Each benchmark directory also has a run_baseline.sh that calls this script with
# its own name.
#
#   vec-mx-unary   conversions            reference: gfloat     (validate_gfloat.py)
#   vec-mx-binary  FMA add / sub / mul    reference: fma_ref.py (validate_fma_ref.py)
#
# Every run is logged to <benchmark>/results/<config>-<timestamp>.log, and
# <benchmark>/results/latest.log points at the most recent one.
#
# Environment overrides:
#   GFLOAT_PYTHON  interpreter that has gfloat installed (needs >= 3.10)
#   CONFIG         Chipyard config whose simulator binary to run (default:
#                  MXV256D128ShuttleConfig, or P3109V256D128ShuttleConfig /
#                  P3109FiniteV256D128ShuttleConfig for p3109 / p3109-finite)
#   N              elements per array (default: the generator's, as checked in)
#
# Note: do NOT add `set -u`. Chipyard's env.sh sources the conda hook, which
# reads unset variables and would abort.
set -eo pipefail

usage() {
	echo "usage: $0 <vec-mx-unary|vec-mx-binary> [ocp|p3109|p3109-finite] [--gen-only]" >&2
	exit 2
}

BENCH=${1:-}
[ $# -gt 0 ] && shift
case "$BENCH" in
	vec-mx-unary)  validator=validate_gfloat.py ;;
	vec-mx-binary) validator=validate_fma_ref.py ;;
	*) usage ;;
esac

STD=ocp
GEN_ONLY=0
for arg in "$@"; do
	case "$arg" in
		ocp|p3109|p3109-finite) STD=$arg ;;
		--gen-only) GEN_ONLY=1 ;;
		*) usage ;;
	esac
done

bmarks=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
here=$bmarks/$BENCH
cydir=$(cd "$bmarks/../../.." && pwd)

case "$STD" in
	ocp)          default_config=MXV256D128ShuttleConfig ;;
	p3109)        default_config=P3109V256D128ShuttleConfig ;;
	p3109-finite) default_config=P3109FiniteV256D128ShuttleConfig ;;
esac
CONFIG=${CONFIG:-$default_config}
GFLOAT_PYTHON=${GFLOAT_PYTHON:-$HOME/venvs/gfloat/bin/python}
sim=$cydir/sims/verilator/simulator-chipyard.harness-$CONFIG
results=$here/results
log=$results/$CONFIG-$(date +%Y%m%d-%H%M%S).log
mkdir -p "$results"

if ! "$GFLOAT_PYTHON" -c 'import gfloat' 2>/dev/null; then
	echo "error: no gfloat in $GFLOAT_PYTHON" >&2
	echo "       gfloat needs Python >= 3.10; see benchmarks/common-data-gen/README.md" >&2
	exit 1
fi

# Check before step 2 overwrites the checked-in data.S
if [ "$GEN_ONLY" = 0 ] && [ ! -x "$sim" ]; then
	echo "error: no simulator at $sim" >&2
	echo "       build it with: make -C $cydir/sims/verilator CONFIG=$CONFIG" >&2
	exit 1
fi

exec > >(tee "$log") 2>&1
trap 'ln -sfn "$(basename "$log")" "$results/latest.log"' EXIT

echo "# $BENCH / $STD / $CONFIG / N=${N:-default} / $(date -Is)"
echo "# saturn $(git -C "$cydir/generators/saturn" describe --always --dirty 2>/dev/null)"
echo

echo "### 1/4  validate the reference model against the Spike golden file ($validator)"
[ "$STD" = ocp ] || echo "(OCP formats; the P3109 references are checked in models/)"
"$GFLOAT_PYTHON" "$here/gen_data/$validator"

echo
echo "### 2/4  generate data.S  (--std $STD, N=${N:-default})"
"$GFLOAT_PYTHON" "$here/gen_data/gen_data.py" --std "$STD" ${N:+-n "$N"} -o "$here/data.S"
head -1 "$here/data.S"
[ "$STD" = ocp ] || echo "note: the checked-in data.S now holds $STD vectors; rerun with ocp or git checkout it"

echo
echo "### 3/4  cross-build the benchmark"
source "$cydir/env.sh"
make -C "$bmarks" "$BENCH.riscv"

if [ "$GEN_ONLY" = 1 ]; then
	echo
	echo "--gen-only: stopping before the simulator."
	echo
	echo "log: $log"
	exit 0
fi

echo
echo "### 4/4  run on Verilator  (config $CONFIG)"
time "$sim" "$bmarks/$BENCH.riscv"

echo
echo "log: $log"
