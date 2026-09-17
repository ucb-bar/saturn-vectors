#!/usr/bin/env bash
#
# Run one FP8 benchmark end to end: validate its reference model, regenerate its
# test vectors, cross-compile it, and run it on Verilator.
#
#   ./run_fp8_test.sh vec-mx-unary              # conversions
#   ./run_fp8_test.sh vec-mx-binary             # FMA add / sub / mul
#   ./run_fp8_test.sh vec-mx-binary --gen-only  # stop before the (long) simulation
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
#   GFLOAT_PYTHON  interpreter that has gfloat installed (needs >= 3.12)
#   CONFIG         Chipyard config whose simulator binary to run
#                  (default: MXV256D128ShuttleConfig)
#   N              elements per array (default: 256 for vec-mx-unary, 128 for vec-mx-binary)
#
# Note: do NOT add `set -u`. Chipyard's env.sh sources the conda hook, which
# reads unset variables and would abort.
set -eo pipefail

usage() {
	echo "usage: $0 <vec-mx-unary|vec-mx-binary> [--gen-only]" >&2
	exit 2
}

BENCH=${1:-}
[ $# -gt 0 ] && shift
case "$BENCH" in
	vec-mx-unary)  default_n=256 ;;
	vec-mx-binary) default_n=128 ;;
	*) usage ;;
esac

GEN_ONLY=0
for arg in "$@"; do
	case "$arg" in
		--gen-only) GEN_ONLY=1 ;;
		*) usage ;;
	esac
done

bmarks=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
here=$bmarks/$BENCH
cydir=$(cd "$bmarks/../../.." && pwd)

CONFIG=${CONFIG:-MXV256D128ShuttleConfig}
N=${N:-$default_n}
GFLOAT_PYTHON=${GFLOAT_PYTHON:-$HOME/venvs/gfloat/bin/python}
sim=$cydir/sims/verilator/simulator-chipyard.harness-$CONFIG
validator=$(ls "$here"/gen_data/validate_*.py)
results=$here/results
log=$results/$CONFIG-$(date +%Y%m%d-%H%M%S).log
mkdir -p "$results"

if ! "$GFLOAT_PYTHON" -c 'import gfloat' 2>/dev/null; then
	echo "error: no gfloat in $GFLOAT_PYTHON" >&2
	echo "       gfloat needs Python >= 3.12; see benchmarks/common-data-gen/README.md" >&2
	exit 1
fi

exec > >(tee "$log") 2>&1
trap 'ln -sfn "$(basename "$log")" "$results/latest.log"' EXIT

echo "# $BENCH / $CONFIG / N=$N / $(date -Is)"
echo "# saturn $(git -C "$cydir/generators/saturn" describe --always --dirty 2>/dev/null)"
echo

echo "### 1/4  validate the reference model against the Spike golden file ($(basename "$validator"))"
"$GFLOAT_PYTHON" "$validator"

echo
echo "### 2/4  generate data.S  (N=$N)"
"$GFLOAT_PYTHON" "$here/gen_data/gen_data.py" -n "$N" -o "$here/data.S"
head -1 "$here/data.S"

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

if [ ! -x "$sim" ]; then
	echo "error: no simulator at $sim" >&2
	echo "       build it with: make -C $cydir/sims/verilator CONFIG=$CONFIG" >&2
	exit 1
fi

echo
echo "### 4/4  run on Verilator  (config $CONFIG)"
time "$sim" "$bmarks/$BENCH.riscv"

echo
echo "log: $log"
