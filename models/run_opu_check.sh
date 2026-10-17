#!/usr/bin/env bash
#
# Check the 8-bit multiply-accumulate of one OuterProductCell with Verilator:
# every (a, b) pair of both formats, against 11 accumulator values each.
#
#   ./run_opu_check.sh <CONFIG> <workdir>
#
# CONFIG is OPUV256D128P3109ShuttleConfig (P3109),
# OPUV256D128P3109FiniteShuttleConfig (P3109, finite domain) or
# OPUV256D128MxShuttleConfig (OCP FP8). Generate its Verilog first:
#   make -C sims/verilator verilog CONFIG=<CONFIG>
#
# Environment overrides:
#   GFLOAT_PYTHON  interpreter that has gfloat installed
set -eo pipefail

usage() {
	echo "usage: $0 <CONFIG> <workdir>" >&2
	exit 2
}
[ $# -eq 2 ] || usage

here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
cydir=$(cd "$here/../../.." && pwd)
config=$1
work=$(mkdir -p "$2" && cd "$2" && pwd)
gen=$cydir/sims/verilator/generated-src/chipyard.harness.TestHarness.$config/gen-collateral
GFLOAT_PYTHON=${GFLOAT_PYTHON:-$HOME/venvs/gfloat/bin/python}

case "$config" in
	OPUV256D128P3109ShuttleConfig) std=p3109 ;;
	OPUV256D128P3109FiniteShuttleConfig) std=p3109-finite ;;
	OPUV256D128MxShuttleConfig)    std=ocp ;;
	*) usage ;;
esac
if [ ! -d "$gen" ]; then
	echo "error: no $gen" >&2
	echo "       run: make -C $cydir/sims/verilator verilog CONFIG=$config" >&2
	exit 1
fi
if ! "$GFLOAT_PYTHON" -c 'import gfloat' 2>/dev/null; then
	echo "error: no gfloat in $GFLOAT_PYTHON; see benchmarks/common-data-gen/README.md" >&2
	exit 1
fi

"$GFLOAT_PYTHON" "$here/opu_vectors.py" "$work" "$std"
"$GFLOAT_PYTHON" "$here/collect_hier.py" "$gen" OuterProductCell > "$work/opu.f"
verilator --cc -f "$work/opu.f" --exe "$here/tb_opu_cell.cpp" --top-module OuterProductCell \
	--Mdir "$work/obj_opu" -o sim -Wno-fatal -Wno-lint -Wno-style \
	-CFLAGS -O2 --build -j "$(nproc)" \
	> "$work/build_opu.log" 2>&1 || { echo "error: see $work/build_opu.log" >&2; exit 1; }
"$work/obj_opu/sim" "$work/opu.bin"
