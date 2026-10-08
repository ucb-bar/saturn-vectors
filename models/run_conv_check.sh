#!/usr/bin/env bash
#
# Check the P3109 conversions in FPConvBlock, codes and exception flags, with
# Verilator. Block configs get a different scale on every lane.
#
#   ./run_conv_check.sh <CONFIG> <workdir>
#
# CONFIG is one of P3109V256D128ShuttleConfig, P3109FiniteV256D128ShuttleConfig,
# P3109BlockV256D128ShuttleConfig, P3109BlockFiniteV256D128ShuttleConfig; the
# domain and block scaling are taken from its name. Generate its Verilog first:
#   make -C sims/verilator verilog CONFIG=<CONFIG>
# A block config writes about 600 MB of vectors to the workdir.
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
	P3109V256D128ShuttleConfig)            finite=0; block=0 ;;
	P3109FiniteV256D128ShuttleConfig)      finite=1; block=0 ;;
	P3109BlockV256D128ShuttleConfig)       finite=0; block=1 ;;
	P3109BlockFiniteV256D128ShuttleConfig) finite=1; block=1 ;;
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

"$GFLOAT_PYTHON" "$here/conv_vectors.py" "$work" "$finite" "$block"
"$GFLOAT_PYTHON" "$here/collect_hier.py" "$gen" FPConvBlock > "$work/conv.f"
verilator --cc -f "$work/conv.f" --exe "$here/tb_conv.cpp" --top-module FPConvBlock \
	--Mdir "$work/obj_conv" -o sim -Wno-fatal -Wno-lint -Wno-style \
	-CFLAGS "-O2 $([ "$block" = 1 ] && echo -DHAS_SCALE)" --build -j "$(nproc)" \
	> "$work/build_conv.log" 2>&1 || { echo "error: see $work/build_conv.log" >&2; exit 1; }
"$work/obj_conv/sim" "$work/conv.bin"
