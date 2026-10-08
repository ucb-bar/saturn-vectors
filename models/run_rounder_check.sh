#!/usr/bin/env bash
#
# Check P3109Rounder's codes and exception flags with Verilator: every BF16
# conversion, and FMA-shaped results on all five core types, in both domains.
#
#   ./run_rounder_check.sh <workdir>
#
# Run from a shell that has sourced Chipyard's env.sh (sbt, verilator).
#
# Environment overrides:
#   GFLOAT_PYTHON  interpreter that has gfloat installed
#   SBT            sbt command (default: Chipyard's sbt-launch.jar)
set -eo pipefail

usage() {
	echo "usage: $0 <workdir>" >&2
	exit 2
}
[ $# -eq 1 ] || usage

here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
cydir=$(cd "$here/../../.." && pwd)
work=$(mkdir -p "$1" && cd "$1" && pwd)
GFLOAT_PYTHON=${GFLOAT_PYTHON:-$HOME/venvs/gfloat/bin/python}
# Chipyard's own sbt and its options (variables.mk)
SBT=${SBT:-java -jar $cydir/scripts/sbt-launch.jar -Dsbt.ivy.home=$cydir/.ivy2 -Dsbt.global.base=$cydir/.sbt -Dsbt.boot.directory=$cydir/.sbt/boot/ -Dsbt.supershell=false}

if ! "$GFLOAT_PYTHON" -c 'import gfloat' 2>/dev/null; then
	echo "error: no gfloat in $GFLOAT_PYTHON; see benchmarks/common-data-gen/README.md" >&2
	exit 1
fi

echo "### emit the wrappers (P3109TestWrappers)"
(cd "$cydir" && $SBT ";project saturn; Test/runMain saturn.exu.P3109TestWrappers $work") > "$work/sbt.log" 2>&1 ||
	{ echo "error: see $work/sbt.log" >&2; exit 1; }

echo "### expected codes and flags"
"$GFLOAT_PYTHON" "$here/dump_expected.py" "$work"
"$GFLOAT_PYTHON" "$here/dump_fma_expected.py" "$work" > /dev/null

# top module, testbench, record file, extra CFLAGS
check() {
	verilator --cc "$work/$1.sv" --exe "$here/$2" --top-module "$1" --Mdir "$work/obj_$1" -o sim \
		-CFLAGS "-DVTOP=V$1 -DVTOP_HEADER='\"V$1.h\"' $4 -O2" --build -j "$(nproc)" \
		> "$work/build_$1.log" 2>&1 || { echo "error: see $work/build_$1.log" >&2; return 1; }
	"$work/obj_$1/sim" "$work/$3" > "$work/run_$1.log" || { tail -1 "$work/run_$1.log"; echo "   see $work/run_$1.log"; return 1; }
	tail -1 "$work/run_$1.log"
}

# Exponent width of each FMA core type (fma_raw.CORES, P3109TestWrappers)
declare -A exp_width=( [FP64]=11 [FP32]=8 [FP16]=5 [BF16]=8 [E5M3]=5 )
rc=0
for dom in Ext Fin; do
	d=${dom,,}
	echo "### conversion rounder, $d domain"
	check "P3109Conv$dom" tb_p3109_rounder.cpp "expected_$d.bin" "" || rc=1
	for core in FP64 FP32 FP16 BF16 E5M3; do
		echo "### FMA rounder on the $core core, $d domain"
		check "P3109FmaRound$core$dom" tb_p3109_fma_round.cpp "fma_${core}_$d.bin" \
			"-DSEXP_W=$(( exp_width[$core] + 2 ))" || rc=1
	done
done
exit $rc
