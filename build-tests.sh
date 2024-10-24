#!/bin/bash

set -ex

cd riscv-vector-tests

rm -rf out/

make MODE=machine VLEN=256 XLEN=64 SPLIT=50000 TEST_MODE="cosim" generate-stage1
make MODE=machine VLEN=256 XLEN=64 SPLIT=50000 TEST_MODE="cosim" all -j72
rm -rf out/v256x64machine/bin/stage2/vfredusum*
rm -rf out/v256x64machine/bin/stage2/vfwredusum*
rm -rf out/v256x64machine/bin/stage2/vaes*
rm -rf out/v256x64machine/bin/stage2/vsha*
rm -rf out/v256x64machine/bin/stage2/vsm3*
rm -rf out/v256x64machine/bin/stage2/vsm4*
rm -rf out/v256x64machine/bin/stage2/vclmul*
rm -rf out/v256x64machine/bin/stage2/vghsh*
rm -rf out/v256x64machine/bin/stage2/vgmul*

make MODE=virtual VLEN=256 XLEN=64 SPLIT=6000 TEST_MODE="cosim" PATTERN='^v[ls].+\.v$' generate-stage1
make MODE=virtual VLEN=256 XLEN=64 SPLIT=6000 TEST_MODE="cosim" PATTERN='^v[ls].+\.v$' all -j72

make MODE=machine VLEN=128 XLEN=64 SPLIT=50000 TEST_MODE="cosim" generate-stage1
make MODE=machine VLEN=128 XLEN=64 SPLIT=50000 TEST_MODE="cosim" all -j72
rm -rf out/v128x64machine/bin/stage2/vfredusum*
rm -rf out/v128x64machine/bin/stage2/vfwredusum*
rm -rf out/v128x64machine/bin/stage2/vaes*
rm -rf out/v128x64machine/bin/stage2/vsha*
rm -rf out/v128x64machine/bin/stage2/vsm3*
rm -rf out/v128x64machine/bin/stage2/vsm4*
rm -rf out/v128x64machine/bin/stage2/vclmul*
rm -rf out/v128x64machine/bin/stage2/vghsh*
rm -rf out/v128x64machine/bin/stage2/vgmul*

make MODE=virtual VLEN=128 XLEN=64 SPLIT=6000 TEST_MODE="cosim" PATTERN='^v[ls].+\.v$' generate-stage1
make MODE=virtual VLEN=128 XLEN=64 SPLIT=6000 TEST_MODE="cosim" PATTERN='^v[ls].+\.v$' all -j72
