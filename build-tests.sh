#!/bin/bash

set -ex

cd riscv-vector-tests

make MODE=machine VLEN=256 XLEN=64 SPLIT=50000 TEST_MODE="cosim" generate-stage1
make MODE=machine VLEN=256 XLEN=64 SPLIT=50000 TEST_MODE="cosim" all -j72

make MODE=virtual VLEN=256 XLEN=64 SPLIT=6000 TEST_MODE="cosim" PATTERN='^v[ls].+\.v$' generate-stage1
make MODE=virtual VLEN=256 XLEN=64 SPLIT=6000 TEST_MODE="cosim" PATTERN='^v[ls].+\.v$' all -j72
