#!/usr/bin/env bash
# Run the FP8 conversion test end to end (validate, generate, build, simulate).
#   ./run_baseline.sh [ocp|p3109|p3109-finite] [--gen-only]
# All the work happens in ../run_fp8_test.sh, which documents the options.
exec "$(dirname "${BASH_SOURCE[0]}")/../run_fp8_test.sh" vec-mx-unary "$@"
