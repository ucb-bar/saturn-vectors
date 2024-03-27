// Copyright 2020 ETH Zurich and University of Bologna.
//
// SPDX-License-Identifier: Apache-2.0
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// Author: Matteo Perotti

// Include <riscv_vector.h> to use vector intrinsics
// Documentation: https://github.com/riscv/rvv-intrinsic-doc
// Compiler support:
// https://github.com/riscv/riscv-gnu-toolchain/tree/rvv-intrinsic

#include "dropout.h"
#include "util.h"

extern const unsigned int N;
extern const float SCALE;
extern const float I[] __attribute__((aligned(16)));
extern const uint8_t SEL[] __attribute__((aligned(16)));
extern float o[] __attribute__((aligned(16)));
extern float o_gold[] __attribute__((aligned(16)));

int main() {
  printf("DROPOU\n");
  unsigned long cycles1, cycles2, instr2, instr1;

  printf("Running Dropout with %d elements.\n", N);

  // Call the main kernel, and measure cycles
  instr1 = read_csr(minstret);
  cycles1 = read_csr(mcycle);
  dropout_vec(N, I, SCALE, SEL, o);
  asm volatile("fence");
  instr2 = read_csr(minstret);
  cycles2 = read_csr(mcycle);

  // Only count effective SPFLOP/cycle
  float performance = (float)N / (cycles2 - cycles1);
  printf("The execution took %ld cycles.\n", cycles2 - cycles1);
  printf("The execution performed %d SFLOPs per 1000 cycles\n", (int)(performance * 1000));

  // Verify correctness
  dropout_gold(N, I, SCALE, SEL, o_gold);

  for (unsigned int k = 0; k < N; ++k) {
    if (o[k] != o_gold[k]) {
      printf("Error: o[%d] = %f != %f\n", k, o[k], o_gold[k]);
      return k ? k : -1;
    }
  }
  printf("Passed.\n");

  return 0;
}
