// Copyright 2022 ETH Zurich and University of Bologna.
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

// Author: Matteo Perotti <mperotti@iis.ee.ethz.ch>

#include <stdint.h>
#include <string.h>
#include "util.h"

#include "ara/fdotproduct.h"
#include "ara/util.h"
#include <stdio.h>

// Threshold for FP comparisons
#define THRESHOLD_64b 0.0000000001
#define THRESHOLD_32b 0.0001
#define THRESHOLD_16b 1

// Vector size (Byte)
extern uint64_t vsize;
// Input vectors
extern double v64a[] __attribute__((aligned(256)));
extern double v64b[] __attribute__((aligned(256)));
extern float v32a[] __attribute__((aligned(256)));
extern float v32b[] __attribute__((aligned(256)));
extern _Float16 v16a[] __attribute__((aligned(256)));
extern _Float16 v16b[] __attribute__((aligned(256)));
// Golden outputs
extern double gold64;
extern float gold32;
extern _Float16 gold16;
// Output vectors
extern double res64_v, res64_s;
extern float res32_v, res32_s;
extern _Float16 res16_v, res16_s;

int main() {
  printf("FDOTP\n");


  unsigned long cycles1, cycles2, instr2, instr1;

  for (uint64_t avl = 8; avl <= vsize; avl *= 8) {
    printf("Calulating 64b dotp with vectors with length = %lu\n", avl);
    instr1 = read_csr(minstret);
    cycles1 = read_csr(mcycle);
    res64_v = fdotp_v64b(v64a, v64b, avl);
    asm volatile("fence");
    instr2 = read_csr(minstret);
    cycles2 = read_csr(mcycle);
    printf("Vector runtime: %ld\n", cycles2 - cycles1);
  }

  for (uint64_t avl = 8; avl <= vsize; avl *= 8) {
    printf("Calulating 32b dotp with vectors with length = %lu\n", avl);
    instr1 = read_csr(minstret);
    cycles1 = read_csr(mcycle);
    res32_v = fdotp_v32b(v32a, v32b, avl);
    asm volatile("fence");
    instr2 = read_csr(minstret);
    cycles2 = read_csr(mcycle);
    printf("Vector runtime: %ld\n", cycles2 - cycles1);
  }

  for (uint64_t avl = 8; avl <= vsize; avl *= 8) {
    printf("Calulating 16b dotp with vectors with length = %lu\n", avl);
    instr1 = read_csr(minstret);
    cycles1 = read_csr(mcycle);
    res16_v = fdotp_v16b(v16a, v16b, avl);
    asm volatile("fence");
    instr2 = read_csr(minstret);
    cycles2 = read_csr(mcycle);
    printf("Vector runtime: %ld\n", cycles2 - cycles1);
  }

  printf("SUCCESS.\n");

  return 0;
}
