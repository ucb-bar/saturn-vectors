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
//
// Author: Matteo Perotti <mperotti@iis.ee.ethz.ch>

#include <stdint.h>
#include <string.h>

#include "softmax.h"
#include "util.h"
#include "ara/util.h"
#include <stdio.h>

// Check the results using a threshold
#define CHECK

// Sanity check to see that there are some precision differences
// between the two algorithms
// #define SANITY_CHECK

// Sanity check to see the results
// #define PRINT_RESULTS

#define THRESHOLD 0.1

extern uint64_t channels;
extern uint64_t innerSize;
extern float i[] __attribute__((aligned(32)));
extern float buf[] __attribute__((aligned(32)));
extern float o_s[] __attribute__((aligned(32)));
extern float o_v[] __attribute__((aligned(32)));

int main() {
  printf("SOFTMAX\n");
  printf("Channels: %lu\nInner Size: %lu\n", channels, innerSize);

  int64_t runtime;
  int error = 0;
  unsigned long cycles1, cycles2, instr2, instr1;

  printf("Scalar Softmax...\n");
  instr1 = read_csr(minstret);
  cycles1 = read_csr(mcycle);
  softmax(i, o_s, buf, channels, innerSize);
  asm volatile("fence");
  instr2 = read_csr(minstret);
  cycles2 = read_csr(mcycle);
  runtime = cycles2 - cycles1;
  printf("The scalar SOFTMAX execution took %d cycles.\n", runtime);

  printf("Vector Softmax...\n");
  instr1 = read_csr(minstret);
  cycles1 = read_csr(mcycle);
  softmax_vec(i, o_v, channels, innerSize);
  asm volatile("fence");
  instr2 = read_csr(minstret);
  cycles2 = read_csr(mcycle);
  runtime = cycles2 - cycles1;
  printf("The vector Softmax execution took %d cycles.\n", runtime);

  for (uint64_t k = 0; k < channels * innerSize; ++k) {
    if (!similarity_check(o_s[k], o_v[k], THRESHOLD)) {
      error = 1;
      printf("Error at index %d. %x != %x\n", k, *(uint32_t*)(&o_v[k]), *(uint32_t*)(&o_s[k]));
    }
  }
  if (!error)
    printf("Check okay. No errors.\n");

  return error;
}
