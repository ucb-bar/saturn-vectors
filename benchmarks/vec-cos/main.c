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

#include "cos.h"
#include "util.h"
#include "ara/util.h"

extern size_t N_f64;
extern double angles_f64[] __attribute__((aligned(16)));
extern double results_f64[] __attribute__((aligned(16)));
extern double gold_results_f64[] __attribute__((aligned(16)));

extern size_t N_f32;
extern float angles_f32[] __attribute__((aligned(16)));
extern float results_f32[] __attribute__((aligned(16)));
extern float gold_results_f32[] __attribute__((aligned(16)));

#define THRESHOLD 0.3
#define CHECK

int main() {
  printf("FCOS\n");

  int error = 0;
  unsigned long cycles1, cycles2, instr2, instr1;

  printf("Executing cosine on %d 64-bit data...\n", N_f64);
 
  instr1 = read_csr(minstret);
  cycles1 = read_csr(mcycle);
  cos_f64_bmark(angles_f64, results_f64, N_f64);
  instr2 = read_csr(minstret);
  cycles2 = read_csr(mcycle);

  printf("The execution took %ld cycles %ld instructions.\n", cycles2 - cycles1, instr2 - instr1);

  printf("Executing cosine on %d 32-bit data...\n", N_f32);

  instr1 = read_csr(minstret);
  cycles1 = read_csr(mcycle);
  cos_f32_bmark(angles_f32, results_f32, N_f32);
  instr2 = read_csr(minstret);
  cycles2 = read_csr(mcycle);

  printf("The execution took %ld cycles %ld instructions.\n", cycles2 - cycles1, instr2 - instr1);

#ifdef CHECK
  printf("Checking results:\n");

  for (uint64_t i = 0; i < N_f64; ++i) {
    if (!similarity_check(results_f64[i], gold_results_f64[i], THRESHOLD)) {
      error = 1;
      printf("64-bit error at index %d. %lx != %lx\n", i,
	     *(uint64_t*)(&results_f64[i]),
             *(uint64_t*)(&gold_results_f64[i]));
    }
  }
  for (uint64_t i = 0; i < N_f32; ++i) {
    if (!similarity_check(results_f32[i], gold_results_f32[i], THRESHOLD)) {
      error = 1;
      printf("32-bit error at index %d. %x != %x\n", i,
	     *(uint32_t*)(&results_f32[i]),
             *(uint32_t*)(&gold_results_f32[i]));
    }
  }
#endif

  return error;
}
