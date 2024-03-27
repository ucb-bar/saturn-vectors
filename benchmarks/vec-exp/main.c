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
// Author: Matteo Perotti

#include <stdint.h>
#include <string.h>

#include "exp.h"
#include "util.h"
#include "ara/util.h"
#include <stdio.h>

extern size_t N_f64;
extern double exponents_f64[] __attribute__((aligned(32)));
extern double results_f64[] __attribute__((aligned(32)));
extern double gold_results_f64[] __attribute__((aligned(32)));

extern size_t N_f32;
extern float exponents_f32[] __attribute__((aligned(32)));
extern float results_f32[] __attribute__((aligned(32)));
extern float gold_results_f32[] __attribute__((aligned(32)));

#define THRESHOLD 0.3


int main() {
  printf("FEXP\n");

  int error = 0;
  unsigned long cycles1, cycles2, instr2, instr1;

  printf("Executing exponential on %d 64-bit data...\n", N_f64);
  instr1 = read_csr(minstret);
  cycles1 = read_csr(mcycle);
  exp_1xf64_bmark(exponents_f64, results_f64, N_f64);
  instr2 = read_csr(minstret);
  cycles2 = read_csr(mcycle);
  printf("The execution took %d cycles.\n", cycles2 - cycles1);

  printf("Executing exponential on %d 32-bit data...\n", N_f32);
  instr1 = read_csr(minstret);
  cycles1 = read_csr(mcycle);
  exp_2xf32_bmark(exponents_f32, results_f32, N_f32);
  instr2 = read_csr(minstret);
  cycles2 = read_csr(mcycle);
  printf("The execution took %d cycles.\n", cycles2 - cycles1);

  printf("Checking results:\n");

  for (uint64_t i = 0; i < N_f64; ++i) {
    if (!similarity_check(results_f64[i], gold_results_f64[i], THRESHOLD)) {
      error = 1;
      printf("64-bit error at index %d. %lx != %lx\n", i, *(uint64_t*)(&results_f64[i]),
             *(uint64_t*)(&gold_results_f64[i]));
    }
  }
  for (uint64_t i = 0; i < N_f32; ++i) {
    if (!similarity_check(results_f32[i], gold_results_f32[i], THRESHOLD)) {
      error = 1;
      printf("32-bit error at index %d. %x != %x\n", i, *(uint32_t*)(&results_f32[i]),
             *(uint32_t*)(&gold_results_f32[i]));
    }
  }
  if (!error)
    printf("Test result: PASS. No errors found.\n");

  return error;
}
