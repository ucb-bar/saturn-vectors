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

#include "log.h"
#include "util.h"
#include "ara/util.h"

#define THRESHOLD 0.1

#define CHECK

extern size_t N_f64;
extern double args_f64[] __attribute__((aligned(16)));
extern double results_f64[] __attribute__((aligned(16)));
extern double gold_results_f64[] __attribute__((aligned(16)));

extern size_t N_f32;
extern float args_f32[] __attribute__((aligned(16)));
extern float results_f32[] __attribute__((aligned(16)));
extern float gold_results_f32[] __attribute__((aligned(16)));

// Natural logarithm (base e)
int main() {
  printf("FLOG\n");
  unsigned long cycles1, cycles2, instr2, instr1;

  int error = 0;
  int64_t runtime;

  printf("Executing natural log (base e) on %d 64-bit data...\n", N_f64);
  instr1 = read_csr(minstret);
  cycles1 = read_csr(mcycle);
  log_1xf64_bmark(args_f64, results_f64, N_f64);
  asm volatile("fence");
  instr2 = read_csr(minstret);
  cycles2 = read_csr(mcycle);
  runtime = cycles2 = cycles1;
  printf("The execution took %d cycles.\n", runtime);

  printf("Executing natural log (base e) on %d 32-bit data...\n", N_f32);
  instr1 = read_csr(minstret);
  cycles1 = read_csr(mcycle);
  log_2xf32_bmark(args_f32, results_f32, N_f32);
  instr2 = read_csr(minstret);
  cycles2 = read_csr(mcycle);
  runtime = cycles2 = cycles1;
  printf("The execution took %d cycles.\n", runtime);

#ifdef CHECK
  printf("Checking results:\n");

  for (uint64_t i = 0; i < N_f64; ++i) {
    if (!similarity_check(results_f64[i], gold_results_f64[i], THRESHOLD)) {
      error = 1;
      printf("64-bit error at index %d. %f != %f\n", i, results_f64[i],
             gold_results_f64[i]);
    }
  }
  for (uint64_t i = 0; i < N_f32; ++i) {
    if (!similarity_check(results_f32[i], gold_results_f32[i], THRESHOLD)) {
      error = 1;
      printf("32-bit error at index %d. %f != %f\n", i, results_f32[i],
             gold_results_f32[i]);
    }
  }
#endif

  return error;
}
