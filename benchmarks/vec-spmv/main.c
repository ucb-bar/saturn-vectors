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

// Author: Chi Zhang, ETH Zurich <chizhang@iis.ee.ethz.ch>

#include <stdint.h>
#include <string.h>

#include "ara/spmv.h"
#include "util.h"
#include <stdio.h>

extern uint64_t R;
extern uint64_t C;
extern uint64_t NZ;

extern int32_t CSR_PROW[]
    __attribute__((aligned(32), section(".l2")));
extern int32_t CSR_INDEX[]
    __attribute__((aligned(32), section(".l2")));
extern double CSR_DATA[] __attribute__((aligned(32), section(".l2")));
extern double CSR_IN_VECTOR[]
    __attribute__((aligned(32), section(".l2")));
extern double CSR_OUT_VECTOR[]
    __attribute__((aligned(32), section(".l2")));

int main() {
  printf("SpMV\n");

  unsigned long cycles1, cycles2, instr2, instr1;
  double density = ((double)NZ) / (R * C);
  double nz_per_row = ((double)NZ) / R;

  printf(
      "Calculating a (%d x %d) x %d sparse matrix vector multiplication...\n",
      R, C, C);
  printf("CSR format with %d nozeros: %ld nonzeros per 1000 elements, %ld nonzeros per row \n", NZ,
         (uint64_t)(density * 1000.0), (uint64_t)nz_per_row);
  instr1 = read_csr(minstret);
  cycles1 = read_csr(mcycle);
  spmv_csr_idx32(R, CSR_PROW, CSR_INDEX, CSR_DATA, CSR_IN_VECTOR,
                 CSR_OUT_VECTOR);
  asm volatile("fence");
  instr2 = read_csr(minstret);
  cycles2 = read_csr(mcycle);

  // Metrics
  int64_t runtime = cycles2 - cycles1;
  float performance = 2.0 * NZ / runtime;

  printf("The execution took %d cycles.\n", runtime);
  printf("The performance is %ld FLOPs/1000 cycles.\n",
         (uint64_t)(1000.0 * performance));

  printf("Verifying ...\n");
  if (spmv_verify(R, CSR_PROW, CSR_INDEX, CSR_DATA, CSR_IN_VECTOR,
                  CSR_OUT_VECTOR)) {
    return 1;
  } else {
    printf("Passed.\n");
  }
  return 0;
}
