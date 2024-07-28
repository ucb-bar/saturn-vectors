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

#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "ara/util.h"
#include "fconv3d.h"
#include "util.h"

// Define Matrix dimensions:
// o = i ° f, with i=[(M+F-1)x(N+f-1)xCH], f=[FxFxCH], o=[MxN]
// The filter is a square matrix, and F is odd

// Matrices defined in data.S
extern double i[] __attribute__((aligned(32))); // [ (M+floor(F/2)) * (N+floor(F/2)) * CH ]
extern double f[] __attribute__((aligned(32)));        // [ F*F*CH ]
extern double o[] __attribute__((aligned(32)));        // [ M*N ]
extern double golden_o[] __attribute__((aligned(32))); // [ M*N ]
// M, N, F defined in data.S
extern int64_t M;
extern int64_t N;
extern int64_t CH;
extern int64_t F;

// Verify the matrices
int verify_matrix(double *matrix, double *golden_matrix, int64_t R, int64_t C,
                  double threshold) {
  for (int r = 0; r < R; ++r)
    for (int c = 0; c < C; ++c)
      if (!similarity_check(matrix[c + C * r], golden_matrix[c + C * r],
                            threshold)) {
        printf("Error: o[%d][%d] = %lf, instead of %lf\n", r, c,
               matrix[c + C * r], golden_matrix[c + C * r]);
        return 1;
      }
  return 0;
}

int main() {
  printf("FCONV3D float64\n");
  printf("Input Mtx size: %dx%d\n", M + F - 1, N + F - 1);
  printf("Output Mtx size: %dx%d\n", M, N);
  printf("Filter size: %dx%d\n", F, F);
  printf("Channels: %d\n", CH);

#if PREALLOCATE
  if (F == 7)
    fconv3d_CHx7x7(o, i, f, M, N, CH, F);
  else
    printf("Error: the filter size is different from 7.\n");
#endif

  unsigned long cycles1, cycles2, instr2, instr1;
  instr1 = read_csr(minstret);
  cycles1 = read_csr(mcycle);
  // Call the main kernel, and measure cycles
  if (F == 7)
    fconv3d_CHx7x7(o, i, f, M, N, CH, F);
  else
    printf("Error: the filter size is different from 7.\n");
  asm volatile("fence");
  instr2 = read_csr(minstret);
  cycles2 = read_csr(mcycle);

  // Performance metrics
  int64_t runtime = cycles2 - cycles1;
  float performance = 2.0 * CH * F * F * M * N / runtime;
  printf("Operations: %ld\n", CH * F * F * M * N);
  printf("The execution took %d cycles.\n", runtime);
  printf("The performance is %ld DPFLOP/1000 cycles.\n",
         (uint64_t)(1000.0 * performance));

  // Verify correctness
  printf("Verifying result...\n");
  int error = verify_matrix(o, golden_o, M, N, THRESHOLD);
  if (error != 0) {
    printf("Fail.\n");
  } else {
    printf("Passed.\n");
  }

  return error;
}
