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

#include "ara/fdotproduct.h"
#include "ara/spmv.h"
#include "util.h"

#include <stdio.h>

#define MIN_LOSS 0.005
#define MAX_ITERS 10
#define abs(x) (x < 0 ? -x : x)

extern uint64_t size;
extern uint64_t step;
extern double sparsity;

extern double A[] __attribute__((aligned(16)));
extern double Ax[] __attribute__((aligned(16)));
extern double x[] __attribute__((aligned(16)));
extern double b[] __attribute__((aligned(16)));
extern double r[] __attribute__((aligned(16)));
extern double p[] __attribute__((aligned(16)));
extern double Ap[] __attribute__((aligned(16)));
extern int32_t A_PROW[] __attribute__((aligned(16)));
extern int32_t A_IDX[] __attribute__((aligned(16)));
extern double A_DATA[] __attribute__((aligned(16)));

void daxpy(double *x, double a, double *y, double *dest, uint64_t len) {
  while (len) {
    size_t vl = __riscv_vsetvl_e64m8(len);
    asm volatile("vle64.v v0, (%0);" ::"r"(x));
    asm volatile("vle64.v v8, (%0);" ::"r"(y));
    asm volatile("vfmacc.vf v8, %0, v0" ::"f"(a));
    asm volatile("vse64.v v8, (%0);" ::"r"(dest));
    x = x + vl;
    y = y + vl;
    dest = dest + vl;
    len = len - vl;
  }
}

double CG_iteration_spmv(int32_t *A_PROW, int32_t *A_IDX, double *A_DATA,
                         double *x, double *b, double *r, double *p, double *Ap,
                         uint64_t size) {
  /*
  Calculate step length alpha
  */
  double rk_norm = fdotp_v64b(r, r, size);
  spmv_csr_idx32(size, A_PROW, A_IDX, A_DATA, p, Ap);
  double pAp = fdotp_v64b(p, Ap, size);
  // printf("pAp: %f\n", pAp);
  if (abs(pAp) < MIN_LOSS) {
    return rk_norm;
  }
  double alpha = rk_norm / pAp;

  /*
  update x
  */
  daxpy(p, alpha, x, x, size);

  /*
  update loss r
  */
  daxpy(Ap, (-1.0) * alpha, r, r, size);

  /*
  calculate beta
  */
  double rk_norm_new = fdotp_v64b(r, r, size);
  double beta = rk_norm_new / rk_norm;

  /*
  update p
  */
  daxpy(p, beta, r, p, size);

  /*
  return loss
  */
  return rk_norm_new;
}

int main() {
  printf("Conjugate Gradient\n");
  printf("Solving a Ax=b equation with (%d x %d) Matrix size...\n", size, size);
  printf("Sparse Matrix in CSR format, with %ld nonzeros per row\n",
	 (size_t)(sparsity * size));

  printf("Initializing CGM parameters...\n");
  spmv_csr_idx32(size, A_PROW, A_IDX, A_DATA, x, Ax);
  daxpy(Ax, -1.0, b, r, size);
  daxpy(Ax, -1.0, b, p, size);

  printf("Start CGM ...\n");

  // Start instruction and cycles count of the region of interest
  unsigned long cycles1, cycles2, instr2, instr1;
  instr1 = read_csr(minstret);
  cycles1 = read_csr(mcycle);
  uint64_t i = 0;
  while (1) {
    if (step > 0 && i >= step) {
      break;
    }
    double loss = CG_iteration_spmv(A_PROW, A_IDX, A_DATA, x, b, r, p, Ap, size);
    if (loss < MIN_LOSS || i > MAX_ITERS) {
      break;
    }
    i++;
  }

  asm volatile("fence");

  // End instruction and cycles count of the region of interest
  instr2 = read_csr(minstret);
  cycles2 = read_csr(mcycle);

  size_t rk_norm_ops = size;
  size_t spmv_ops = sparsity * size * size;
  size_t pAp_ops = size;
  size_t daxpy_ops = size * 3;
  size_t rk_norm_new_ops = size;

  size_t operations = i * (rk_norm_ops + spmv_ops + pAp_ops + daxpy_ops + rk_norm_new_ops);

  // Instruction and cycles count of the region of interest
  printf("NUMBER OF OPERATIONS %lu\n", operations);
  printf("NUMBER OF EXEC CYCLES :%lu\n", cycles2 - cycles1);
  printf("NUMBER OF INSTRUCTIONS EXECUTED :%lu\n", instr2 - instr1);

  return 0;
}
