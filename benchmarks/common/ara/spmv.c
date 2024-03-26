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

#include "spmv.h"
#include "util.h"
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <riscv_vector.h>

void spmv_csr_idx32(int32_t N_ROW, int32_t *CSR_PROW, int32_t *CSR_INDEX,
                    double *CSR_DATA, double *IN_VEC, double *OUT_VEC) {
  /* printf("Do spmv\n"); */
  for (int i = 0; i < N_ROW; ++i) {
    int32_t len = CSR_PROW[i + 1] - CSR_PROW[i];
    double *data = CSR_DATA + CSR_PROW[i];
    int32_t *index = CSR_INDEX + CSR_PROW[i];

    // clear register file

    volatile size_t maxvl = __riscv_vsetvl_e64m8(len);
    asm volatile("vmv.v.i v24,  0");

    double dbg = 0;
    while (len) {
      volatile size_t slice_size = __riscv_vsetvl_e64m8(len);
      asm volatile("vle64.v v8, (%0)" ::"r"(data));          // fetch entries
      asm volatile("vle32.v v16, (%0)" ::"r"(index));         // fetch indices
      asm volatile("vloxei32.v v0, (%0), v16" ::"r"(IN_VEC)); // load data
      asm volatile("vfmacc.vv v24, v8, v0");      // vector multiply
      /* if (i == 0) { */
      /* 	printf("slice=%ld\n", slice_size); */
      /* 	for (size_t j = 0; j < slice_size; j++) { */
      /* 	  double t, u, v; */
      /* 	  asm volatile("vrgather.vx v16, v12, %0" :: "r"(j)); */
      /* 	  asm volatile("vfmv.f.s %0, v16" : "=f"(t)); */

      /* 	  asm volatile("vrgather.vx v16, v4, %0" :: "r"(j)); */
      /* 	  asm volatile("vfmv.f.s %0, v16" : "=f"(u)); */

      /* 	  asm volatile("vrgather.vx v16, v0, %0" :: "r"(j)); */
      /* 	  asm volatile("vfmv.f.s %0, v16" : "=f"(v)); */
      /* 	  dbg += data[j] * IN_VEC[index[j]/sizeof(double)]; */
      /* 	  printf("i=%ld j=%ld id=%ld data=%lx vec=%lx out=%lx vdata=\"%lx\" vvec=\"%lx\" vout=\"%lx\"\n", */
      /* 		 i, j, index[j] / sizeof(double), */
      /* 		 *(uint64_t*)(&data[j]), *(uint64_t*)(&IN_VEC[index[j]/sizeof(double)]), */
      /* 		 *(uint64_t*)(&dbg), */
      /* 		 *(uint64_t*)(&u), *(uint64_t*)(&v), *(uint64_t*)(&t) */
      /* 		 ); */

      /* 	} */
      /* } */

      len = len - slice_size;
      data = data + slice_size;
      index = index + slice_size;
    }

    double tmp;
    asm volatile("vsetvli x0, %0, e64, m8, ta, ma"::"r"(maxvl));
    asm volatile("vmv.v.i v8, 0");
    asm volatile("vfredusum.vs v24, v24, v8"); // reduction
    asm volatile("vfmv.f.s %0, v24" : "=f"(tmp));
    OUT_VEC[i] = tmp;
  }
  /* printf("Verifying\n"); */
  //spmv_verify(N_ROW, CSR_PROW, CSR_INDEX, CSR_DATA, IN_VEC, OUT_VEC);
}

int spmv_verify(int32_t N_ROW, int32_t *CSR_PROW, int32_t *CSR_INDEX,
                double *CSR_DATA, double *IN_VEC, double *OUT_VEC) {
  for (int32_t i = 0; i < N_ROW; ++i) {
    double res = OUT_VEC[i];

    int32_t len = CSR_PROW[i + 1] - CSR_PROW[i];
    double *data = CSR_DATA + CSR_PROW[i];
    int32_t *index = CSR_INDEX + CSR_PROW[i];

    double golden = 0;
    for (int32_t j = 0; j < len; ++j) {
      int32_t idx = index[j] / sizeof(double);
      double next = golden + data[j] * IN_VEC[idx];
      /* printf("index:%d, data: %lx, vec: %lx %lx\n", idx, *(uint64_t*)(&data[j]), *(uint64_t*)(&IN_VEC[idx]), */
      /* 	     *(uint64_t*)(&next)); */
      golden = next;

    }
    if ((float)golden != (float)res) {
      printf("Sorry, wrong value! at index %d, result = %lx, golden = %lx \n", i,
             *(uint64_t*)(&res), *(uint64_t*)(&golden));
      exit(1);
      return i;
    }
  }
  return 0;
}
