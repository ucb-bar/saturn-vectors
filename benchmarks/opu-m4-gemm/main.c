#include <stdio.h>
#include <riscv-pk/encoding.h>
#include <riscv_vector.h>
#include <stdint.h>
#include <stdlib.h>
#include "bme.h"
#include "kernel.h"
#include "dataset.h"

#define STR1(x) #x
#ifndef STR
#define STR(x) STR1(x)
#endif

int i32_compare(int32_t* c_opu, int32_t* c_ref, size_t m, size_t n) {
    for (size_t i = 0; i < m; i++) {
      for (size_t j = 0; j < n; j++) {
        size_t index = i * n + j;
        if (c_opu[index] != c_ref[index]) {
          printf("DIVERGENCE at index (%ld, %ld): opu=0x%x =/= ref=0x%x\n", i, j, c_opu[index], c_ref[index]);
          printf("opu:\n");
          for (size_t ii = 0; ii < m; ii++) {
            for (size_t jj = 0; jj < n; jj++) {
              printf("0x%x ", c_opu[ii*n + jj]);
            }
            printf("\n");
          }
          printf("reference:\n");
          for (size_t ii = 0; ii < m; ii++) {
            for (size_t jj = 0; jj < n; jj++) {
              printf("0x%x ", c_ref[ii*n + jj]);
            }
            printf("\n");
          }
          return 1;
        }
      }
    }
    return 0;
  }

int main(void) {
  size_t maxvl;
  asm volatile("vsetvli %[vl], zero, e32, m4, ta, ma" : [vl]"=r"(maxvl));
  size_t dl = maxvl / 2;
  printf("maxvl=%lu; dl=%lu\n", maxvl, dl);

  int32_t c_opu[M_DIM*N_DIM];
  
  printf("Testing M=%ld, N=%ld, K=%ld\n", M_DIM, N_DIM, K_DIM);
  i8_mm_bme_2x2(c_bias, c_opu, at, b, M_DIM, N_DIM, K_DIM);
  
  // verify against reference data
  int r = i32_compare(c_opu, verify_data, M_DIM, N_DIM);
  if (r) {
    printf("FAILURE; M, N, K = %ld %ld %ld\n", M_DIM, N_DIM, K_DIM);
    exit(1);
  }

  printf("SUCCESS; M, N, K = %ld %ld %ld\n", M_DIM, N_DIM, K_DIM);
  return 0;
}