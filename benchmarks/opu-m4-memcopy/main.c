#include <stdio.h>
#include <riscv-pk/encoding.h>
#include <riscv_vector.h>
#include <stdint.h>
#include <stdlib.h>
#include "bme.h"
#include "kernel.h"
#include "dataset.h"

int i32_compare(int32_t* c_opu, int32_t* c_ref, size_t m, size_t n) {
  for (size_t i = 0; i < m; i++) {
    for (size_t j = 0; j < n; j++) {
      size_t index = i * n + j;
      if (c_opu[index] != c_ref[index]) {
        printf("DIVERGENCE at index (%ld, %ld): 0x%x != 0x%x\n", i, j, c_opu[index], c_ref[index]);
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
  asm volatile("vsetvli %[vl], zero, e8, m1, ta, ma" : [vl]"=r"(maxvl));
  size_t dl = maxvl / 2;
  printf("maxvl=%lu; dl=%lu\n", maxvl, dl);

  int32_t c_opu[M_DIM*N_DIM];

  for (size_t m = M_DIM; m <= M_DIM; m+=maxvl) {
    for (size_t n = N_DIM; n <= N_DIM; n+=maxvl) {
        printf("Testing M=%ld, N=%ld\n", m, n);
        i32_bme_memcopy(c_in, c_opu, m, n);
        
        // verify against reference
        int r = 0;
        r = i32_compare(c_opu, c_in, n, m);
        if (r) {
            printf("FAILURE; M, N = %ld %ld\n", m, n);
            exit(1);
        }
        printf("SUCCESS; M, N = %ld %ld\n", m, n);
    }
  }
  return 0;
}