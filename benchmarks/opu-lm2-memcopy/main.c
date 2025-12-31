#include <stdio.h>
#include <riscv-pk/encoding.h>
#include <riscv_vector.h>
#include <stdint.h>
#include <stdlib.h>
#include "bme.h"
#include "kernel.h"

void i32_mm_scalar(int32_t* c_in, int32_t* c_out, size_t M, size_t N) {
  for (size_t i = 0; i < M; i++) {
    for (size_t j = 0; j < N; j++) {
      c_out[i*N+j] = c_in[i*N+j];
    }
  }
}
void i32_init(int32_t* d, size_t s) {
  for (size_t i = 0; i < s; i++) {
    d[i] = i + 1;
    // d[i] = 0;
  }
}


int i32_compare(int32_t* c_opu, int32_t* c_ref, size_t m, size_t n) {
  for (size_t i = 0; i < m; i++) {
    for (size_t j = 0; j < n; j++) {
      size_t index = i * n + j;
      // printf("i=%ld, j=%ld, index=%ld\n", i, j, index);
      // printf("c_ref[index]=%ld\n", c_ref[index]);
      // printf("c_opu[index]=%ld\n", c_opu[index]);
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
  asm volatile("vsetvli %[vl], zero, e32, m4, ta, ma" : [vl]"=r"(maxvl));
  size_t dl = maxvl / 2;
  printf("maxvl=%lu; dl=%lu\n", maxvl, dl);

  const size_t M = 2*maxvl;
  const size_t N = 2*maxvl;
  int32_t c_opu[M*N];
  int32_t c_ref[M*N];
  int32_t c_in[M*N];
  i32_init(c_in, M*N);

  for (size_t m = maxvl; m <= M; m+=maxvl) {
    for (size_t n = maxvl; n <= N; n+=maxvl) {
        printf("Testing M=%ld, N=%ld\n", m, n);
        i32_mm_scalar(c_in, c_ref, m, n);
        i32_mm_bme_1x2(c_in, c_opu, m, n);
        
        // verify against reference
        int r = 0;
        r = i32_compare(c_opu, c_ref, m, n);
        if (r) {
            printf("FAILURE; M, N = %ld %ld\n", m, n);
            exit(1);
        }
        printf("SUCCESS; M, N = %ld %ld\n", m, n);
    }
  }
  return 0;
}