#include <stdio.h>
#include <riscv-pk/encoding.h>
#include <riscv_vector.h>
#include <stdint.h>
#include <stdlib.h>
#include "bme.h"
#include "kernel.h"
#include "dataset.h"

void i8_mm_scalar(int32_t* c_bias, int32_t* c_out, int8_t* at, int8_t* b, size_t M, size_t N, size_t K) {
  for (size_t i = 0; i < M; i++) {
    for (size_t j = 0; j < N; j++) {
      c_out[j*M+i] = c_bias[j];
      for (size_t k = 0; k < K; k++) {
        c_out[j*M+i] += at[k*M+i] * b[k*N+j];
      }
    }
  }
}

void i32_init(int32_t* d, size_t s) {
  for (size_t i = 0; i < s; i++) {
    d[i] = i + 1;
    // d[i] = 0;
  }
}
void i8_init(int8_t* d, size_t s, int8_t start) {
  // printf("i8_init: d: %p, s: %lu, start: %d\n", d, s, start);
    for (size_t i = 0; i < s; i++) {
      d[i] = (int8_t) ((i + start) % 256); // keep values small to avoid overflow
      // printf("d[%lu]=%d; ", i, d[i]);
      // d[i] = 0;
    }
    // printf("\n");
  }

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
  asm volatile("vsetvli %[vl], zero, e8, m2, ta, ma" : [vl]"=r"(maxvl));
  size_t dl = maxvl / 2;
  printf("maxvl=%lu; dl=%lu\n", maxvl, dl);

  int32_t c_opu[M_DIM*N_DIM];

  for (size_t m = M_DIM; m <= M_DIM; m+=maxvl) {
    for (size_t n = N_DIM; n <= N_DIM; n+=maxvl) {
      // for (size_t k = 2; k < K; k++) {
        size_t k = K_DIM;
        printf("Testing M=%ld, N=%ld, K=%ld\n", m, n, k);
        // i8_mm_scalar(c_bias, c_opu, a_matrix, b_matrix, m, n, k);
        i8_mm_bme_lm2(c_bias, c_opu, a_matrix, b_matrix, m, n, k);
        
        // verify against reference
        int r = 0;
        r = i32_compare(c_opu, verify_data, m, n);
        if (r) {
            printf("FAILURE; M, N, K = %ld %ld %ld\n", m, n, k);
            exit(1);
        }
        printf("SUCCESS; M, N, K = %ld %ld %ld\n", m, n, k);
      // }
    }
  }
  return 0;
}