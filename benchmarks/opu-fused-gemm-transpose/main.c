#include <stdio.h>
#include <riscv-pk/encoding.h>
#include <riscv_vector.h>
#include <stdint.h>
#include <stdlib.h>
#include "bme.h"
#include "kernel.h"

void i8_mm_scalar(int32_t* c_bias, int32_t* c_out, int8_t* at, int8_t* b, size_t M, size_t N, size_t K) {
  for (size_t i = 0; i < M; i++) {
    for (size_t j = 0; j < N; j++) {
      c_out[i*N+j] = c_bias[j];
      for (size_t k = 0; k < K; k++) {
        c_out[i*N+j] += at[k*M+i] * b[k*N+j];
      }
    }
  }
}
void i32_transpose_scalar(int32_t* c_in, int32_t* c_out, size_t M, size_t N) {
  for (size_t i = 0; i < M; i++) {
    for (size_t j = 0; j < N; j++) {
      c_out[j*M+i] = c_in[i*N+j];
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
    for (size_t i = 0; i < s; i++) {
      d[i] = (i + start) % 127; // keep values small to avoid overflow
      // d[i] = 0;
    }
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
  asm volatile("vsetvli %[vl], zero, e32, m4, ta, ma" : [vl]"=r"(maxvl));
  size_t dl = maxvl / 2;
  printf("maxvl=%lu; dl=%lu\n", maxvl, dl);

  const size_t M = 2*maxvl;
  const size_t N = 3*maxvl;
  const size_t K = 3;
  int8_t at[M*K];
  int8_t b[N*K];
  int32_t c_opu[M*N];
  int32_t c_ref[M*N];
  int32_t c_ref_transpose[N*M];
  int32_t c_bias[N];
  i32_init(c_bias, N);
  i8_init(at, M*K, 1);
  i8_init(b, N*K, 2);

  for (size_t m = maxvl; m <= M; m+=maxvl) {
    for (size_t n = maxvl; n <= N; n+=maxvl) {
      // for (size_t k = 2; k < K; k++) {
        size_t k = K;
        printf("Testing M=%ld, N=%ld, K=%ld\n", m, n, k);
        i8_mm_scalar(c_bias, c_ref, at, b, m, n, k);
        i32_transpose_scalar(c_ref, c_ref_transpose, m, n);
        i8_mm_bme_sq(c_bias, c_opu, at, b, m, n, k);
        
        // verify against reference
        int r = 0;
        r = i32_compare(c_opu, c_ref_transpose, m, n);
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