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
void i32_init(int32_t* d, size_t s) {
  for (size_t i = 0; i < s; i++) {
    d[i] = i + 1;
  }
}
void i8_init(int8_t* d, size_t s, int8_t start) {
  for (size_t i = 0; i < s; i++) {
    d[i] = (i + start) % 127; // keep values small to avoid overflow
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
    printf("maxvl=%lu\n", maxvl);
    printf("dim,ops,cycles\n");
    
    size_t n = 4*maxvl; 
    size_t m = 3*maxvl;
    const size_t K = 32;
    unsigned long cyclest1, cyclest2;
    int8_t at[m*K];
    int8_t b[n*K];
    int32_t c_opu[m*n];
    int32_t c_bias[n];
    i32_init(c_bias, n);
    i8_init(at, m*K, 1);
    i8_init(b, n*K, -3);
    
    // warm up cache
    i8_mm_bme_1x2(c_bias, c_opu, at, b, m, n, K);  
    for (size_t k = 32; k <= K; k+=8) {
        cyclest1 = read_csr(mcycle);
        i8_mm_bme_1x2(c_bias, c_opu, at, b, m, n, k);  
        cyclest2 = read_csr(mcycle);

        // compute metrics and print to csv
        size_t cycles = cyclest2 - cyclest1;
        size_t ops = m * n * k;
        printf("%ld,%ld,%ld\n", k, ops, cycles);
    }

    printf("Verifying against reference...\n");
    printf("M=%ld, N=%ld, K=%ld\n", m, n, K);
    int32_t c_ref[m*n];
    i8_mm_scalar(c_bias, c_ref, at, b, m, n, K);
    int r = 0;      
    r = i32_compare(c_opu, c_ref, m, n);
    if (r) {
        printf("FAILURE; M, N, K = %ld %ld %ld\n", m, n, K);
        exit(1);
    }
    printf("SUCESS; M, N, K = %ld %ld %ld\n", m, n, K);
    return 0;
}