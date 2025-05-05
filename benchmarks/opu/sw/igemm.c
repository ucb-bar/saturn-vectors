#include <stdio.h>
#include <riscv-pk/encoding.h>
#include <riscv_vector.h>
#include <stdint.h>
#include <stdlib.h>
#include "bme.h"

#define STR1(x) #x
#ifndef STR
#define STR(x) STR1(x)
#endif

void i8_mm_scalar(int32_t* c_in, int32_t* c_out, int8_t* at, int8_t* b, size_t M, size_t N, size_t K) {
  for (size_t i = 0; i < M; i++) {
    for (size_t j = 0; j < N; j++) {
      c_out[i*N+j] = c_in[i*N+j];
      for (size_t k = 0; k < K; k++) {
        c_out[i*N+j] += at[k*M+i] * b[k*N+j];
      }
    }
  }
}

void i32_load_c(int32_t* c, size_t ml, size_t N) {
  for (size_t r = 0; r < ml; r++) {
    asm volatile("vle32.v v0, (%0)" : : "r"(&c[r*N]));
    VMV_RV(m0, r, v0); // move v0 into row r of m4
  }
}

void i32_store_c(int32_t* c, size_t ml, size_t N) {
  for (size_t r = 0; r < ml; r++) {
    VMV_VR(v0, r, m0); // move row r of m4 into v0
    asm volatile("vse32.v v0, (%0)" : : "r"(&c[r*N]));
  }
}

void i8w32_opacc_loop_k(int8_t* at, int8_t* b, size_t M, size_t N, size_t K, size_t ml, size_t vl) {
  for (size_t k = 0; k < K; k++) {
    asm volatile("vsetvli zero, %0, e8, m1, ta, ma" : : "r"(ml));
    asm volatile("vle8.v v1, (%0)" : : "r"(&at[k*M]));
    // asm volatile("vlse8.v v1, (%0), %1" : : "r"(&a[k]), "r"(K));
    asm volatile("vsetvli zero, %0, e8, m1, ta, ma" : : "r"(vl));
    asm volatile("vle8.v v2, (%0)" : : "r"(&b[k*N]));
    VOPACC(m0, v2, v1);
  }
}

void i8_mm_bme(int32_t* c_in, int32_t* c_out, int8_t* at, int8_t* b, size_t M, size_t N, size_t K) {
  size_t vlenb ;
  asm volatile("vsetvli %0, zero, e8, m1, ta, ma" : "=r"(vlenb));
  size_t mlmax = vlenb / sizeof(int8_t);

  size_t vl;
  asm volatile("vsetvli %0, zero, e32, m4, ta, ma" : "=r"(vl));

  size_t i = 0;
  while (i < M) {
    size_t remaining = M - i;
    size_t ml = remaining > mlmax ? mlmax : remaining;
    // printf("i=%ld, M=%ld, mlmax=%ld, ml=%ld\n", i, M, mlmax, ml);
    for (size_t j = 0; j < N;) {
      asm volatile("vsetvli %0, %1, e32, m4, ta, ma" : "=r"(vl) : "r"(N - j));
      // printf("j=%ld, N=%ld, vl=%ld\n", j, N, vl);
      // printf("load_e32 c[%ld]\n", i*N+j);
      i32_load_c(&c_in[(i*N)+j], ml, N);

      // printf("opacc_e8 at[%ld], b[%ld], N=%ld, vl=%ld\n", i*K, j, N, vl);
      i8w32_opacc_loop_k(&at[i], &b[j], M, N, K, ml, vl);
      
      asm volatile("vsetvli zero, %0, e32, m4, ta, ma" : : "r"(N - j));
      i32_store_c(&c_out[(i*N)+j], ml, N);
      j += vl;
    }
    i += ml;
  }
}

void i8_init(int8_t* d, size_t s, int8_t start) {
  for (size_t i = 0; i < s; i++) {
    d[i] = start + i;
  }
}

void i32_init(int32_t* d, size_t s) {
  for (size_t i = 0; i < s; i++) {
    d[i] = 0;
  }
}
void print_matrix(int32_t* c, size_t M, size_t N) {
  for (size_t i = 0; i < M; i++) {
    for (size_t j = 0; j < N; j++) {
      printf("%03x ", c[i*N + j]);
    }
    printf("\n");
  }
}
int i32_compare(int32_t* a, int32_t* b, size_t m, size_t n) {
  for (size_t i = 0; i < m; i++) {
    for (size_t j = 0; j < n; j++) {
      size_t index = i * n + j;
      if (a[index] != b[index]) {
        printf("DIVERGENCE at index (%ld, %ld): 0x%x != 0x%x\n", i, j, a[index], b[index]);
        return 1;
      }
    }
  }
  return 0;
}

#define MIN 17
#define MAX 33
#define STEP 16
#define K 7

int main(void) {

  int8_t At[MAX*K];
  int8_t B[K*MAX];
  int32_t C_init[MAX*MAX];
  int32_t C_bme[MAX*MAX];
  int32_t C_gold[MAX*MAX];

  for (size_t k = K; k <= K; k += STEP) {
  for (size_t m = MIN; m <= MAX; m += STEP) {
    for (size_t n = MIN; n <= MAX; n += STEP) {
        printf("Initializing M, N, K = %ld %ld %ld\n", m, n, k);
        i8_init(At, m * k, 1);
        i8_init(B, k * n, 2);
        i32_init(C_init, m * n);
        
        i8_mm_scalar(C_init, C_gold, At, B, m, n, k);
        // printf("C init\n");
        // print_matrix(C_init, m, n);
        // printf("C gold\n");
        // print_matrix(C_gold, m, n);

        printf("Testing BME\n");
        i8_mm_bme(C_init, C_bme, At, B, m, n, k);
        // printf("C BME\n");
        // print_matrix(C_bme, m, n);

        int r = 0;      
        r = i32_compare(C_bme, C_gold, m, n);
        if (r) {
          printf("Failure in BME M, N, K = %ld %ld %ld\n", m, n, k);
          exit(1);
        }
        printf("SUCCESS in BME M, N, K = %ld %ld %ld\n\n", m, n, k);
      }
    }
  }
  // printf("SUCCESS testing mmBME\n");
  return 0;
}
