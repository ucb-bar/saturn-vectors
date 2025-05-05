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

void i32_load_c(int* c, size_t rows, size_t N) {
  for (size_t r = 0; r < rows; r++) {
    asm volatile("vle32.v v0, (%0)" : : "r"(&c[r*N]));
    VMV_RV(m0, r, v0); // move v0 into row r of m0
  }
}

void i32_store_c(int* c, size_t rows, size_t N) {
  for (size_t r = 0; r < rows; r++) {
    VMV_VR(v0, r, m0); // move row r of m0 into v0
    asm volatile("vse32.v v0, (%0)" : : "r"(&c[r*N]));
  }
}
void i32_move_c(int* c_in, int* c_out, size_t rows, size_t N) {
  for (size_t r = 0; r < rows; r++) {
    asm volatile("vle32.v v0, (%0)" : : "r"(&c_in[r*N]));
    VMV_RV(m0, r, v0); // move v0 into row r of m0
    VMV_VR(v4, r, m0); // move row r of m0 into v0
    asm volatile("vse32.v v4, (%0)" : : "r"(&c_out[r*N]));
  }
}

void i32_setc_bme(int* c_in, int* c_out, size_t M, size_t N, size_t K) {
  size_t vlenb ;
  asm volatile("vsetvli %0, zero, e8, m1, ta, ma" : "=r"(vlenb));
  size_t mlmax = vlenb / sizeof(int);

  size_t vl;
  asm volatile("vsetvli %0, zero, e32, m4, ta, ma" : "=r"(vl));
  size_t i = 0;

  while (i < M) {
    size_t remaining = M - i;
    size_t block = remaining > mlmax ? mlmax : remaining;
    for (size_t j = 0; j < N;) {
      asm volatile("vsetvli %0, %1, e32, m4, ta, ma" : "=r"(vl) : "r"(N - j));
      i32_move_c(&c_in[(i*N)+j], &c_out[(i*N)+j], block, N);
      j += vl;
    }
    i += block;
  }
}

void i32_init(int* d, size_t s) {
  for (size_t i = 0; i < s; i++) {
    d[i] = i + 1;
  }
}

int i32_compare(int* a, int* b, size_t s) {
  for (size_t i = 0; i < s; i++) {
    if (a[i] != b[i]) {
      printf("Divergence %d != %d index %ld\n", a[i], b[i], i);
      return 1;
    }
  }
  return 0;
}

void print_matrix(int32_t* c, size_t M, size_t N) {
  for (size_t i = 0; i < M; i++) {
    for (size_t j = 0; j < N; j++) {
      printf("%03x ", c[i*N + j]);
    }
    printf("\n");
  }
}
#define MAX 17

int main(void) {

  int32_t C_bme[MAX*MAX];
  int32_t C_gold[MAX*MAX];

  int r = 0;
  int m = MAX;
  int n = MAX;
  int k = MAX;
  printf("Testing MM M, N, K = %d %d %d\n", m, n, k);
  i32_init(C_gold, MAX*MAX);
  i32_setc_bme(C_gold, C_bme, m, n, k);

  // printf("C gold\n");
  // print_matrix(C_gold, m, n);
  // printf("C BME\n");
  // print_matrix(C_bme, m, n);

  r = i32_compare(C_bme, C_gold, m*n);
  if (r) {
    printf("Failure in MM M, N, K = %d %d %d\n", m, n, k);
    exit(1);
  }
  printf("SUCCESS in M, N, K = %d %d %d\n", m, n, k);
  return 0;
}
