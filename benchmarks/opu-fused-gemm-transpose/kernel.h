#include <stdio.h>
#include <riscv-pk/encoding.h>
#include <riscv_vector.h>
#include <stdint.h>
#include <stdlib.h>
#include "bme.h"

void i8_loop_k_general(int32_t* c, int8_t* at, int8_t* b, size_t M, size_t N, size_t K, size_t ml, size_t vl) {
  asm volatile("vsetvli zero, %0, e32, m4, ta, ma" : : "r"(vl));
  asm volatile("vle32.v v0, (%0)" : : "r"(c));
  // OPMVINBCAST(m0, v0); // move v0 into row r of m1
  OPMVINBCAST(mc0, v0); // move v0 into column r of m1
  for (size_t k = 0; k < K; k++) {
    asm volatile("vsetvli zero, %0, e8, m1, ta, ma" : : "r"(ml));
    asm volatile("vle8.v v5, (%0)" : : "r"(&at[k*M]));
    asm volatile("vsetvli zero, %0, e8, m1, ta, ma" : : "r"(vl));
    asm volatile("vle8.v v4, (%0)" : : "r"(&b[k*N]));
    VOPACC(m0, v5, v4);
  }
}
void i32_store_ct(int32_t* c, size_t ml, size_t vl, size_t M) {
  asm volatile("vsetvli zero, %0, e32, m4, ta, ma" : : "r"(ml));
  for (size_t r = 0; r < vl; r++) {
    VMV_VR(v0, r, m0); // move row r of m0 into v0
    asm volatile("vse32.v v0, (%0)" : : "r"(&c[r*M]));
  }
}
  
void i8_sq_loop_k(int32_t* c, int8_t* at, int8_t* b, size_t M, size_t N, size_t K, size_t vl) {
  asm volatile("vsetvli zero, %0, e8, m1, ta, ma" : : "r"(vl));
  asm volatile("vle32.v v0, (%0)" : : "r"(c));
  // OPMVINBCAST(m0, v0); // move v0 into row r of m1
  OPMVINBCAST(mc0, v0); // move v0 into column r of m1
  size_t k = 0;
  while (k + 2 <= K) {
      asm volatile("vle8.v v16, (%0)" : : "r"(&at[k*M]));
      asm volatile("vle8.v v18, (%0)" : : "r"(&b[k*N]));
      VOPACC(m0, v16, v18);
      k++;
      asm volatile("vle8.v v20, (%0)" : : "r"(&at[k*M]));
      asm volatile("vle8.v v22, (%0)" : : "r"(&b[k*N]));
      VOPACC(m0, v20, v22);
      k++;
  }
  if (k < K) {    
    asm volatile("vle8.v v16, (%0)" : : "r"(&at[k*M]));
    asm volatile("vle8.v v18, (%0)" : : "r"(&b[k*N]));
    VOPACC(m0, v16, v18);
  }
}
void i32_sq_store_ct(int32_t* c, size_t ml, size_t M) {
  for (size_t r = 0; r < ml; r++) {
    VMV_VR(v0, r, m0); // move row r of m0 into v0
    asm volatile("vse32.v v0, (%0)" : : "r"(&c[r*M]));
  }
}

void i8_mm_bme_sq(int32_t* c_bias, int32_t* c_out, int8_t* at, int8_t* b, size_t M, size_t N, size_t K) {
  size_t mlmax;
  asm volatile("vsetvli %0, zero, e8, m1, ta, ma" : "=r"(mlmax));
  size_t vl;
  size_t i = 0;
  while (i + mlmax <= M) {
    size_t j = 0;

    while (j + mlmax <= N) {
      i8_sq_loop_k(&c_bias[j], &at[i], &b[j], M, N, K, mlmax);
      i32_sq_store_ct(&c_out[j*M + i], mlmax, M);
      j += mlmax;
    }
    while (j < N) {
      asm volatile("vsetvli %0, %1, e32, m4, ta, ma" : "=r"(vl) : "r"(N - j));
      i8_loop_k_general(&c_bias[j], &at[i], &b[j], M, N, K, mlmax, vl);
      i32_store_ct(&c_out[j*M + i], mlmax, vl, M);
      j += vl;
    }
    i += mlmax;
  }
  while (i < M) {
    size_t ml = (M - i) > mlmax ? mlmax : M - i;
    for (size_t j = 0; j < N;) {
      asm volatile("vsetvli %0, %1, e32, m4, ta, ma" : "=r"(vl) : "r"(N - j));
      i8_loop_k_general(&c_bias[j], &at[i], &b[j], M, N, K, ml, vl);
      i32_store_ct(&c_out[j*M + i], ml, vl, M);
      j += vl;
    }
    i += ml;
  }
}
