#include <stdio.h>
#include <riscv-pk/encoding.h>
#include <riscv_vector.h>
#include <stdint.h>
#include <stdlib.h>
#include "bme.h"

void i8_loop_k_general(int32_t* c, int8_t* at, int8_t* b, size_t M, size_t N, size_t K, size_t ml, size_t vl) {
  asm volatile("vle32.v v0, (%0)" : : "r"(c));
  OPMVINBCAST(m0, v0); // move v0 into row r of m1
  for (size_t k = 0; k < K; k++) {
      asm volatile("vsetvli zero, %0, e8, m1, ta, ma" : : "r"(ml));
      asm volatile("vle8.v v5, (%0)" : : "r"(&at[k*M]));
      asm volatile("vsetvli zero, %0, e8, m1, ta, ma" : : "r"(vl));
      asm volatile("vle8.v v4, (%0)" : : "r"(&b[k*N]));
      VOPACC(m0, v4, v5);
      // vopacc md=m0, vs2=v0, vs1=v8
  }
}
void i32_store_c(int32_t* c, size_t ml, size_t vl, size_t N) {
  asm volatile("vsetvli zero, %0, e32, m4, ta, ma" : : "r"(vl));
  for (size_t r = 0; r < ml; r++) {
    VMV_VR(v0, r, m0); // move row r of m0 into v0
    asm volatile("vse32.v v0, (%0)" : : "r"(&c[r*N]));
  }
}
  
void i8_lm2_loop_k(int32_t* c, int8_t* at, int8_t* b, size_t M, size_t N, size_t K, size_t ml) {
  asm volatile("vsetvli zero, %0, e32, m4, ta, ma" : : "r"(ml));
  asm volatile("vle32.v v0, (%0)" : : "r"(c));
  asm volatile("vle32.v v4, (%0)" : : "r"(c+ml));
  OPMVINBCAST(m0, v0); // move v0 into row r of m1
  OPMVINBCAST(m2, v0); // move v0 into row r of m1
  OPMVINBCAST(m1, v4);
  OPMVINBCAST(m3, v4);
  asm volatile("vsetvli zero, %0, e8, m2, ta, ma" : : "r"(2*ml));
  size_t k = 0;
  while (k + 2 <= K) {
      asm volatile("vle8.v v16, (%0)" : : "r"(&at[k*M]));
      asm volatile("vle8.v v18, (%0)" : : "r"(&b[k*N]));
      VOPACC(m0, v18, v16);
      k++;
      asm volatile("vle8.v v20, (%0)" : : "r"(&at[k*M]));
      asm volatile("vle8.v v22, (%0)" : : "r"(&b[k*N]));
      VOPACC(m0, v22, v20);
      k++;
  }
}
void i32_lm2_store_c(int32_t* c, size_t ml, size_t N) {
  // asm volatile("vsetvli zero, %0, e32, m8, ta, ma" : : "r"(ml));
  // for (size_t r = 0; r < ml; r++) {
  //   VMV_VR(v0, r, m0); // move row r of m0 into v0
  //   asm volatile("vse32.v v0, (%0)" : : "r"(&c[r*N]));
  //   VMV_VR(v8, r, m2); 
  //   asm volatile("vse32.v v8, (%0)" : : "r"(&c[(r+ml)*N]));
  // }
// }
// void i32_2x2_store_c(int32_t* c, size_t ml, size_t N) {
  asm volatile("vsetvli zero, %0, e32, m4, ta, ma" : : "r"(ml));
  for (size_t r = 0; r < ml; r++) {
    VMV_VR(v0, r, m0); // move row r of m0 into v0
    asm volatile("vse32.v v0, (%0)" : : "r"(&c[r*N]));
    VMV_VR(v4, r, m1); 
    asm volatile("vse32.v v4, (%0)" : : "r"(&c[r*N + ml]));
    VMV_VR(v8, r, m2); 
    asm volatile("vse32.v v8, (%0)" : : "r"(&c[(r+ml)*N]));
    VMV_VR(v12, r, m3);
    asm volatile("vse32.v v12, (%0)" : : "r"(&c[(r+ml)*N + ml]));
  }
}
void i8_mm_bme_lm2(int32_t* c_bias, int32_t* c_out, int8_t* at, int8_t* b, size_t M, size_t N, size_t K) {
  size_t mlmax;
  asm volatile("vsetvli %0, zero, e8, m1, ta, ma" : "=r"(mlmax));
  size_t vl;
  size_t i = 0;
  while (i + mlmax <= M) {
    size_t j = 0;

    while (j + 2*mlmax <= N) {
      i8_lm2_loop_k(&c_bias[j], &at[i], &b[j], M, N, K, mlmax);
      i32_lm2_store_c(&c_out[(i*N)+j], mlmax, N);
      j += 2*mlmax;
    }
    while (j < N) {
      asm volatile("vsetvli %0, %1, e32, m4, ta, ma" : "=r"(vl) : "r"(N - j));
      i8_loop_k_general(&c_bias[j], &at[i], &b[j], M, N, K, mlmax, vl);
      i32_store_c(&c_out[(i*N)+j], mlmax, vl, N);
      i8_loop_k_general(&c_bias[j], &at[i+mlmax], &b[j], M, N, K, mlmax, vl);
      i32_store_c(&c_out[(i+mlmax)*N+j], mlmax, vl, N);
      j += vl;
    }
    i += 2*mlmax;
  }
  while (i < M) {
    size_t ml = (M - i) > mlmax ? mlmax : M - i;
    for (size_t j = 0; j < N;) {
      asm volatile("vsetvli %0, %1, e32, m4, ta, ma" : "=r"(vl) : "r"(N - j));
      i8_loop_k_general(&c_bias[j], &at[i], &b[j], M, N, K, ml, vl);
      i32_store_c(&c_out[(i*N)+j], ml, vl, N);
      j += vl;
    }
    i += ml;
  }
}
