#include <stdio.h>
#include <riscv-pk/encoding.h>
#include <riscv_vector.h>
#include <stdint.h>
#include <stdlib.h>
#include "bme.h"

void i8_loop_k_general(int32_t* c, int8_t* at, int8_t* b, size_t M, size_t N, size_t K, size_t ml, size_t vl) {
    asm volatile("vle32.v v0, (%0)" : : "r"(c));
    OPMVINBCAST(m3, v0); // move v0 into row r of m1
    for (size_t k = 0; k < K; k++) {
        asm volatile("vsetvli zero, %0, e8, m1, ta, ma" : : "r"(ml));
        asm volatile("vle8.v v5, (%0)" : : "r"(&at[k*M]));
        asm volatile("vsetvli zero, %0, e8, m1, ta, ma" : : "r"(vl));
        asm volatile("vle8.v v4, (%0)" : : "r"(&b[k*N]));
        VOPACC(m3, v4, v5);
        // vopacc md=m3, vs2=v0, vs1=v8
    }
}
void i32_store_c(int32_t* c, size_t ml, size_t vl, size_t N) {
  asm volatile("vsetvli zero, %0, e32, m4, ta, ma" : : "r"(vl));
  for (size_t r = 0; r < ml; r++) {
    VMV_VR(v0, r, m3); // move row r of m3 into v0
    asm volatile("vse32.v v0, (%0)" : : "r"(&c[r*N]));
  }
}
void i8_1x2_prologue(int32_t* c, int8_t* at, int8_t* b, size_t ml, size_t M, size_t N, size_t K) {
    asm volatile("vsetvli zero, %0, e32, m4, ta, ma" : : "r"(ml));
    asm volatile("vle32.v v0, (%0)" : : "r"(&c[0]));
    OPMVINBCAST(m0, v0); // move v0 into row r of m1
    asm volatile("vle32.v v4, (%0)" : : "r"(&c[ml]));
    OPMVINBCAST(m1, v4); // move v0 into row r of m1
    
    asm volatile("vsetvli zero, %0, e8, m1, ta, ma" : : "r"(ml));
    for (size_t k = 0; k+2 <= K; k+=2) {
        asm volatile("vle8.v v8, (%0)" : : "r"(&at[k*M]));
        asm volatile("vle8.v v9, (%0)" : : "r"(&b[k*N]));
        VOPACC(m0, v9, v8);

        asm volatile("vle8.v v10, (%0)" : : "r"(&b[k*N + ml]));
        VOPACC(m1, v10, v8);

        //unroll in k to avoid vrf raw hazards
        asm volatile("vle8.v v11, (%0)" : : "r"(&at[(k+1)*M]));
        asm volatile("vle8.v v12, (%0)" : : "r"(&b[(k+1)*N]));
        VOPACC(m0, v12, v11);

        asm volatile("vle8.v v13, (%0)" : : "r"(&b[(k+1)*N + ml]));
        VOPACC(m1, v13, v11);
    }
}
void i8_1x2_body(int32_t* c, int8_t* at, int8_t* b, size_t ml, size_t M, size_t N, size_t K) {
    asm volatile("vsetvli zero, %0, e32, m4, ta, ma" : : "r"(ml));
    asm volatile("vle32.v v0, (%0)" : : "r"(&c[0]));
    OPMVINBCAST(m2, v0); // move v0 into row r of m3
    asm volatile("vle32.v v4, (%0)" : : "r"(&c[ml]));
    OPMVINBCAST(m3, v4); // move v0 into row r of m3
    for (size_t r = 0; r + 2 <= ml; r += 2) {
        //store c[i-1]
        VMV_VR(v0, r, m0); // move row r of m3 into v0
        asm volatile("vse32.v v0, (%0)" : : "r"(&c[r*N]));
        
        //opacc c[i]
        asm volatile("vle8.v v8, (%0)" : : "r"(&at[r*M]));
        asm volatile("vle8.v v9, (%0)" : : "r"(&b[r*N]));
        VOPACC(m2, v9, v8);
        asm volatile("vle8.v v10, (%0)" : : "r"(&b[r*N + ml]));
        VOPACC(m3, v10, v8);

        //store c[i-1]
        VMV_VR(v4, r+1, m0); // move row r of m3 into v0
        asm volatile("vse32.v v4, (%0)" : : "r"(&c[(r+1)*N]));
        
        //opacc c[i]
        asm volatile("vle8.v v11, (%0)" : : "r"(&at[(r+1)*M]));
        asm volatile("vle8.v v12, (%0)" : : "r"(&b[(r+1)*N]));
        VOPACC(m2, v12, v11);
        asm volatile("vle8.v v13, (%0)" : : "r"(&b[(r+1)*N + ml]));
        VOPACC(m3, v13, v11);
    }
    for (size_t k = ml; k+2 <= K; k+=2) {
        asm volatile("vle8.v v8, (%0)" : : "r"(&at[k*M]));
        asm volatile("vle8.v v9, (%0)" : : "r"(&b[k*N]));
        VOPACC(m2, v9, v8);

        asm volatile("vle8.v v10, (%0)" : : "r"(&b[k*N + ml]));
        VOPACC(m3, v10, v8);

        //unroll in k to avoid vrf raw hazards
        asm volatile("vle8.v v11, (%0)" : : "r"(&at[(k+1)*M]));
        asm volatile("vle8.v v12, (%0)" : : "r"(&b[(k+1)*N]));
        VOPACC(m2, v12, v11);

        asm volatile("vle8.v v13, (%0)" : : "r"(&b[(k+1)*N + ml]));
        VOPACC(m3, v13, v11);
    }

    asm volatile("vle32.v v0, (%0)" : : "r"(&c[0]));
    OPMVINBCAST(m0, v0); // move v0 into row r of m3
    asm volatile("vle32.v v4, (%0)" : : "r"(&c[ml]));
    OPMVINBCAST(m1, v4); // move v0 into row r of m3
    for (size_t r = 0; r + 2 <= ml; r += 2) {
        //store c[i-1]
        VMV_VR(v0, r, m2); // move row r of m3 into v0
        asm volatile("vse32.v v0, (%0)" : : "r"(&c[r*N]));
        
        //opacc c[i]
        asm volatile("vle8.v v8, (%0)" : : "r"(&at[r*M]));
        asm volatile("vle8.v v9, (%0)" : : "r"(&b[r*N]));
        VOPACC(m0, v9, v8);
        asm volatile("vle8.v v10, (%0)" : : "r"(&b[r*N + ml]));
        VOPACC(m1, v10, v8);

        //store c[i-1]
        VMV_VR(v4, r+1, m3); // move row r of m3 into v0
        asm volatile("vse32.v v4, (%0)" : : "r"(&c[(r+1)*N]));
        
        //opacc c[i]
        asm volatile("vle8.v v11, (%0)" : : "r"(&at[(r+1)*M]));
        asm volatile("vle8.v v12, (%0)" : : "r"(&b[(r+1)*N]));
        VOPACC(m1, v12, v11);
        asm volatile("vle8.v v13, (%0)" : : "r"(&b[(r+1)*N + ml]));
        VOPACC(m0, v13, v11);
    }
    for (size_t k = ml; k+2 <= K; k+=2) {
        asm volatile("vle8.v v8, (%0)" : : "r"(&at[k*M]));
        asm volatile("vle8.v v9, (%0)" : : "r"(&b[k*N]));
        VOPACC(m0, v9, v8);
        asm volatile("vle8.v v10, (%0)" : : "r"(&b[k*N + ml]));
        VOPACC(m1, v10, v8);
        //unroll in k to avoid vrf raw hazards
        asm volatile("vle8.v v11, (%0)" : : "r"(&at[(k+1)*M]));
        asm volatile("vle8.v v12, (%0)" : : "r"(&b[(k+1)*N]));
        VOPACC(m0, v12, v11);
        asm volatile("vle8.v v13, (%0)" : : "r"(&b[(k+1)*N + ml]));
        VOPACC(m1, v13, v11);
    }
}
void i32_1x2_epilogue(int32_t* c, size_t ml, size_t N) {
    asm volatile("vsetvli zero, %0, e32, m4, ta, ma" : : "r"(ml));
    for (size_t r = 0; r < ml; r++) {
      VMV_VR(v0, r, m2); // move row r of m3 into v0
      asm volatile("vse32.v v0, (%0)" : : "r"(&c[r*N]));
      VMV_VR(v4, r, m3); // move row r of m3 into v0
      asm volatile("vse32.v v4, (%0)" : : "r"(&c[r*N + ml]));
    }
  }
  
void i8_mm_bme_1x2(int32_t* c_bias, int32_t* c_out, int8_t* at, int8_t* b, size_t M, size_t N, size_t K) {
  size_t mlmax;
  asm volatile("vsetvli %0, zero, e8, m1, ta, ma" : "=r"(mlmax));
  size_t vl;
  size_t i = 0;
  while (i + 2*mlmax <= M) {
    size_t j = 0;
    i8_1x2_prologue(&c_bias[j], &at[i], &b[j], mlmax, M, N, K);
    while (j + 2*mlmax <= N) {
      asm volatile("vsetvli %0, zero, e32, m4, ta, ma" : "=r"(mlmax));
      i8_1x2_body(&c_bias[j], &at[i], &b[j], mlmax, M, N, K);
      j += 2*mlmax;
    }
    i32_1x2_epilogue(&c_out[(i*N)+j], mlmax, N);

    while (j < N) {
      asm volatile("vsetvli %0, %1, e32, m4, ta, ma" : "=r"(vl) : "r"(N - j));
      i8_loop_k_general(&c_bias[j], &at[i], &b[j], M, N, K, mlmax, vl);
      i32_store_c(&c_out[(i*N)+j], mlmax, vl, N);
      j += vl;
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