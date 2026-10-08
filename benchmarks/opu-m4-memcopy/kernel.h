#include <stdio.h>
#include <riscv-pk/encoding.h>
#include <riscv_vector.h>
#include <stdint.h>
#include <stdlib.h>
#include "bme.h"


void i32_load_c(int32_t* c, size_t ml, size_t N) {
    for (size_t r = 0; r < ml; r++) {
        asm volatile("vle32.v v0, (%0)" : : "r"(&c[r*N]));
        VMV_RV(m2, r, v0); // move v0 into row r of m1
    }
  }

void i32_store_c(int32_t* c, size_t ml, size_t N) {
    for (size_t r = 0; r < ml; r++) {
      VMV_VR(v0, r, m2); // move row r of m0 into v0
      asm volatile("vse32.v v0, (%0)" : : "r"(&c[r*N]));
    }
  }
    
void i32_lm2_load_c(int32_t* c, size_t ml, size_t N) {
  for (size_t r = 0; r < ml; r++) {
    asm volatile("vle32.v v0, (%0)" : : "r"(&c[r*N]));
    VMV_RV(m0, r, v0); // move v0 into row r of m1
    asm volatile("vle32.v v4, (%0)" : : "r"(&c[r*N + ml]));
    VMV_RV(m1, r, v4); // move v0 into row r of m1
    asm volatile("vle32.v v8, (%0)" : : "r"(&c[(r+ml)*N]));
    VMV_RV(m2, r, v8); // move v0 into row r of m1
    asm volatile("vle32.v v12, (%0)" : : "r"(&c[(r+ml)*N + ml]));
    VMV_RV(m3, r, v12); // move v0 into row r of m1
  }
}
void i32_lm2_store_c(int32_t* c, size_t ml, size_t N) {
        // asm volatile("vsetvli zero, %0, e32, m8, ta, ma" : : "r"(ml));
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
  
void i32_bme_memcopy(int32_t* c_in, int32_t* c_out, size_t M, size_t N) {
  size_t mlmax, vl;
  asm volatile("vsetvli %0, zero, e32, m4, ta, ma" : "=r"(mlmax));
  size_t i = 0;
  while (i < M) {
    size_t ml = (M - i) > mlmax ? mlmax : M - i;
    size_t j = 0;
    while (j < N) {
      asm volatile("vsetvli %0, %1, e32, m4, ta, ma" : "=r"(vl) : "r"(N - j));
      i32_lm2_load_c(&c_in[(i*N)+j], ml, N);
      i32_lm2_store_c(&c_out[(i*N)+j], ml, N);
      j += 2*vl;
    }
    i += 2*ml;
  }
}
