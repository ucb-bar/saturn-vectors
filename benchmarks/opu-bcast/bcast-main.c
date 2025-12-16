#include <stdio.h>
#include <riscv-pk/encoding.h>
#include <riscv_vector.h>
#include <stdint.h>
#include <stdlib.h>
#include "bme.h"

void i32_set_c_scalar(int32_t* c_bias, int32_t* c_out, size_t N, size_t M) {
    for (size_t i = 0; i < M; i++) {
        for (size_t j = 0; j < N; j++) {
            c_out[i*N+j] = c_bias[j];
        }
    }
}
void i32_set_c(int32_t* c) {
  asm volatile("vle32.v v0, (%0)" : : "r"(c));
  OPMVINBCAST(m3, v0); // move v0 into row r of m1
}
void i32_store_c(int32_t* c_tile, size_t ml, size_t N) {
  for (size_t row = 0; row < ml; row++) {
    VMV_VR(v0, row, m3); // move row row of m3 into v0
    asm volatile("vse32.v v0, (%0)" : : "r"(&c_tile[row*N]));
  }
}
void i32_set_c_opu(int32_t* c_bias, int32_t* c_out, size_t N, size_t M) {
    size_t mlmax;
    asm volatile("vsetvli %0, zero, e8, m1, ta, ma" : "=r"(mlmax));
    size_t vl;
    size_t i = 0;
    while (i < M) {
        size_t ml = (M - i) > mlmax ? mlmax : M - i;
        for (size_t j = 0; j < N;) {
            asm volatile("vsetvli %0, %1, e32, m4, ta, ma" : "=r"(vl) : "r"(N - j));
            printf("i=%ld, j=%ld, ml=%ld, vl=%ld\n", i, j, ml, vl);
            i32_set_c(&c_bias[j]);
            i32_store_c(&c_out[i*N+j], ml, N);
            j += vl;
        }
        i += ml;
    }
}
void i32_init(int32_t* d, size_t s) {
  for (size_t i = 0; i < s; i++) {
    d[i] = i + 0x11;
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
  const size_t N = 2*maxvl;
  int32_t c_opu[M*N];
  int32_t c_ref[M*N];
  int32_t c_bias[M];
  i32_init(c_bias, M);

  for (size_t m = maxvl; m < M; m+=dl) {
    for (size_t n = maxvl; n < N; n+=dl) {
        printf("Testing M=%ld, N=%ld\n", m, n);
        i32_set_c_scalar(c_bias, c_ref, m, n);
        i32_set_c_opu(c_bias, c_opu, m, n);
        
        // verify against reference
        int r = 0;      
          r = i32_compare(c_opu, c_ref, m, n);
          if (r) {
              printf("FAILURE; M, N = %ld %ld\n", m, n);
              exit(1);
          }

        printf("SUCESS; M, N = %ld %ld\n", m, n);
      // }
    }
  }
  return 0;
}