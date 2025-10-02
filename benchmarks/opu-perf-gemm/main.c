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

void i32_load_c(int32_t* c, size_t ml) {
  asm volatile("vle32.v v0, (%0)" : : "r"(c));
  for (size_t r = 0; r < ml; r++) {
    VMV_RV(m0, r, v0); // move v0 into row r of m1
  }
}
void i32_set_c(int32_t* c) {
  asm volatile("vle32.v v0, (%0)" : : "r"(c));
  OPMVINBCAST(m0, v0); // move v0 into row r of m1
}
void i8_loop_k_general(int8_t* at, int8_t* b, size_t M, size_t N, size_t K, size_t ml, size_t vl) {
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

void i32_2x2_set_c(int32_t* c, size_t ml) {
  asm volatile("vle32.v v0, (%0)" : : "r"(c));
  asm volatile("vle32.v v4, (%0)" : : "r"(c+ml));
  OPMVINBCAST(m0, v0); //broadcast bias to each column of m0
  OPMVINBCAST(m2, v0);
  OPMVINBCAST(m1, v4);
  OPMVINBCAST(m3, v4);
}
void i8_2x2_loop_k(int8_t* at, int8_t* b, size_t ml, size_t M, size_t N, size_t K) {
  asm volatile("vsetvli zero, %0, e8, m1, ta, ma" : : "r"(ml));
  size_t k;
  for (k = 0; k < K; k++) {
    asm volatile("vle8.v v16, (%0)" : : "r"(&at[k*M]));
    asm volatile("vle8.v v17, (%0)" : : "r"(&b[k*N]));
    VOPACC(m0, v17, v16);
    asm volatile("vle8.v v19, (%0)" : : "r"(&b[k*N + ml]));
    VOPACC(m1, v19, v16);
    asm volatile("vle8.v v18, (%0)" : : "r"(&at[k*M + ml]));
    VOPACC(m2, v17, v18);
    VOPACC(m3, v19, v18);
  }
}
void i32_2x2_store_c(int32_t* c, size_t ml, size_t N) {
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

void i8_mm_bme_2x2(int32_t* c_bias, int32_t* c_out, int8_t* at, int8_t* b, size_t M, size_t N, size_t K) {
  size_t mlmax;
  asm volatile("vsetvli %0, zero, e8, m1, ta, ma" : "=r"(mlmax));

  size_t vl;
  size_t i = 0;
  while (i + 2*mlmax <= M) {
    size_t j = 0;

    while (j + 2*mlmax <= N) {
      asm volatile("vsetvli %0, zero, e32, m4, ta, ma" : "=r"(mlmax));
      i32_2x2_set_c(&c_bias[j], mlmax);
      i8_2x2_loop_k(&at[i], &b[j], mlmax, M, N, K);
      i32_2x2_store_c(&c_out[i*N+j], mlmax, N);
      j += 2*mlmax;
    }

    while (j < N) {
      asm volatile("vsetvli %0, %1, e32, m4, ta, ma" : "=r"(vl) : "r"(N - j));
      i32_set_c(&c_bias[j]);
      i8_loop_k_general(&at[i], &b[j], M, N, K, mlmax, vl);
      i32_store_c(&c_out[i*N+j], mlmax, vl, N);
      //unroll by 2 in M dimension
      i32_set_c(&c_bias[j]);
      i8_loop_k_general(&at[i+mlmax], &b[j], M, N, K, mlmax, vl);
      i32_store_c(&c_out[(i+mlmax)*N+j], mlmax, vl, N);
      j += vl;
    }
    i += 2*mlmax;
  }
  
  while (i < M) {
    size_t ml = (M - i) > mlmax ? mlmax : M - i;
    for (size_t j = 0; j < N;) {
      asm volatile("vsetvli %0, %1, e32, m4, ta, ma" : "=r"(vl) : "r"(N - j));
      i32_set_c(&c_bias[j]);
      // i32_load_c(&c_bias[j], ml);
      i8_loop_k_general(&at[i], &b[j], M, N, K, ml, vl);
      i32_store_c(&c_out[i*N+j], ml, vl, N);
      j += vl;
    }
    i += ml;
  }
}

void i32_init(int32_t* d, size_t s) {
  for (size_t i = 0; i < s; i++) {
    d[i] = i + 1;
  }
}

void i8_init(int8_t* d, size_t s, int8_t start) {
    for (size_t i = 0; i < s; i++) {
      d[i] = (i + start) % 127; // avoid overflow
    }
  }

int i32_compare(int32_t* c_opu, int32_t* c_ref, size_t m, size_t n) {
    for (size_t i = 0; i < m; i++) {
      for (size_t j = 0; j < n; j++) {
        size_t index = i * n + j;
        if (c_opu[index] != c_ref[index]) {
          printf("DIVERGENCE at index (%ld, %ld): opu=0x%x =/= ref=0x%x\n", i, j, c_opu[index], c_ref[index]);
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
    
    size_t n = 2*maxvl; 
    size_t m = 2*maxvl;
    const size_t K = 64;
    unsigned long cyclest1, cyclest2;
    int8_t at[m*K];
    int8_t b[n*K];
    int32_t c_opu[m*n];
    int32_t c_bias[n];
    i32_init(c_bias, n);
    i8_init(at, m*K, 1);
    i8_init(b, n*K, -3);
    
    // warm up cache
    i8_mm_bme_2x2(c_bias, c_opu, at, b, m, n, K);  
    for (size_t k = 32; k <= K; k+=32) {
        cyclest1 = read_csr(mcycle);
        i8_mm_bme_2x2(c_bias, c_opu, at, b, m, n, k);  
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