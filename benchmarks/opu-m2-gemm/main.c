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

void i32_load_c(int* c, size_t ml, size_t N) {
  for (size_t r = 0; r < ml; r++) {
    asm volatile("vle32.v v0, (%0)" : : "r"(&c[r*N]));
    VMV_RV(m0, r, v0); // move v0 into row r of m0
  }
}

void i32_store_c(int* c, size_t ml, size_t N) {
  for (size_t r = 0; r < ml; r++) {
    VMV_VR(v0, r, m0); // move row r of m0 into v0
    asm volatile("vse32.v v0, (%0)" : : "r"(&c[r*N]));
  }
}

void i8_loop_k_general(int8_t* at, int8_t* b, size_t M, size_t N, size_t K, size_t ml, size_t vl) {
    for (size_t k = 0; k < K; k++) {
        asm volatile("vsetvli zero, %0, e8, m1, ta, ma" : : "r"(ml));
        asm volatile("vle8.v v0, (%0)" : : "r"(&at[k*M]));
        asm volatile("vsetvli zero, %0, e8, m1, ta, ma" : : "r"(vl));
        asm volatile("vle8.v v4, (%0)" : : "r"(&b[k*N]));
        VOPACC(m0, v4, v0);
        // vopacc md=m0, vs2=v0, vs1=v8
    }
}
void i32_m2_load_c(int* c, size_t ml, size_t N) {
  for (size_t r = 0; r < ml; r++) {
    asm volatile("vle32.v v0, (%0)" : : "r"(&c[r*N]));
    VMV_RV(m0, r, v0); // move v0 into row r of m0
    asm volatile("vle32.v v4, (%0)" : : "r"(&c[r*N + ml])); 
    VMV_RV(m1, r, v4); // move v0 into row r of m0
  }
}

void i8_m2_loop_k(int8_t* at, int8_t* b, size_t ml, size_t M, size_t N, size_t K) {
  size_t k;
  for (k = 0; k+2 <= K; k+=2) {
    asm volatile("vle8.v v16, (%0)" : : "r"(&at[k*M]));
    asm volatile("vle8.v v17, (%0)" : : "r"(&b[k*N]));
    VOPACC(m0, v17, v16);

    asm volatile("vle8.v v18, (%0)" : : "r"(&b[k*N + ml]));
    VOPACC(m1, v18, v16);

    //unroll in k to avoid vrf raw hazards
    asm volatile("vle8.v v19, (%0)" : : "r"(&at[(k+1)*M]));
    asm volatile("vle8.v v20, (%0)" : : "r"(&b[(k+1)*N]));
    VOPACC(m0, v20, v19);

    asm volatile("vle8.v v21, (%0)" : : "r"(&b[(k+1)*N + ml]));
    VOPACC(m1, v21, v19);
  }
  //TODO: handle odd K
}

void i32_m2_store_c(int* c, size_t ml, size_t N) {
  for (size_t r = 0; r < ml; r++) {
    VMV_VR(v8, r, m0); // move row r of m0 into v0
    asm volatile("vse32.v v8, (%0)" : : "r"(&c[r*N]));
    VMV_VR(v12, r, m1); // move row r of m0 into v0
    asm volatile("vse32.v v12, (%0)" : : "r"(&c[r*N + ml]));
  }
}
  
void i8_mm_bme_square(int32_t* c_in, int32_t* c_out, int8_t* at, int8_t* b, size_t M, size_t N, size_t K) {
  size_t vlenb ;
  asm volatile("vsetvli %0, zero, e8, m1, ta, ma" : "=r"(vlenb));
  size_t mlmax = vlenb;

  size_t vl;
  size_t i = 0;
  while (i + mlmax <= M) {
    size_t j = 0;

    while (j + 2*mlmax <= N) {
      asm volatile("vsetvli zero, %0, e32, m4, ta, ma" : : "r"(mlmax));
      i32_m2_load_c(&c_in[(i*N)+j], mlmax, N);
      asm volatile("vsetvli zero, %0, e8, m1, ta, ma" : : "r"(mlmax));
      i8_m2_loop_k(&at[i], &b[j], mlmax, M, N, K);
      asm volatile("vsetvli zero, %0, e32, m4, ta, ma" : : "r"(mlmax));
      i32_m2_store_c(&c_out[(i*N)+j], mlmax, N);
      j += 2*mlmax;
    }

    while (j < N) {
      asm volatile("vsetvli %0, %1, e32, m4, ta, ma" : "=r"(vl) : "r"(N - j));
      i32_load_c(&c_in[(i*N)+j], mlmax, N);
      i8_loop_k_general(&at[i], &b[j], M, N, K, mlmax, vl);
      asm volatile("vsetvli zero, %0, e32, m4, ta, ma" : : "r"(vl));
      i32_store_c(&c_out[(i*N)+j], mlmax, N);
      j += vl;
    }
    i += mlmax;
  }
  
  while (i < M) {
    size_t remaining = M - i;
    size_t ml = remaining > mlmax ? mlmax : remaining;

    for (size_t j = 0; j < N;) {
      asm volatile("vsetvli %0, %1, e32, m4, ta, ma" : "=r"(vl) : "r"(N - j));
      i32_load_c(&c_in[(i*N)+j], ml, N);

      i8_loop_k_general(&at[i], &b[j], M, N, K, ml, vl);

      asm volatile("vsetvli zero, %0, e32, m4, ta, ma" : : "r"(vl));
      i32_store_c(&c_out[(i*N)+j], ml, N);
      j += vl;
    }
    i += ml;
  }
}

void i32_init(int* d, size_t s) {
  for (size_t i = 0; i < s; i++) {
    d[i] = i + 1;
  }
}

void i8_init(int8_t* d, size_t s, int8_t start) {
    for (size_t i = 0; i < s; i++) {
      d[i] = start + i;
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

#define TCM_BASE 0x70000000

#define MIN 64
#define MAX 256
#define STEP 64
#define VL 64
// #define DL 8

int main(void) {
  size_t m = VL;
  size_t n = 2*VL;

  int8_t* B = (int8_t*)TCM_BASE;
  int8_t* At = (int8_t*)(TCM_BASE + n * MAX);
  int32_t C_init[m*n];
  int32_t C_bme[m*n];
  // scalar copy of A, B to avoid D1 coherence delays
  int32_t C_gold[m*n];
  int8_t Ats[m*MAX];
  int8_t Bs[MAX*n];
  i8_init(At, m * MAX, 1);
  i8_init(B, MAX * n, 2);
  i8_init(Ats, m * MAX, 1);
  i8_init(Bs, MAX * n, 2);
  i32_init(C_init, m * n);
  
  printf("i8 GEMM\nvlen = %d;\nwarmup cache:\ndim,ops,cycles\n", VL*8);
  int64_t cyclest1 = read_csr(mcycle);
  i8_mm_bme_square(C_init, C_bme, At, B, m, n, MAX);
  asm volatile("fence");
  int64_t cyclest2 = read_csr(mcycle);
  int64_t cycles = cyclest2 - cyclest1;
  int64_t ops = m * n * MAX;
  
  for (size_t k = MIN; k <= MAX; k += STEP) {
    i8_mm_scalar(C_init, C_gold, Ats, Bs, m, n, k);
    cyclest1 = read_csr(mcycle);
    i8_mm_bme_square(C_init, C_bme, At, B, m, n, k);
    asm volatile("fence");
    cyclest2 = read_csr(mcycle);
    
    // verify against reference
    int r = 0;      
    r = i32_compare(C_bme, C_gold, m, n);
    if (r) {
        printf("Failure in BME M, N, K = %ld %ld %ld\n", m, n, k);
        exit(1);
    }

    // compute metrics and print to csv
    cycles = cyclest2 - cyclest1;
    ops = m * n * k;
    printf("%ld,%ld,%ld\n", k, ops, cycles);
  }
  printf("SUCCESS testing mmBME\n");
  return 0;
}