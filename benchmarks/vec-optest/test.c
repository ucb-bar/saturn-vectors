#include <stdio.h>
#include <riscv-pk/encoding.h>
#include <riscv_vector.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// HACK reuse the scalar registers to avoid assembler hacking for now
#define m0 "x0"
#define m1 "x1"
#define m2 "x2"
#define m3 "x3"
#define m4 "x4"
#define m5 "x5"
#define m6 "x6"
#define m7 "x7"

#define v0 "x0"
#define v1 "x1"
#define v2 "x2"
#define v3 "x3"
#define v4 "x4"
#define v5 "x5"
#define v6 "x6"
#define v7 "x7"
#define v8 "x8"
#define v9 "x9"
#define v10 "x10"
#define v11 "x11"
#define v12 "x12"
#define v13 "x13"
#define v14 "x14"
#define v15 "x15"
#define v16 "x16"
#define v17 "x17"
#define v18 "x18"
#define v19 "x19"
#define v20 "x20"
#define v21 "x21"
#define v22 "x22"
#define v23 "x23"
#define v24 "x24"
#define v25 "x25"
#define v26 "x26"
#define v27 "x27"
#define v28 "x28"
#define v29 "x29"
#define v30 "x30"
#define v31 "x31"

// opmvx. f6=b101010, f7=b1010101
#define OPMVIN(md, vs2, rs1) \
  asm volatile(".insn r 0x57, 0x6, 0x55, " md ", %0, " vs2 : : "r"(rs1));

// opmvx. f6=b101110, f7=b1011101
#define OPMVOUT(vd, ms2, rs1) \
  asm volatile(".insn r 0x57, 0x6, 0x5d, " vd ", %0, " ms2 : : "r"(rs1));

// opmvx. f6=b101100, f7=b1011001
#define OPMVINBCAST(md, vs2) \
  asm volatile(".insn r 0x57, 0x6, 0x59, " md ", x0, " vs2);

// opmvv. f6=b101000, f7=b1010001
#define OPMACC(md, vs2, vs1) \
  asm volatile(".insn r 0x57, 0x2, 0x51, " md ", " vs1 ", " vs2);


void i32_init(int32_t* d, size_t s) {
  for (size_t i = 0; i < s; i++) d[i] = i + 1;
}

void i8_init(int8_t* d, size_t s) {
  for (size_t i = 0; i < s; i++) d[i] = 0;
}

int i32_compare(int32_t* C_opu, int32_t* C_ref, size_t m, size_t n) {
  for (size_t i = 0; i < m; i++) {
    for (size_t j = 0; j < n; j++) {
      size_t index = i * n + j;
      if (C_opu[index] != C_ref[index]) {
        printf("DIVERGENCE at index (%ld, %ld): 0x%x != 0x%x\n", i, j, C_opu[index], C_ref[index]);
        printf("opu:\n");
        for (size_t ii = 0; ii < m; ii++) {
          for (size_t jj = 0; jj < n; jj++) {
            printf("0x%x ", C_opu[ii*n + jj]);
          }
          printf("\n");
        }
        printf("reference:\n");
        for (size_t ii = 0; ii < m; ii++) {
          for (size_t jj = 0; jj < n; jj++) {
            printf("0x%x ", C_ref[ii*n + jj]);
          }
          printf("\n");
        }
        return 1;
      }
    }
  }
  return 0;
}

void i8_mm_scalar(int8_t* at, int8_t* b, int32_t* C_bias, int32_t* C_out, size_t M, size_t N, size_t K) {
    for (size_t i = 0; i < M; i++) {
      for (size_t j = 0; j < N; j++) {
        C_out[i*N+j] = C_bias[j];
        for (size_t k = 0; k < K; k++) {
          C_out[i*N+j] += at[k*M+i] * b[k*N+j];
        }
      }
    }
  }


void mm_opu(int8_t* A, int8_t* B, int32_t* C_bias, int32_t* C, size_t M, size_t N, size_t K) {
  size_t maxvl;
  size_t vl;
  asm volatile("vsetvli %[vl], zero, e32, m4, ta, ma" : [vl]"=r"(maxvl));

  size_t i = 0;
  while (i < M) {
    size_t rows;
    asm volatile("vsetvli %[vl], %[avl], e8, m1, ta, ma" : [vl]"=r"(rows) : [avl]"r"(M-i));

    size_t j = 0;
    while (j < N) {
      // Clear the m1 tile
      asm volatile("vsetvli %[vl], x0, e32, m4, ta, ma" : [vl]"=r"(vl));
      asm volatile("vle32.v v0, (%0)" : : "r"(&C_bias[j]));
      // OPMVINBCAST(m1, v0);
      for (size_t r = 0; r < rows; r++) {
        OPMVIN(m0, v0, r); // move v0 into row r of m1
      }
      
      // Set rows/cols to remaining rows/cols using vsetvli
      size_t cols;
      asm volatile("vsetvli %[vl], %[avl], e8, m1, ta, ma" : [vl]"=r"(cols) : [avl]"r"(N-j));

      // do the k-loop
      for (size_t k = 0; k < K; k++) {
        asm volatile("vsetvli x0, %[avl], e8, m1, ta, ma" : : [avl]"r"(N-j));
        asm volatile("vle8.v v1, (%0)" : : "r"(&B[N*k+j]));
        asm volatile("vsetvli x0, %[avl], e8, m1, ta, ma" : : [avl]"r"(M-i));
        asm volatile("vle8.v v0, (%0)" : : "r"(&A[M*k+i]));
        OPMACC(m1, v1, v0);
      }

      // move row of c-tile to v-reg, accmulate wth c-row from memory, store back out
      asm volatile("vsetvli x0, %[avl], e32, m4, ta, ma" : : [avl]"r"(cols));
      for (size_t r = 0; r < rows; r++) {
        OPMVOUT(v0, m1, r);
        asm volatile("vle32.v v4, (%0)" : : "r"(&C[(i+r)*N+j]));
        asm volatile("vadd.vv v0, v0, v4");
        asm volatile("vse32.v v0, (%0)" : : "r"(&C[(i+r)*N+j]));
      }
      j += cols;
    }
    i += rows;
  }
}

int main(void) {
  size_t maxvl;
  asm volatile("vsetvli %[vl], zero, e32, m4, ta, ma" : [vl]"=r"(maxvl));
  size_t dl = maxvl / 2;
  printf("maxvl=%lu; dl=%lu\n", maxvl, dl);

  const size_t M = maxvl;
  const size_t N = maxvl;
  const size_t K = 3;
  int8_t A[M*K];
  int8_t B[N*K];
  int32_t C_opu[M*N];
  int32_t C_ref[M*N];
  int32_t C_bias[N];
  i32_init(C_bias, N);
  i8_init(A, M*K);
  i8_init(B, N*K);

  for (size_t m = dl-1; m < M; m+=dl) {
    for (size_t n = dl-1; n < N; n+=dl) {
      for (size_t k = 1; k < K; k++) {
        printf("Testing M=%ld, N=%ld, K=%ld\n", m, n, k);
        //Test outer product
        mm_opu(A, B, C_bias, C_opu, m, n, k);
        // mm_scalar(A, B, C_ref, m, n, k);
        i8_mm_scalar(A, B, C_bias, C_ref, m, n, k);

        int r = 0;      
          r = i32_compare(C_opu, C_ref, m, n);
          if (r) {
              printf("FAILURE; M, N, K = %ld %ld %ld\n", m, n, k);
              exit(1);
          }

        printf("SUCESS; M, N, K = %ld %ld %ld\n", m, n, k);
      }
    }
  }
  return 0;
  }
