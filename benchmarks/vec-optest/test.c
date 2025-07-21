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
  for (size_t i = 0; i < s; i++) d[i] = i + 1;
}

int i32_compare(int32_t* a, int32_t* b, size_t s) {
  for (size_t i = 0; i < s; i++) {
    if (a[i] != b[i]) {
      printf("Divergence %d != %d index %ld\n", a[i], b[i], i);
      return 1;
    }
  }
  return 0;
}

void mm_scalar(int8_t* A, int8_t* B, int32_t* C, size_t M, size_t N, size_t K) {
  for (size_t m = 0; m < M; m++) {
    for (size_t n = 0; n < N; n++) {
      for (size_t k = 0; k < K; k++) {
        C[m*N+n] += A[M*k+m] * B[N*k+n];
      }
    }
  }
}

void mm_opu(int8_t* A, int8_t* B, int32_t* C, size_t M, size_t N, size_t K) {
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
      asm volatile("vmv.v.i v0, 0x0");
      OPMVINBCAST(m1, v0);

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

#define VLEN 32
#define MIN 1
#define MAX 39
#define STEP 12

int main(void) {

  int8_t A[MAX*MAX];
  int8_t B[MAX*MAX];
  int32_t C[MAX*MAX];
  int32_t C_gold[MAX*MAX];


  i32_init(C, MAX*MAX);
  i32_init(C_gold, MAX*MAX);
  /* memset(C, 0, sizeof(int32_t)*MAX*MAX); */
  /* memset(C_gold, 0, sizeof(int32_t)*MAX*MAX); */

  size_t vl;
  size_t maxvl;
  asm volatile("vsetvli %[vl], zero, e32, m4, ta, ma" : [vl]"=r"(maxvl));
  asm volatile("vsetvli %[vl], %[avl], e32, m4, ta, ma" : [vl]"=r"(vl) : [avl]"r"(MAX));
  printf("maxvl is %lu\n", maxvl);
  /* for (size_t j = 0; j < 32; j++) { */
  /*   asm volatile("vle32.v v0, (%0)" : : "r"(&C[vl*j])); */
  /*   OPMVIN(m1, v0, j); */
  /* } */

  /* for (size_t j = 0; j < 32; j++) { */
  /*   asm volatile("vle32.v v0, (%0)" : : "r"(&C[vl*j])); */
  /*   OPMVIN(m1, v0, j); */
  /* } */

  /* for (size_t j = 0; j < 32; j++) { */
  /*   OPMVOUT(v0, m1, j); */
  /*   //asm volatile("vse32.v v0, (%0)" : : "r"(&C_gold[vl*j])); */
  /* } */

  /* asm volatile("vle32.v v0, (%0)" : : "r"(&C[0])); */
  /* OPMVINBCAST(m1, v0); */

  /* asm volatile("vle32.v v0, (%0)" : : "r"(&C[vl])); */
  /* OPMVINBCAST(m1, v0); */

  i8_init(A, MAX*MAX);
  i8_init(B, MAX*MAX);

  // flush the DCache of A, B, to avoid coherence traffic with the L1D on the outer-product test
  for (size_t i = 0; i < MAX*MAX; ) {
    asm volatile("vsetvli %[vl], zero, e8, m1, ta, ma" : [vl]"=r"(maxvl));
    asm volatile("vle8.v v0, (%0)" : : "r"(&A[i]));
    asm volatile("vle8.v v0, (%0)" : : "r"(&B[i]));
    i += maxvl;
  }

  //Test outer product
  size_t M = 33;
  size_t N = 33;
  size_t K = 2;
  mm_opu(A, B, C, M, N, K);
  mm_scalar(A, B, C_gold, M, N, K);
  for (size_t i = 0; i < M; i++) {
    for (size_t j = 0; j < N; j++) {
      if (C[i*N+j] != C_gold[i*N+j]) {
        printf("err r != gold %d != %d at (%d, %d)\n", C[i*N+j], C_gold[i*N+j], i, j);
      }
    }
  }

  printf("done\n");
  return 0;
  }
