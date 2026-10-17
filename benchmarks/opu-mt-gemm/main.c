#include <stdio.h>
#include <riscv-pk/encoding.h>
#include <riscv_vector.h>
#include <stdint.h>
#include <stdlib.h>
#include "bme.h"
#include "kernel.h"
#include "dataset.h"

#include <riscv-pk/encoding.h>
#include <stdio.h>
#include "marchid.h"

// EDIT THIS
static size_t n_cores = 4;

static void __attribute__((noinline)) barrier()
{
  static volatile int sense;
  static volatile int count;
  static __thread int threadsense;

  __sync_synchronize();

  threadsense = !threadsense;
  if (__sync_fetch_and_add(&count, 1) == n_cores-1)
  {
    count = 0;
    sense = threadsense;
  }
  else while(sense != threadsense)
    ;

  __sync_synchronize();
}

void i8_mm_scalar(int32_t* c_bias, int32_t* c_out, int8_t* at, int8_t* b, size_t M, size_t N, size_t K, size_t lda) {
  for (size_t i = 0; i < M; i++) {
    for (size_t j = 0; j < N; j++) {
      c_out[i*N+j] = c_bias[j];
      for (size_t k = 0; k < K; k++) {
        c_out[i*N+j] += at[k*lda+i] * b[k*N+j];
      }
    }
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

static int32_t c_opu[M_DIM*N_DIM];

void __main(void) {
  size_t mhartid = read_csr(mhartid);
  if (mhartid >= n_cores) while (1);

  barrier();
  for (size_t i = 0; i < n_cores; i++) {
    if (mhartid == i) {
      printf("Hello world from core %lu\n", mhartid);
      size_t maxvl;
      asm volatile("vsetvli %[vl], zero, e32, m4, ta, ma" : [vl]"=r"(maxvl));
      size_t dl = maxvl / 2;
      printf("maxvl=%lu; dl=%lu\n", maxvl, dl);
      printf("Testing M=%ld, N=%ld, K=%ld\n", M_DIM, N_DIM, K_DIM);
      size_t rows_per_core = M_DIM / n_cores;
      size_t m_start = i * rows_per_core;
      size_t m_end = (i == n_cores - 1) ? M_DIM : (i + 1) * rows_per_core;
      printf("m_start=%lu; m_end=%lu\n", m_start, m_end);
      // i8_mm_bme_2x2(c_bias, c_opu, at, b, rows_per_core, N_DIM, K_DIM, m_start, m_end);
      i8_mm_scalar(c_bias, c_opu + m_start * N_DIM, at + m_start, b, m_end - m_start, N_DIM, K_DIM, M_DIM);
      
    }
    barrier();
  }

  if (mhartid == 0) {
    int r = i32_compare(c_opu, verify_data, M_DIM, N_DIM);
    if (r) {
      printf("FAILURE; M, N, K = %ld %ld %ld\n", M_DIM, N_DIM, K_DIM);
      exit(1);
    }
    printf("SUCCESS; M, N, K = %ld %ld %ld\n", M_DIM, N_DIM, K_DIM);
  }

  if (mhartid > 0) while (1);
}

int main(void) {
  __main();
  return 0;
}