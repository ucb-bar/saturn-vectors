// vec-memcpy: RVV memcpy bandwidth benchmark.
//
// Kernel:  vendored in rvv_memcpy.h (see that file for its license).
// Harness: adapted from vec-daxpy/main.cc
//          (Axpy Kernel, Jesus Labarta, Barcelona Supercomputing Center).

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <riscv_vector.h>
#include "util.h"
#include "rvv_memcpy.h"

// Logical memcpy size per run. Set above LLC size for DRAM bandwidth tests.
// Tunable: -DCOPY_BYTES=<n>.
#ifndef COPY_BYTES
#define COPY_BYTES (1 << 22)  // 4 MiB
#endif

uint8_t src[COPY_BYTES];
uint8_t dst[COPY_BYTES];

int main(int argc, char *argv[])
{
  // warmup
  memcpy_vec(dst, src, COPY_BYTES);

  // Start instruction and cycles count of the region of interest
  unsigned long cycles1, cycles2, instr2, instr1;
  instr1 = read_csr(minstret);
  cycles1 = read_csr(mcycle);

  memcpy_vec(dst, src, COPY_BYTES);

  asm volatile("fence");
  // End instruction and cycles count of the region of interest
  instr2 = read_csr(minstret);
  cycles2 = read_csr(mcycle);

  unsigned long cycles = cycles2 - cycles1;
  unsigned long instrs = instr2 - instr1;
  unsigned long traffic_bytes = 2UL * (unsigned long)COPY_BYTES;

  printf("-CSR   NUMBER OF EXEC CYCLES :%lu\n", cycles);
  printf("-CSR   NUMBER OF INSTRUCTIONS EXECUTED :%lu\n", instrs);
  printf("-CSR   BYTES COPIED :%lu\n", (unsigned long)COPY_BYTES);
  printf("-CSR   MEMORY TRAFFIC (BYTES, RD+WR) :%lu\n", traffic_bytes);

  return 0;
}
