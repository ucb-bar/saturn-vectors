#ifndef _PATHFINDER_H_
#define _PATHFINDER_H_

#include <stdint.h>
#include <riscv_vector.h>
#include <stdio.h>

int *run(int *wall, int *result_s, int *src, uint32_t cols, uint32_t rows,
         uint32_t num_runs);

#define DECLARE(l)                                                          \
void run_vectorm##l(int *wall, int *result_v, uint32_t cols, uint32_t rows, \
                    uint32_t num_runs);

DECLARE(1)
DECLARE(2)
DECLARE(4)
DECLARE(8)
#endif
