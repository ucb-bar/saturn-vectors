#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "rvv_mx.h"
#include "bme.h"

extern const size_t N;
extern uint8_t a[] __attribute__((aligned(64)));
extern uint8_t b[] __attribute__((aligned(64)));
extern uint32_t r[] __attribute__((aligned(64)));
extern uint32_t rt[] __attribute__((aligned(64)));

#define BLOCK_SIZE 16

void matmul_opu() {
    int cycles_start;
    int cycles_end;
    uint32_t res[N * N];
    memset(res, 0, N * N * sizeof(uint32_t));
    int vl;
    asm volatile("csrr %0, cycle" : "=r"(cycles_start));

    VSETVLI_ALTFMT(vl, N, SEW_E8, LMUL_M1, 0);
    for (int i = 0; i < N; i += vl) {
        for (int j = 0; j < N; j += vl) {
            VSETVLI_ALTFMT_X0(vl, SEW_E32, LMUL_M4, 0);
            asm volatile("vmv.v.i v0, 0");
            OPMVINBCAST("x1", "x0");
            VSETVLI_ALTFMT_X0(vl, SEW_E8, LMUL_M1, 1);
            for (int k = 0; k < N; k ++) {
                asm volatile("vle8.v v0, (%0)" :: "r"(a + i + k * N));
                asm volatile("vle8.v v1, (%0)" :: "r"(b + j + k * N));
                OPFMACC("x1", "x1", "x0");
            }
            VSETVLI_ALTFMT_X0(vl, SEW_E32, LMUL_M4, 0);
            for (int l = 0; l < vl; l ++) {
                OPMVOUT("x0", "x1", l);
                asm volatile("vle32.v v4, (%0)" :: "r"(res + (i + l) * N + j));
                asm volatile("vadd.vv v0, v0, v4");
                asm volatile("vle32.v v0, (%0)" :: "r"(res + (i + l) * N + j));
            }
        }
    }

    asm volatile("fence");
    asm volatile("csrr %0, cycle" : "=r"(cycles_end));
    printf("Cycles (OPU): %d\n", cycles_end - cycles_start);
    // for (int i = 0; i < N * N; i ++) {
    //     if (res[i] != rt[i]) {
    //         printf("Bad value at index %d: got %d, expected %d\n", i, res[i], r[i]);
    //         exit(1);
    //     }
    // }
}

// void matmul_bdot_multi_acc() {
//     int cycles_start;
//     int cycles_end;
//     uint32_t res[N * N];
//     memset(res, 0, N * N * sizeof(uint32_t));
//     int vl;
//     asm volatile("csrr %0, cycle" : "=r"(cycles_start));

//     VSETVLI_ALTFMT(vl, N, SEW_E8, LMUL_M1, 0);
//     for (int i = 0; i < N; i += 8) {
//         int i_N = i * N;
//         for (int j = 0; j < N; j += 8) {
//             int j_N = j * N;
//             uint32_t *res_base = res + i_N + j;
//             VDOTSETZEROBC_VV();
//             VSETVLI_ALTFMT_X0(vl, SEW_E8, LMUL_M1, 0);
//             for (int k = 0; k < N; k += vl) {
//                 uint8_t *a_base = a + k + i_N;
//                 uint8_t *b_base = b + k + j_N;
//                 // Load VS2
//                 asm volatile("vle8.v v0, (%0)" :: "r"(b_base + 0 * N));
//                 asm volatile("vle8.v v1, (%0)" :: "r"(b_base + 1 * N));
//                 asm volatile("vle8.v v2, (%0)" :: "r"(b_base + 2 * N));
//                 asm volatile("vle8.v v3, (%0)" :: "r"(b_base + 3 * N));
//                 asm volatile("vle8.v v4, (%0)" :: "r"(b_base + 4 * N));
//                 asm volatile("vle8.v v5, (%0)" :: "r"(b_base + 5 * N));
//                 asm volatile("vle8.v v6, (%0)" :: "r"(b_base + 6 * N));
//                 asm volatile("vle8.v v7, (%0)" :: "r"(b_base + 7 * N));
//                 // Load VS1 and accumulate
//                 asm volatile("vle8.v v8, (%0)" :: "r"(a_base + 0 * N));
//                 VQBDOTUA_VV(X0, V0, V8);
//                 asm volatile("vle8.v v9, (%0)" :: "r"(a_base + 1 * N));
//                 VQBDOTUA_VV(X1, V0, V9);
//                 asm volatile("vle8.v v10, (%0)" :: "r"(a_base + 2 * N));
//                 VQBDOTUA_VV(X2, V0, V10);
//                 asm volatile("vle8.v v11, (%0)" :: "r"(a_base + 3 * N));
//                 VQBDOTUA_VV(X3, V0, V11);
//                 asm volatile("vle8.v v12, (%0)" :: "r"(a_base + 4 * N));
//                 VQBDOTUA_VV(X4, V0, V12);
//                 asm volatile("vle8.v v13, (%0)" :: "r"(a_base + 5 * N));
//                 VQBDOTUA_VV(X5, V0, V13);
//                 asm volatile("vle8.v v14, (%0)" :: "r"(a_base + 6 * N));
//                 VQBDOTUA_VV(X6, V0, V14);
//                 asm volatile("vle8.v v15, (%0)" :: "r"(a_base + 7 * N));
//                 VQBDOTUA_VV(X7, V0, V15);
//             }
//             VSETVLI_ALTFMT_X0(8, SEW_E32, LMUL_M2, 0);
//             VDOTWB_VV(V0, X0);
//             asm volatile("vse32.v v0, (%0)" :: "r"(res_base + 0 * N));
//             VDOTWB_VV(V2, X1);
//             asm volatile("vse32.v v2, (%0)" :: "r"(res_base + 1 * N));
//             VDOTWB_VV(V4, X2);
//             asm volatile("vse32.v v4, (%0)" :: "r"(res_base + 2 * N));
//             VDOTWB_VV(V6, X3);
//             asm volatile("vse32.v v6, (%0)" :: "r"(res_base + 3 * N));
//             VDOTWB_VV(V8, X4);
//             asm volatile("vse32.v v8, (%0)" :: "r"(res_base + 4 * N));
//             VDOTWB_VV(V10, X5);
//             asm volatile("vse32.v v10, (%0)" :: "r"(res_base + 5 * N));
//             VDOTWB_VV(V12, X6);
//             asm volatile("vse32.v v12, (%0)" :: "r"(res_base + 6 * N));
//             VDOTWB_VV(V14, X7);
//             asm volatile("vse32.v v14, (%0)" :: "r"(res_base + 7 * N));
//         }
//     }

//     asm volatile("fence");
//     asm volatile("csrr %0, cycle" : "=r"(cycles_end));
//     printf("Cycles (BDot Multi-Acc): %d\n", cycles_end - cycles_start);
//     for (int i = 0; i < N * N; i ++) {
//         if (res[i] != r[i]) {
//             printf("Bad value at index %d: got %d, expected %d\n", i, res[i], r[i]);
//             exit(1);
//         }
//     }
// }
/*
void matmul_bdot_multi_acc_unroll_m_32() {
    int cycles_start;
    int cycles_end;
    uint32_t res[N * N];
    memset(res, 0, N * N * sizeof(uint32_t));
    int vl;
    asm volatile("csrr %0, cycle" : "=r"(cycles_start));

    VSETVLI_ALTFMT(vl, N, SEW_E8, LMUL_M1, 0);
    for (int j = 0; j < N; j += 8) {
        int j_N = j * N;
        for (int i = 0; i < N; i += 32) {
            int i_N = i * N;
            uint32_t *res_base = res + i_N + j;
            VDOTSETZEROBC_VV();
            VSETVLI_ALTFMT_X0(vl, SEW_E8, LMUL_M1, 0);
            for (int k = 0; k < N; k += vl) {
                uint8_t *a_base = a + k + i_N;
                uint8_t *b_base = b + k + j_N;
                // Load VS2
                asm volatile("vle8.v v0, (%0)" :: "r"(b_base + 0 * N));
                asm volatile("vle8.v v1, (%0)" :: "r"(b_base + 1 * N));
                asm volatile("vle8.v v2, (%0)" :: "r"(b_base + 2 * N));
                asm volatile("vle8.v v3, (%0)" :: "r"(b_base + 3 * N));
                asm volatile("vle8.v v4, (%0)" :: "r"(b_base + 4 * N));
                asm volatile("vle8.v v5, (%0)" :: "r"(b_base + 5 * N));
                asm volatile("vle8.v v6, (%0)" :: "r"(b_base + 6 * N));
                asm volatile("vle8.v v7, (%0)" :: "r"(b_base + 7 * N));
                // Load VS1 and accumulate
                asm volatile("vle8.v v8, (%0)" :: "r"(a_base + 0 * N));
                VQBDOTUA_VV(X0, V0, V8);
                asm volatile("vle8.v v9, (%0)" :: "r"(a_base + 1 * N));
                VQBDOTUA_VV(X1, V0, V9);
                asm volatile("vle8.v v10, (%0)" :: "r"(a_base + 2 * N));
                VQBDOTUA_VV(X2, V0, V10);
                asm volatile("vle8.v v11, (%0)" :: "r"(a_base + 3 * N));
                VQBDOTUA_VV(X3, V0, V11);
                asm volatile("vle8.v v12, (%0)" :: "r"(a_base + 4 * N));
                VQBDOTUA_VV(X4, V0, V12);
                asm volatile("vle8.v v13, (%0)" :: "r"(a_base + 5 * N));
                VQBDOTUA_VV(X5, V0, V13);
                asm volatile("vle8.v v14, (%0)" :: "r"(a_base + 6 * N));
                VQBDOTUA_VV(X6, V0, V14);
                asm volatile("vle8.v v15, (%0)" :: "r"(a_base + 7 * N));
                VQBDOTUA_VV(X7, V0, V15);

                asm volatile("vle8.v v8, (%0)" :: "r"(a_base + 8 * N));
                VQBDOTUA_VV(X8, V0, V8);
                asm volatile("vle8.v v9, (%0)" :: "r"(a_base + 9 * N));
                VQBDOTUA_VV(X9, V0, V9);
                asm volatile("vle8.v v10, (%0)" :: "r"(a_base + 10 * N));
                VQBDOTUA_VV(X10, V0, V10);
                asm volatile("vle8.v v11, (%0)" :: "r"(a_base + 11 * N));
                VQBDOTUA_VV(X11, V0, V11);
                asm volatile("vle8.v v12, (%0)" :: "r"(a_base + 12 * N));
                VQBDOTUA_VV(X12, V0, V12);
                asm volatile("vle8.v v13, (%0)" :: "r"(a_base + 13 * N));
                VQBDOTUA_VV(X13, V0, V13);
                asm volatile("vle8.v v14, (%0)" :: "r"(a_base + 14 * N));
                VQBDOTUA_VV(X14, V0, V14);
                asm volatile("vle8.v v15, (%0)" :: "r"(a_base + 15 * N));
                VQBDOTUA_VV(X15, V0, V15);

                asm volatile("vle8.v v8, (%0)" :: "r"(a_base + 16 * N));
                VQBDOTUA_VV(X16, V0, V8);
                asm volatile("vle8.v v9, (%0)" :: "r"(a_base + 17 * N));
                VQBDOTUA_VV(X17, V0, V9);
                asm volatile("vle8.v v10, (%0)" :: "r"(a_base + 18 * N));
                VQBDOTUA_VV(X18, V0, V10);
                asm volatile("vle8.v v11, (%0)" :: "r"(a_base + 19 * N));
                VQBDOTUA_VV(X19, V0, V11);
                asm volatile("vle8.v v12, (%0)" :: "r"(a_base + 20 * N));
                VQBDOTUA_VV(X20, V0, V12);
                asm volatile("vle8.v v13, (%0)" :: "r"(a_base + 21 * N));
                VQBDOTUA_VV(X21, V0, V13);
                asm volatile("vle8.v v14, (%0)" :: "r"(a_base + 22 * N));
                VQBDOTUA_VV(X22, V0, V14);
                asm volatile("vle8.v v15, (%0)" :: "r"(a_base + 23 * N));
                VQBDOTUA_VV(X23, V0, V15);

                asm volatile("vle8.v v8, (%0)" :: "r"(a_base + 24 * N));
                VQBDOTUA_VV(X24, V0, V8);
                asm volatile("vle8.v v9, (%0)" :: "r"(a_base + 25 * N));
                VQBDOTUA_VV(X25, V0, V9);
                asm volatile("vle8.v v10, (%0)" :: "r"(a_base + 26 * N));
                VQBDOTUA_VV(X26, V0, V10);
                asm volatile("vle8.v v11, (%0)" :: "r"(a_base + 27 * N));
                VQBDOTUA_VV(X27, V0, V11);
                asm volatile("vle8.v v12, (%0)" :: "r"(a_base + 28 * N));
                VQBDOTUA_VV(X28, V0, V12);
                asm volatile("vle8.v v13, (%0)" :: "r"(a_base + 29 * N));
                VQBDOTUA_VV(X29, V0, V13);
                asm volatile("vle8.v v14, (%0)" :: "r"(a_base + 30 * N));
                VQBDOTUA_VV(X30, V0, V14);
                asm volatile("vle8.v v15, (%0)" :: "r"(a_base + 31 * N));
                VQBDOTUA_VV(X31, V0, V15);
            }
            VSETVLI_ALTFMT_X0(8, SEW_E32, LMUL_M2, 0);
            VDOTWB_VV(V0, X0);
            asm volatile("vse32.v v0, (%0)" :: "r"(res_base + 0 * N));
            VDOTWB_VV(V2, X1);
            asm volatile("vse32.v v2, (%0)" :: "r"(res_base + 1 * N));
            VDOTWB_VV(V4, X2);
            asm volatile("vse32.v v4, (%0)" :: "r"(res_base + 2 * N));
            VDOTWB_VV(V6, X3);
            asm volatile("vse32.v v6, (%0)" :: "r"(res_base + 3 * N));
            VDOTWB_VV(V8, X4);
            asm volatile("vse32.v v8, (%0)" :: "r"(res_base + 4 * N));
            VDOTWB_VV(V10, X5);
            asm volatile("vse32.v v10, (%0)" :: "r"(res_base + 5 * N));
            VDOTWB_VV(V12, X6);
            asm volatile("vse32.v v12, (%0)" :: "r"(res_base + 6 * N));
            VDOTWB_VV(V14, X7);
            asm volatile("vse32.v v14, (%0)" :: "r"(res_base + 7 * N));

            VDOTWB_VV(V16, X8);
            asm volatile("vse32.v v16, (%0)" :: "r"(res_base + 8 * N));
            VDOTWB_VV(V18, X9);
            asm volatile("vse32.v v18, (%0)" :: "r"(res_base + 9 * N));
            VDOTWB_VV(V20, X10);
            asm volatile("vse32.v v20, (%0)" :: "r"(res_base + 10 * N));
            VDOTWB_VV(V22, X11);
            asm volatile("vse32.v v22, (%0)" :: "r"(res_base + 11 * N));
            VDOTWB_VV(V24, X12);
            asm volatile("vse32.v v24, (%0)" :: "r"(res_base + 12 * N));
            VDOTWB_VV(V26, X13);
            asm volatile("vse32.v v26, (%0)" :: "r"(res_base + 13 * N));
            VDOTWB_VV(V28, X14);
            asm volatile("vse32.v v28, (%0)" :: "r"(res_base + 14 * N));
            VDOTWB_VV(V30, X15);
            asm volatile("vse32.v v30, (%0)" :: "r"(res_base + 15 * N));

            VDOTWB_VV(V0, X16);
            asm volatile("vse32.v v0, (%0)" :: "r"(res_base + 16 * N));
            VDOTWB_VV(V2, X17);
            asm volatile("vse32.v v2, (%0)" :: "r"(res_base + 17 * N));
            VDOTWB_VV(V4, X18);
            asm volatile("vse32.v v4, (%0)" :: "r"(res_base + 18 * N));
            VDOTWB_VV(V6, X19);
            asm volatile("vse32.v v6, (%0)" :: "r"(res_base + 19 * N));
            VDOTWB_VV(V8, X20);
            asm volatile("vse32.v v8, (%0)" :: "r"(res_base + 20 * N));
            VDOTWB_VV(V10, X21);
            asm volatile("vse32.v v10, (%0)" :: "r"(res_base + 21 * N));
            VDOTWB_VV(V12, X22);
            asm volatile("vse32.v v12, (%0)" :: "r"(res_base + 22 * N));
            VDOTWB_VV(V14, X23);
            asm volatile("vse32.v v14, (%0)" :: "r"(res_base + 23 * N));

            VDOTWB_VV(V16, X24);
            asm volatile("vse32.v v16, (%0)" :: "r"(res_base + 24 * N));
            VDOTWB_VV(V18, X25);
            asm volatile("vse32.v v18, (%0)" :: "r"(res_base + 25 * N));
            VDOTWB_VV(V20, X26);
            asm volatile("vse32.v v20, (%0)" :: "r"(res_base + 26 * N));
            VDOTWB_VV(V22, X27);
            asm volatile("vse32.v v22, (%0)" :: "r"(res_base + 27 * N));
            VDOTWB_VV(V24, X28);
            asm volatile("vse32.v v24, (%0)" :: "r"(res_base + 28 * N));
            VDOTWB_VV(V26, X29);
            asm volatile("vse32.v v26, (%0)" :: "r"(res_base + 29 * N));
            VDOTWB_VV(V28, X30);
            asm volatile("vse32.v v28, (%0)" :: "r"(res_base + 30 * N));
            VDOTWB_VV(V30, X31);
            asm volatile("vse32.v v30, (%0)" :: "r"(res_base + 31 * N));
        }
    }

    asm volatile("fence");
    asm volatile("csrr %0, cycle" : "=r"(cycles_end));
    printf("Cycles (BDot Multi-Acc) (Unroll M=32): %d\n", cycles_end - cycles_start);
    for (int i = 0; i < N * N; i ++) {
        if (res[i] != r[i]) {
            printf("Bad value at index %d: got %d, expected %d\n", i, res[i], r[i]);
            exit(1);
        }
    }
}*/

void matmul_bdot_multi_acc_unroll_m_32_rescheduled() {
    int cycles_start;
    int cycles_end;
    uint32_t res[N * N];
    memset(res, 0, N * N * sizeof(uint32_t));
    int vl;
    asm volatile("csrr %0, cycle" : "=r"(cycles_start));

    VSETVLI_ALTFMT(vl, N, SEW_E8, LMUL_M1, 0);
    for (int j = 0; j < N; j += 8) {
        int j_N = j * N;
        VSETVLI_ALTFMT_X0(8, SEW_E32, LMUL_M2, 0);
        for (int i = 0; i < N; i += 32) {
            int i_N = i * N;
            uint32_t *res_base = res + i_N + j;
            VDOTSETZEROBC_VV();
            VSETVLI_ALTFMT_X0(vl, SEW_E8, LMUL_M1, 0);
            for (int k = 0; k < N; k += vl) {
                uint8_t *a_base = a + k + i_N;
                uint8_t *b_base = b + k + j_N;
                // Load VS2
                asm volatile("vle8.v v0, (%0)" :: "r"(b_base + 0 * N));
                asm volatile("vle8.v v1, (%0)" :: "r"(b_base + 1 * N));
                asm volatile("vle8.v v2, (%0)" :: "r"(b_base + 2 * N));
                asm volatile("vle8.v v3, (%0)" :: "r"(b_base + 3 * N));
                asm volatile("vle8.v v4, (%0)" :: "r"(b_base + 4 * N));
                asm volatile("vle8.v v5, (%0)" :: "r"(b_base + 5 * N));
                asm volatile("vle8.v v6, (%0)" :: "r"(b_base + 6 * N));
                asm volatile("vle8.v v7, (%0)" :: "r"(b_base + 7 * N));
                // Load VS1 and accumulate
                asm volatile("vle8.v v8, (%0)" :: "r"(a_base + 0 * N));
                asm volatile("vle8.v v9, (%0)" :: "r"(a_base + 1 * N));
                asm volatile("vle8.v v10, (%0)" :: "r"(a_base + 2 * N));
                asm volatile("vle8.v v11, (%0)" :: "r"(a_base + 3 * N));
                VQBDOTUA_VV(X0, V0, V8);
                asm volatile("vle8.v v12, (%0)" :: "r"(a_base + 4 * N));
                VQBDOTUA_VV(X1, V0, V9);
                asm volatile("vle8.v v13, (%0)" :: "r"(a_base + 5 * N));
                VQBDOTUA_VV(X2, V0, V10);
                asm volatile("vle8.v v14, (%0)" :: "r"(a_base + 6 * N));
                VQBDOTUA_VV(X3, V0, V11);
                asm volatile("vle8.v v15, (%0)" :: "r"(a_base + 7 * N));
                VQBDOTUA_VV(X4, V0, V12);
                asm volatile("vle8.v v8, (%0)" :: "r"(a_base + 8 * N));
                VQBDOTUA_VV(X5, V0, V13);
                asm volatile("vle8.v v9, (%0)" :: "r"(a_base + 9 * N));
                VQBDOTUA_VV(X6, V0, V14);
                asm volatile("vle8.v v10, (%0)" :: "r"(a_base + 10 * N));
                VQBDOTUA_VV(X7, V0, V15);
                
                asm volatile("vle8.v v11, (%0)" :: "r"(a_base + 11 * N));
                VQBDOTUA_VV(X8, V0, V8);
                asm volatile("vle8.v v12, (%0)" :: "r"(a_base + 12 * N));
                VQBDOTUA_VV(X9, V0, V9);
                asm volatile("vle8.v v13, (%0)" :: "r"(a_base + 13 * N));
                VQBDOTUA_VV(X10, V0, V10);
                asm volatile("vle8.v v14, (%0)" :: "r"(a_base + 14 * N));
                VQBDOTUA_VV(X11, V0, V11);
                asm volatile("vle8.v v15, (%0)" :: "r"(a_base + 15 * N));
                VQBDOTUA_VV(X12, V0, V12);
                asm volatile("vle8.v v8, (%0)" :: "r"(a_base + 16 * N));
                VQBDOTUA_VV(X13, V0, V13);
                asm volatile("vle8.v v9, (%0)" :: "r"(a_base + 17 * N));
                VQBDOTUA_VV(X14, V0, V14);
                asm volatile("vle8.v v10, (%0)" :: "r"(a_base + 18 * N));
                VQBDOTUA_VV(X15, V0, V15);

                asm volatile("vle8.v v11, (%0)" :: "r"(a_base + 19 * N));
                VQBDOTUA_VV(X16, V0, V8);
                asm volatile("vle8.v v12, (%0)" :: "r"(a_base + 20 * N));
                VQBDOTUA_VV(X17, V0, V9);
                asm volatile("vle8.v v13, (%0)" :: "r"(a_base + 21 * N));
                VQBDOTUA_VV(X18, V0, V10);
                asm volatile("vle8.v v14, (%0)" :: "r"(a_base + 22 * N));
                VQBDOTUA_VV(X19, V0, V11);
                asm volatile("vle8.v v15, (%0)" :: "r"(a_base + 23 * N));
                VQBDOTUA_VV(X20, V0, V12);
                asm volatile("vle8.v v8, (%0)" :: "r"(a_base + 24 * N));
                VQBDOTUA_VV(X21, V0, V13);
                asm volatile("vle8.v v9, (%0)" :: "r"(a_base + 25 * N));
                VQBDOTUA_VV(X22, V0, V14);
                asm volatile("vle8.v v10, (%0)" :: "r"(a_base + 26 * N));
                VQBDOTUA_VV(X23, V0, V15);

                asm volatile("vle8.v v11, (%0)" :: "r"(a_base + 27 * N));
                VQBDOTUA_VV(X24, V0, V8);
                asm volatile("vle8.v v12, (%0)" :: "r"(a_base + 28 * N));
                VQBDOTUA_VV(X25, V0, V9);
                asm volatile("vle8.v v13, (%0)" :: "r"(a_base + 29 * N));
                VQBDOTUA_VV(X26, V0, V10);
                asm volatile("vle8.v v14, (%0)" :: "r"(a_base + 30 * N));
                VQBDOTUA_VV(X27, V0, V11);
                asm volatile("vle8.v v15, (%0)" :: "r"(a_base + 31 * N));
                VQBDOTUA_VV(X28, V0, V12);
                VQBDOTUA_VV(X29, V0, V13);
                VQBDOTUA_VV(X30, V0, V14);
                VQBDOTUA_VV(X31, V0, V15);
            }
            VSETVLI_ALTFMT_X0(8, SEW_E32, LMUL_M2, 0);
            VDOTWB_VV(V0, X0);
            asm volatile("vse32.v v0, (%0)" :: "r"(res_base + 0 * N));
            VDOTWB_VV(V2, X1);
            asm volatile("vse32.v v2, (%0)" :: "r"(res_base + 1 * N));
            VDOTWB_VV(V4, X2);
            asm volatile("vse32.v v4, (%0)" :: "r"(res_base + 2 * N));
            VDOTWB_VV(V6, X3);
            asm volatile("vse32.v v6, (%0)" :: "r"(res_base + 3 * N));
            VDOTWB_VV(V8, X4);
            asm volatile("vse32.v v8, (%0)" :: "r"(res_base + 4 * N));
            VDOTWB_VV(V10, X5);
            asm volatile("vse32.v v10, (%0)" :: "r"(res_base + 5 * N));
            VDOTWB_VV(V12, X6);
            asm volatile("vse32.v v12, (%0)" :: "r"(res_base + 6 * N));
            VDOTWB_VV(V14, X7);
            asm volatile("vse32.v v14, (%0)" :: "r"(res_base + 7 * N));

            VDOTWB_VV(V16, X8);
            asm volatile("vse32.v v16, (%0)" :: "r"(res_base + 8 * N));
            VDOTWB_VV(V18, X9);
            asm volatile("vse32.v v18, (%0)" :: "r"(res_base + 9 * N));
            VDOTWB_VV(V20, X10);
            asm volatile("vse32.v v20, (%0)" :: "r"(res_base + 10 * N));
            VDOTWB_VV(V22, X11);
            asm volatile("vse32.v v22, (%0)" :: "r"(res_base + 11 * N));
            VDOTWB_VV(V24, X12);
            asm volatile("vse32.v v24, (%0)" :: "r"(res_base + 12 * N));
            VDOTWB_VV(V26, X13);
            asm volatile("vse32.v v26, (%0)" :: "r"(res_base + 13 * N));
            VDOTWB_VV(V28, X14);
            asm volatile("vse32.v v28, (%0)" :: "r"(res_base + 14 * N));
            VDOTWB_VV(V30, X15);
            asm volatile("vse32.v v30, (%0)" :: "r"(res_base + 15 * N));

            VDOTWB_VV(V0, X16);
            asm volatile("vse32.v v0, (%0)" :: "r"(res_base + 16 * N));
            VDOTWB_VV(V2, X17);
            asm volatile("vse32.v v2, (%0)" :: "r"(res_base + 17 * N));
            VDOTWB_VV(V4, X18);
            asm volatile("vse32.v v4, (%0)" :: "r"(res_base + 18 * N));
            VDOTWB_VV(V6, X19);
            asm volatile("vse32.v v6, (%0)" :: "r"(res_base + 19 * N));
            VDOTWB_VV(V8, X20);
            asm volatile("vse32.v v8, (%0)" :: "r"(res_base + 20 * N));
            VDOTWB_VV(V10, X21);
            asm volatile("vse32.v v10, (%0)" :: "r"(res_base + 21 * N));
            VDOTWB_VV(V12, X22);
            asm volatile("vse32.v v12, (%0)" :: "r"(res_base + 22 * N));
            VDOTWB_VV(V14, X23);
            asm volatile("vse32.v v14, (%0)" :: "r"(res_base + 23 * N));

            VDOTWB_VV(V16, X24);
            asm volatile("vse32.v v16, (%0)" :: "r"(res_base + 24 * N));
            VDOTWB_VV(V18, X25);
            asm volatile("vse32.v v18, (%0)" :: "r"(res_base + 25 * N));
            VDOTWB_VV(V20, X26);
            asm volatile("vse32.v v20, (%0)" :: "r"(res_base + 26 * N));
            VDOTWB_VV(V22, X27);
            asm volatile("vse32.v v22, (%0)" :: "r"(res_base + 27 * N));
            VDOTWB_VV(V24, X28);
            asm volatile("vse32.v v24, (%0)" :: "r"(res_base + 28 * N));
            VDOTWB_VV(V26, X29);
            asm volatile("vse32.v v26, (%0)" :: "r"(res_base + 29 * N));
            VDOTWB_VV(V28, X30);
            asm volatile("vse32.v v28, (%0)" :: "r"(res_base + 30 * N));
            VDOTWB_VV(V30, X31);
            asm volatile("vse32.v v30, (%0)" :: "r"(res_base + 31 * N));
        }
    }

    asm volatile("fence");
    asm volatile("csrr %0, cycle" : "=r"(cycles_end));
    printf("Cycles (BDot Multi-Acc) (Unroll M=32, rescheduled): %d\n", cycles_end - cycles_start);
    for (int i = 0; i < N * N; i ++) {
        if (res[i] != r[i]) {
            printf("Bad value at index %d: got %d, expected %d\n", i, res[i], r[i]);
            exit(1);
        }
    }
}

// void matmul_bdot_multi_acc_unroll_k_2() {
//     int cycles_start;
//     int cycles_end;
//     uint32_t res[N * N];
//     memset(res, 0, N * N * sizeof(uint32_t));
//     int vl;
//     asm volatile("csrr %0, cycle" : "=r"(cycles_start));

//     VSETVLI_ALTFMT(vl, N, SEW_E8, LMUL_M1, 0);
//     for (int i = 0; i < N; i += 8) {
//         int i_N = i * N;
//         for (int j = 0; j < N; j += 8) {
//             int j_N = j * N;
//             uint32_t *res_base = res + i_N + j;
//             VDOTSETZEROBC_VV();
//             VSETVLI_ALTFMT_X0(vl, SEW_E8, LMUL_M1, 0);
//             for (int k = 0; k < N; k += vl * 2) {
//                 uint8_t *a_base = a + k + i_N;
//                 uint8_t *a_base_2 = a + (k + vl) + i_N;
//                 uint8_t *b_base = b + k + j_N;
//                 uint8_t *b_base_2 = b + (k + vl) + j_N;
//                 // Load VS2
//                 asm volatile("vle8.v v0, (%0)" :: "r"(b_base + 0 * N));
//                 asm volatile("vle8.v v1, (%0)" :: "r"(b_base + 1 * N));
//                 asm volatile("vle8.v v2, (%0)" :: "r"(b_base + 2 * N));
//                 asm volatile("vle8.v v3, (%0)" :: "r"(b_base + 3 * N));
//                 asm volatile("vle8.v v4, (%0)" :: "r"(b_base + 4 * N));
//                 asm volatile("vle8.v v5, (%0)" :: "r"(b_base + 5 * N));
//                 asm volatile("vle8.v v6, (%0)" :: "r"(b_base + 6 * N));
//                 asm volatile("vle8.v v7, (%0)" :: "r"(b_base + 7 * N));
//                 // Load VS2 (high k)
//                 asm volatile("vle8.v v16, (%0)" :: "r"(b_base_2 + 0 * N));
//                 asm volatile("vle8.v v17, (%0)" :: "r"(b_base_2 + 1 * N));
//                 asm volatile("vle8.v v18, (%0)" :: "r"(b_base_2 + 2 * N));
//                 asm volatile("vle8.v v19, (%0)" :: "r"(b_base_2 + 3 * N));
//                 asm volatile("vle8.v v20, (%0)" :: "r"(b_base_2 + 4 * N));
//                 asm volatile("vle8.v v21, (%0)" :: "r"(b_base_2 + 5 * N));
//                 asm volatile("vle8.v v22, (%0)" :: "r"(b_base_2 + 6 * N));
//                 asm volatile("vle8.v v23, (%0)" :: "r"(b_base_2 + 7 * N));
//                 // Load VS1 and accumulate
//                 asm volatile("vle8.v v8, (%0)" :: "r"(a_base + 0 * N));
//                 VQBDOTUA_VV(X0, V0, V8);
//                 asm volatile("vle8.v v9, (%0)" :: "r"(a_base + 1 * N));
//                 VQBDOTUA_VV(X1, V0, V9);
//                 asm volatile("vle8.v v10, (%0)" :: "r"(a_base + 2 * N));
//                 VQBDOTUA_VV(X2, V0, V10);
//                 asm volatile("vle8.v v11, (%0)" :: "r"(a_base + 3 * N));
//                 VQBDOTUA_VV(X3, V0, V11);
//                 asm volatile("vle8.v v12, (%0)" :: "r"(a_base + 4 * N));
//                 VQBDOTUA_VV(X4, V0, V12);
//                 asm volatile("vle8.v v13, (%0)" :: "r"(a_base + 5 * N));
//                 VQBDOTUA_VV(X5, V0, V13);
//                 asm volatile("vle8.v v14, (%0)" :: "r"(a_base + 6 * N));
//                 VQBDOTUA_VV(X6, V0, V14);
//                 asm volatile("vle8.v v15, (%0)" :: "r"(a_base + 7 * N));
//                 VQBDOTUA_VV(X7, V0, V15);
//                 // Load VS1 and accumulate (high k)
//                 asm volatile("vle8.v v24, (%0)" :: "r"(a_base_2 + 0 * N));
//                 VQBDOTUA_VV(X0, V16, V24);
//                 asm volatile("vle8.v v25, (%0)" :: "r"(a_base_2 + 1 * N));
//                 VQBDOTUA_VV(X1, V16, V25);
//                 asm volatile("vle8.v v26, (%0)" :: "r"(a_base_2 + 2 * N));
//                 VQBDOTUA_VV(X2, V16, V26);
//                 asm volatile("vle8.v v27, (%0)" :: "r"(a_base_2 + 3 * N));
//                 VQBDOTUA_VV(X3, V16, V27);
//                 asm volatile("vle8.v v28, (%0)" :: "r"(a_base_2 + 4 * N));
//                 VQBDOTUA_VV(X4, V16, V28);
//                 asm volatile("vle8.v v29, (%0)" :: "r"(a_base_2 + 5 * N));
//                 VQBDOTUA_VV(X5, V16, V29);
//                 asm volatile("vle8.v v30, (%0)" :: "r"(a_base_2 + 6 * N));
//                 VQBDOTUA_VV(X6, V16, V30);
//                 asm volatile("vle8.v v31, (%0)" :: "r"(a_base_2 + 7 * N));
//                 VQBDOTUA_VV(X7, V16, V31);
//             }
//             VSETVLI_ALTFMT_X0(8, SEW_E32, LMUL_M2, 0);
//             VDOTWB_VV("x0", "x0");
//             asm volatile("vse32.v v0, (%0)" :: "r"(res_base + 0 * N));
//             VDOTWB_VV("x2", "x1");
//             asm volatile("vse32.v v2, (%0)" :: "r"(res_base + 1 * N));
//             VDOTWB_VV("x4", "x2");
//             asm volatile("vse32.v v4, (%0)" :: "r"(res_base + 2 * N));
//             VDOTWB_VV("x6", "x3");
//             asm volatile("vse32.v v6, (%0)" :: "r"(res_base + 3 * N));
//             VDOTWB_VV("x8", "x4");
//             asm volatile("vse32.v v8, (%0)" :: "r"(res_base + 4 * N));
//             VDOTWB_VV("x10", "x5");
//             asm volatile("vse32.v v10, (%0)" :: "r"(res_base + 5 * N));
//             VDOTWB_VV("x12", "x6");
//             asm volatile("vse32.v v12, (%0)" :: "r"(res_base + 6 * N));
//             VDOTWB_VV("x14", "x7");
//             asm volatile("vse32.v v14, (%0)" :: "r"(res_base + 7 * N));
//         }
//     }

//     asm volatile("fence");
//     asm volatile("csrr %0, cycle" : "=r"(cycles_end));
//     printf("Cycles (BDot Multi-Acc) (Unroll K=2): %d\n", cycles_end - cycles_start);
//     for (int i = 0; i < N * N; i ++) {
//         if (res[i] != r[i]) {
//             printf("Bad value at index %d: got %d, expected %d\n", i, res[i], r[i]);
//             exit(1);
//         }
//     }
// }

// void matmul_bdot() {
//     int cycles_start;
//     int cycles_end;
//     uint32_t res[N * N];
//     memset(res, 0, N * N * sizeof(uint32_t));
//     int vl;
//     asm volatile("csrr %0, cycle" : "=r"(cycles_start));

//     VSETVLI_ALTFMT(vl, N, SEW_E8, LMUL_M1, 0);
//     for (int i = 0; i < N; i += BLOCK_SIZE) {
//         for (int j = 0; j < N; j += BLOCK_SIZE) {
//             for (int ii = 0; ii < BLOCK_SIZE; ii ++) {
//                 for (int jj = 0; jj < BLOCK_SIZE; jj += 8) {
//                     VDOTSETZERO_VV("x0");
//                     VSETVLI_ALTFMT_X0(vl, SEW_E8, LMUL_M1, 0);
//                     for (int k = 0; k < N; k += vl) {
//                         asm volatile("vle8.v v8, (%0)" :: "r"(a + (i + ii) * N + k));
//                         asm volatile("vle8.v v0, (%0)" :: "r"(b + (j + jj + 0) * N + k));
//                         asm volatile("vle8.v v1, (%0)" :: "r"(b + (j + jj + 1) * N + k));
//                         asm volatile("vle8.v v2, (%0)" :: "r"(b + (j + jj + 2) * N + k));
//                         asm volatile("vle8.v v3, (%0)" :: "r"(b + (j + jj + 3) * N + k));
//                         asm volatile("vle8.v v4, (%0)" :: "r"(b + (j + jj + 4) * N + k));
//                         asm volatile("vle8.v v5, (%0)" :: "r"(b + (j + jj + 5) * N + k));
//                         asm volatile("vle8.v v6, (%0)" :: "r"(b + (j + jj + 6) * N + k));
//                         asm volatile("vle8.v v7, (%0)" :: "r"(b + (j + jj + 7) * N + k));
//                         VQBDOTUA_VV("x0", "x0", "x8");
//                     }
//                     VSETVLI_ALTFMT_X0(8, SEW_E32, LMUL_M2, 0);
//                     VDOTWB_VV("x16", "x0");
//                     asm volatile("vse32.v v16, (%0)" :: "r"(res + (i + ii) * N + j + jj));
//                 }
//             }
//         }
//     }

//     asm volatile("fence");
//     asm volatile("csrr %0, cycle" : "=r"(cycles_end));
//     printf("Cycles (BDot): %d\n", cycles_end - cycles_start);
//     for (int i = 0; i < N * N; i ++) {
//         if (res[i] != r[i]) {
//             printf("Bad value at index %d: got %d, expected %d\n", i, res[i], r[i]);
//             exit(1);
//         }
//     }
// }

// void matmul_vector_inner() {
//     int cycles_start;
//     int cycles_end;
//     uint32_t res[N * N];
//     memset(res, 0, N * N * sizeof(uint32_t));
//     int vl;
//     asm volatile("csrr %0, cycle" : "=r"(cycles_start));

//     VSETVLI_ALTFMT_X0(1, SEW_E32, LMUL_M1, 0);
//     asm volatile("vmv.v.i v28, 0");
//     VSETVLI_ALTFMT(vl, N, SEW_E8, LMUL_M1, 0);
//     for (int i = 0; i < N; i += BLOCK_SIZE) {
//         for (int j = 0; j < N; j += BLOCK_SIZE) {
//             for (int ii = 0; ii < BLOCK_SIZE; ii ++) {
//                 for (int jj = 0; jj < BLOCK_SIZE; jj ++) {
//                     VSETVLI_ALTFMT_X0(vl, SEW_E16, LMUL_M2, 0);
//                     asm volatile("vmv.v.i v0, 0");
//                     VSETVLI_ALTFMT_X0(vl, SEW_E8, LMUL_M1, 0);
//                     for (int k = 0; k < N; k += vl) {
//                         asm volatile("vle8.v v16, (%0)" :: "r"(a + (i + ii) * N + k));
//                         asm volatile("vle8.v v24, (%0)" :: "r"(b + (j + jj) * N + k));
//                         asm volatile("vwmaccu.vv v0, v16, v24");
//                     }
//                     VSETVLI_ALTFMT_X0(vl, SEW_E16, LMUL_M2, 0);
//                     asm volatile("vwredsumu.vs v16, v0, v28");
//                     VSETVLI_ALTFMT_X0(1, SEW_E32, LMUL_M1, 0);
//                     int e0;
//                     asm volatile("vmv.x.s %0, v16" : "=r"(e0));
//                     res[(i + ii) * N + j + jj] = e0;
//                     VSETVLI_ALTFMT_X0(vl, SEW_E8, LMUL_M1, 0);
//                 }
//             }
//         }
//     }

//     asm volatile("fence");
//     asm volatile("csrr %0, cycle" : "=r"(cycles_end));
//     printf("Cycles (Vector Inner): %d\n", cycles_end - cycles_start);
//     for (int i = 0; i < N * N; i ++) {
//         if (res[i] != r[i]) {
//             printf("Bad value at index %d: got %d, expected %d\n", i, res[i], r[i]);
//             exit(1);
//         }
//     }
// }

// void matmul_scalar() {
//     int cycles_start;
//     int cycles_end;
//     uint32_t res[N * N];
//     memset(res, 0, N * N * sizeof(uint32_t));
//     asm volatile("csrr %0, cycle" : "=r"(cycles_start));

//     for (int i = 0; i < N; i += BLOCK_SIZE) {
//         for (int j = 0; j < N; j += BLOCK_SIZE) {
//             for (int k = 0; k < N; k ++) {
//                 for (int ii = 0; ii < BLOCK_SIZE; ii ++) {
//                     for (int jj = 0; jj < BLOCK_SIZE; jj ++) {
//                         res[(i + ii) * N + j + jj] += a[(i + ii) * N + k] * b[(j + jj) * N + k];
//                     }
//                 }
//             }
//         }
//     }

//     asm volatile("fence");
//     asm volatile("csrr %0, cycle" : "=r"(cycles_end));
//     printf("Cycles (Scalar): %d\n", cycles_end - cycles_start);
//     for (int i = 0; i < N * N; i ++) {
//         if (res[i] != r[i]) {
//             printf("Bad value at index %d: got %d, expected %d\n", i, res[i], r[i]);
//             exit(1);
//         }
//     }
// }

int main() {

    // matmul_bdot_multi_acc(); // Warm up cache
    // matmul_opu();
    // matmul_bdot_multi_acc();
    // matmul_bdot_multi_acc_unroll_m_32_k_2();
    // matmul_bdot_multi_acc_unroll_m_32_k_2_rescheduled();

    matmul_bdot_multi_acc_unroll_m_32_rescheduled();

    // matmul_bdot_multi_acc_unroll_m_32();
    // matmul_bdot_multi_acc_unroll_k_2();
    // matmul_bdot();
    // matmul_vector_inner();
    // matmul_scalar();

    exit(0);

    
    int res;
    int a = 128;
    int vl;
    int cycles_start;
    int cycles_end;

    VSETVLI_ALTFMT_X0(a, SEW_E32, LMUL_M2, 0);
    // vd
    asm volatile("vmv.v.i v24, 1");
    VSETVLI_ALTFMT(vl, a, SEW_E8, LMUL_M1, 0);
    // vs2
    asm volatile("vmv.v.i v8, 2");
    asm volatile("vmv.v.i v9, 3");
    asm volatile("vmv.v.i v10, 4");
    asm volatile("vmv.v.i v11, 5");
    asm volatile("vmv.v.i v12, 6");
    asm volatile("vmv.v.i v13, 7");
    asm volatile("vmv.v.i v14, 8");
    asm volatile("vmv.v.i v15, 9");
    // vs1
    VSETVLI_ALTFMT_X0(a, SEW_E8, LMUL_M4, 0);
    asm volatile("vmv.v.i v16, 3");

    // asm volatile("csrr %0, cycle" : "=r"(cycles_start));

    VSETVLI_ALTFMT_X0(8, SEW_E32, LMUL_M1, 0);
    // VDOTSET_VV("x24", "x1");
    VDOTSETZERO_VV(X0);
    VDOTSETZERO_VV(X1);
    VSETVLI_ALTFMT_X0(a, SEW_E8, LMUL_M1, 0);
    VQBDOTUA_VV(X0, V8, V16);
    VQBDOTUA_VV(X1, V8, V16);
    // VQBDOTUA_VV("x8", "x16");
    // VQBDOTUA_VV("x8", "x16");
    // VQLDOTUA_VV("x8", "x16"); // Long dot product
    VSETVLI_ALTFMT_X0(8, SEW_E32, LMUL_M2, 0);
    VDOTWB_VV(V24, X0);
    VDOTWB_VV(V26, X1);
    // VDOTWB_VV("x16", "x1");

    // exit(0);

    // asm volatile("vmv.x.s x0, v24"); // Wait for writeback
    // asm volatile("fence");
    // asm volatile("csrr %0, cycle" : "=r"(cycles_end));

    for (int i = 0; i < 8; i ++) {
        asm volatile("vmv.x.s %0, v24" : "=r"(res));
        printf("Result %d: %d\n", i, res);
        asm volatile("vslidedown.vi v24, v24, 1");
    }

    for (int i = 0; i < 8; i ++) {
        asm volatile("vmv.x.s %0, v26" : "=r"(res));
        printf("Result %d: %d\n", i, res);
        asm volatile("vslidedown.vi v26, v26, 1");
    }

    // exit(0);
    // printf("Cycles: %d\n", cycles_end - cycles_start);
    // printf("VL: %d\n", vl);
    // for (int i = 0; i < 8; i ++) {
    //     asm volatile("vmv.x.s %0, v24" : "=r"(res));
    //     printf("Result %d: %d\n", i, res);
    //     asm volatile("vslidedown.vi v24, v24, 1");
    // }

    // VSETVLI_ALTFMT_X0(a, SEW_E8, LMUL_M2, 1);
    // asm volatile("vmv.v.i v8, 5");
    // asm volatile("vmv.v.i v16, 3");
    // VQLDOTSA_VV("x24", "x8", "x16");
    // // STALL(100);
    // asm volatile("vmv.v.i v16, 4");
    // VQLDOTUA_VV("x24", "x8", "x16");
    // asm volatile("vsetvli zero, a0, e32, m1");
    // asm volatile("vmv.x.s %0, v24" : "=r"(res));
    // printf("Result: %d\n", res);

    return 0;
}