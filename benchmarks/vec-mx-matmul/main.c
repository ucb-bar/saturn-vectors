#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include "rvv_mx.h"

extern size_t M;
extern size_t N;
extern size_t K;
size_t avl;
size_t vl;

#define TEST_DATA(type, name, otype) \
    extern type name ## _a[] __attribute__((aligned(64))); \
    extern type name ## _b[] __attribute__((aligned(64))); \
    extern otype name ## _c[] __attribute__((aligned(64))); \
    extern type name ## _at[] __attribute__((aligned(64))); \
    extern type name ## _bt[] __attribute__((aligned(64))); \
    extern otype name ## _out[] __attribute__((aligned(64)));;

#define TEST_VECTOR_OUTER(name, isew, osew, ealt, ivle, ovse) \
    printf("Testing " #name "\n"); \
    avl = N; \
    { \
        size_t n = 0; \
        while (n < N) { \
            VSETVLI_ALTFMT(vl, avl, isew, LMUL_M1, ealt); \
            for (size_t m = 0; m < M; m += 8) { \
                VSETVLI_ALTFMT_X0(vl * 16, osew, LMUL_M8, 0); \
                asm volatile("vmv.v.x v16, x0"); \
                asm volatile("vmv.v.x v24, x0"); \
                for (size_t k = 0; k < K; k ++) { \
                    VSETVLI_ALTFMT_X0(vl, isew, LMUL_M1, ealt); \
                    asm volatile(ivle " v0, (%0)" : : "r"(name ## _b + n + (k * N))); \
                    asm volatile("vmv.v.x v8, %0" : : "r"(name ## _a[k + (m + 0) * K])); \
                    asm volatile("vmv.v.x v9, %0" : : "r"(name ## _a[k + (m + 1) * K])); \
                    asm volatile("vmv.v.x v10, %0" : : "r"(name ## _a[k + (m + 2) * K])); \
                    asm volatile("vmv.v.x v11, %0" : : "r"(name ## _a[k + (m + 3) * K])); \
                    asm volatile("vmv.v.x v12, %0" : : "r"(name ## _a[k + (m + 4) * K])); \
                    asm volatile("vmv.v.x v13, %0" : : "r"(name ## _a[k + (m + 5) * K])); \
                    asm volatile("vmv.v.x v14, %0" : : "r"(name ## _a[k + (m + 6) * K])); \
                    asm volatile("vmv.v.x v15, %0" : : "r"(name ## _a[k + (m + 7) * K])); \
                    asm volatile("vfwmacc.vv v16, v8, v0"); \
                    asm volatile("vfwmacc.vv v18, v9, v0"); \
                    asm volatile("vfwmacc.vv v20, v10, v0"); \
                    asm volatile("vfwmacc.vv v22, v11, v0"); \
                    asm volatile("vfwmacc.vv v24, v12, v0"); \
                    asm volatile("vfwmacc.vv v26, v13, v0"); \
                    asm volatile("vfwmacc.vv v28, v14, v0"); \
                    asm volatile("vfwmacc.vv v30, v15, v0"); \
                } \
                VSETVLI_ALTFMT_X0(vl, osew, LMUL_M2, 0); \
                asm volatile(ovse " v16, (%0)" : : "r"(name ## _out + n + ((m + 0) * N))); \
                asm volatile(ovse " v18, (%0)" : : "r"(name ## _out + n + ((m + 1) * N))); \
                asm volatile(ovse " v20, (%0)" : : "r"(name ## _out + n + ((m + 2) * N))); \
                asm volatile(ovse " v22, (%0)" : : "r"(name ## _out + n + ((m + 3) * N))); \
                asm volatile(ovse " v24, (%0)" : : "r"(name ## _out + n + ((m + 4) * N))); \
                asm volatile(ovse " v26, (%0)" : : "r"(name ## _out + n + ((m + 5) * N))); \
                asm volatile(ovse " v28, (%0)" : : "r"(name ## _out + n + ((m + 6) * N))); \
                asm volatile(ovse " v30, (%0)" : : "r"(name ## _out + n + ((m + 7) * N))); \
            } \
            avl -= vl; \
            n += vl; \
        } \
    }

#define CLEAR_OUT(name, osew) \
    memset(name ## _out, 0, M * N * osew);

#define CHECK_TEST(name) \
    for (size_t m = 0; m < M; m ++) { \
        for (size_t n = 0; n < N; n ++) { \
            uint64_t expected = name ## _c[n + (m * N)]; \
            uint64_t result = name ## _out[n + (m * N)]; \
            if (expected != result) { \
                printf("Test failed\n"); \
                printf("m = %d, n = %d, exp = %x, res = %x\n", m, n, expected, result); \
            } \
        } \
    }

TEST_DATA(uint8_t, e4m3, uint16_t)
TEST_DATA(uint8_t, e5m2, uint16_t)

int main() {

    TEST_VECTOR_OUTER(e4m3, SEW_E8, SEW_E16, 0, "vle8.v", "vse16.v")
    CHECK_TEST(e4m3)
    TEST_VECTOR_OUTER(e5m2, SEW_E8, SEW_E16, 1, "vle8.v", "vse16.v")
    CHECK_TEST(e5m2)

    printf("All tests passed\n");

    return 0;
}