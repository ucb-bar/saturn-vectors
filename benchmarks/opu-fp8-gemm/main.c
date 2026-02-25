#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "rvv_mx.h"
#include "bme.h"

extern size_t M;
extern size_t N;
extern size_t K;
size_t avl;
size_t vl;

#define TEST_DATA(type, name, otype) \
    extern type name ## _at[] __attribute__((aligned(64))); \
    extern type name ## _b[] __attribute__((aligned(64))); \
    extern otype name ## _c[] __attribute__((aligned(64))); \
    extern otype name ## _out[] __attribute__((aligned(64)));;

void verify_result(uint32_t* out, uint32_t* ref, size_t M, size_t N) {
    for (size_t i = 0; i < M; i++) {
        for (size_t j = 0; j < N; j++) {
            float res = out[i*N+j];
            float gold = ref[i*N+j];
            if (res != gold) {
                // Print gold and out as bit strings: sign_exp_sig format
                printf("MISMATCH: m = %d, n = %d, gold = %x, out = %x\n", i, j, (uint32_t)gold, (uint32_t)res);

                // Convert gold to bit string
                {
                    uint32_t g = (uint32_t)gold;
                    printf("gold: %u_", (g >> 31) & 0x1);      // sign
                    for (int b = 30; b >= 23; --b)            // exponent
                        printf("%u", (g >> b) & 0x1);
                    printf("_");
                    for (int b = 22; b >= 0; --b)             // significand
                        printf("%u", (g >> b) & 0x1);
                    printf("\n");
                }
                // Convert out(res) to bit string
                {
                    uint32_t r = (uint32_t)res;
                    printf("out : %u_", (r >> 31) & 0x1);      // sign
                    for (int b = 30; b >= 23; --b)            // exponent
                        printf("%u", (r >> b) & 0x1);
                    printf("_");
                    for (int b = 22; b >= 0; --b)             // significand
                        printf("%u", (r >> b) & 0x1);
                    printf("\n");
                }
                return;
            }
        }
    }
    printf("passed\n");
}

void mm_opu(uint8_t* A, uint8_t* B, uint32_t* C, size_t M, size_t N, size_t K, size_t altfmt) {
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
            asm volatile("vsetvli x0, %[avl], e8, m1, ta, ma" : : [avl]"r"(M-i));
            asm volatile("vle8.v v0, (%0)" : : "r"(&A[M*k+i]));
            if (altfmt == 0) {
                asm volatile("vsetvli x0, %[avl], e8, m1, ta, ma" : : [avl]"r"(N-j));
                // VSETVLI_ALTFMT_X0(N-j, SEW_E8, LMUL_M1, 0);
            }
            else {
                VSETVLI_ALTFMT_X0(N-j, SEW_E8, LMUL_M1, 1);
            }
            asm volatile("vle8.v v1, (%0)" : : "r"(&B[N*k+j]));
            OPFMACC(m1, v1, v0);
        }
    
        // move row of c-tile to v-reg, accmulate wth c-row from memory, store back out
        asm volatile("vsetvli x0, %[avl], e32, m4, ta, ma" : : [avl]"r"(cols));
        for (size_t r = 0; r < rows; r++) {
            VMV_VR(v0, r, m1);
            // asm volatile("vle32.v v4, (%0)" : : "r"(&C[(i+r)*N+j]));
            // asm volatile("vadd.vv v0, v0, v4");
            asm volatile("vse32.v v0, (%0)" : : "r"(&C[(i+r)*N+j]));
        }
        j += cols;
        }
        i += rows;
    }
}
  
// Declare test data for all mode combinations
#define DECLARE_PAIR(ma, mb) \
    TEST_DATA(uint8_t, e4m3_##ma##_##mb, uint32_t) \
    TEST_DATA(uint8_t, e5m2_##ma##_##mb, uint32_t)

#define DECLARE_ALL_B(ma) \
    DECLARE_PAIR(ma, inf) \
    DECLARE_PAIR(ma, ninf) \
    DECLARE_PAIR(ma, nan) \
    DECLARE_PAIR(ma, nnan) \
    DECLARE_PAIR(ma, zero) \
    DECLARE_PAIR(ma, nzero) \
    DECLARE_PAIR(ma, rand)

DECLARE_ALL_B(rand)

// Struct + arrays to enable runtime for-loops over all combinations
#define NUM_MODES 7

typedef struct {
    uint8_t *at;
    uint8_t *b;
    uint32_t *out;
    uint32_t *c;
} test_data_t;

#define E4M3_ENTRY(ma, mb) { e4m3_##ma##_##mb##_at, e4m3_##ma##_##mb##_b, e4m3_##ma##_##mb##_out, e4m3_##ma##_##mb##_c },
#define E5M2_ENTRY(ma, mb) { e5m2_##ma##_##mb##_at, e5m2_##ma##_##mb##_b, e5m2_##ma##_##mb##_out, e5m2_##ma##_##mb##_c },

#define ENTRIES_ROW(ENTRY, ma) \
    ENTRY(ma, inf) ENTRY(ma, ninf) ENTRY(ma, nan) ENTRY(ma, nnan) ENTRY(ma, zero) ENTRY(ma, nzero) ENTRY(ma, rand)

#define ALL_ENTRIES(ENTRY) \
    ENTRIES_ROW(ENTRY, rand)

test_data_t e4m3_tests[] = { ALL_ENTRIES(E4M3_ENTRY) };
test_data_t e5m2_tests[] = { ALL_ENTRIES(E5M2_ENTRY) };

const char *mode_names[] = {"inf", "ninf", "nan", "nnan", "zero", "nzero", "rand"};

int main() {

    for (int i = 0; i < NUM_MODES; i++) {
        printf("running e4m3_rand_%s\n", mode_names[i]);
        mm_opu(e4m3_tests[i].at, e4m3_tests[i].b, e4m3_tests[i].out, M, N, K, 0);
        verify_result(e4m3_tests[i].out, e4m3_tests[i].c, M, N);

        printf("running e5m2_rand_%s\n", mode_names[i]);
        mm_opu(e5m2_tests[i].at, e5m2_tests[i].b, e5m2_tests[i].out, M, N, K, 1);
        verify_result(e5m2_tests[i].out, e5m2_tests[i].c, M, N);
    }

    return 0;
}