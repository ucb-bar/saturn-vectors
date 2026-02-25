#include <riscv_vector.h>
#include <stdint.h>
#include <stdio.h>

int main(void) {
    uint8_t input[16] = {0, 1, 4, 126, 127, 128, 240, 247, 248, 255, 42, 170, 85, 15, 143};
    uint16_t half_bits[16]; // float16 raw storage
    float output[16];        // float32 results

    asm volatile (
        /* a0 = &input, a1 = &half_bits, a2 = &output */
        "mv    a0, %[IN]\n"
        "mv    a1, %[HALF]\n"
        "mv    a2, %[OUT]\n"

        /* --- step 1: load 16 uint8 and widen to float16 --- */
        "li    t0, 16\n"
        "vsetvli t0, t0, e8, m1, tu, mu\n"
        "vle8.v  v2, (a0)\n"
        "vfwcvt.f.f.v  v4, v2\n"

        /* --- step 2: store all 16 float16 bit patterns --- */
        "li    t0, 16\n"
        "vsetvli t0, t0, e16, m1, tu, mu\n"
        "vse16.v v4, (a1)\n"

        /* --- step 3: widen first 8 halfs to float32 --- */
        "li    t0, 8\n"
        "vsetvli t0, t0, e16, m1, tu, mu\n"
        "vle16.v v4, (a1)\n"                /* load half_bits[0..7] */
        "li    t0, 8\n"
        "vsetvli t0, t0, e16, m1, tu, mu\n"
        "vfwcvt.f.f.v v6, v4\n"             /* widen to f32 */
        "li    t0, 8\n"
        "vsetvli t0, t0, e32, m1, tu, mu\n"
        "vse32.v v6, (a2)\n"                /* store output[0..7] */

        /* --- step 4: widen next 8 halfs to float32 --- */
        "addi  a1, a1, 16\n"                /* advance half_bits pointer by 8 * 2 bytes */
        "addi  a2, a2, 32\n"                /* advance output pointer by 8 * 4 bytes */
        "li    t0, 8\n"
        "vsetvli t0, t0, e16, m1, tu, mu\n"
        "vle16.v v4, (a1)\n"                /* load half_bits[8..15] */
        "li    t0, 8\n"
        "vsetvli t0, t0, e16, m1, tu, mu\n"
        "vfwcvt.f.f.v v6, v4\n"
        "li    t0, 8\n"
        "vsetvli t0, t0, e32, m1, tu, mu\n"
        "vse32.v v6, (a2)\n"
        :
        : [IN] "r"(input), [HALF] "r"(half_bits), [OUT] "r"(output)
        : "a0", "a1", "a2", "t0", "v2", "v4", "v6", "memory"
    );

    /* --- print results --- */
    printf("=== float16 raw bit patterns ===\n");
    for (int i = 0; i < 16; ++i) {
        printf("half[%2d] = 0x%04x\n", i, (unsigned)half_bits[i]);
    }

    printf("=== float32 scaled outputs ===\n");
    for (int i = 0; i < 16; ++i) {
        printf("out[%2d] = %d\n", i, (int)(output[i] * 10000));
    }

    return 0;
}
