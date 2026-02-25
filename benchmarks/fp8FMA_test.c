#include <stdint.h>
#include <stdio.h>

int main(void) {
    // You can change these inputs freely.
    uint8_t A[8] = {0x00, 0x38, 0xB8, 0x30, 0xB0, 0x3C, 0xC0, 0x44};
    uint8_t B[8] = {0x00, 0x30, 0xB0, 0x40, 0x44, 0xBC, 0x3A, 0xC8};
    uint16_t OUT[8];

    asm volatile(
        /* a0 = &A, a1 = &B, a2 = &OUT */
        "mv         a0,     %[A]\n"
        "mv         a1,     %[B]\n"
        "mv         a2,     %[OUT]\n"

        /* set vl=8, SEW=8 (load fp8 as bytes) */
        "li         t0,     8\n"
        "vsetvli    t0, t0, e8,  m1, tu, mu\n"
        "vle8.v     v2,     (a0)\n"      /* v2 <- A[8] (fp8 bits) */
        "vle8.v     v3,     (a1)\n"      /* v3 <- B[8] (fp8 bits) */

        /* custom: fp8×fp8 -> fp16 widening multiply */
        "vfwmul.vv  v4, v2, v3\n"        /* v4: fp16 results */

        /* store as 16-bit */
        "li         t0,     8\n"
        "vsetvli    t0, t0, e16, m1, tu, mu\n"
        "vse16.v    v4,     (a2)\n"
        :
        : [A] "r"(A), [B] "r"(B), [OUT] "r"(OUT)
        : "a0","a1","a2","t0","v2","v3","v4","memory"
    );

    for (int i = 0; i < 8; ++i)
        printf("out[%d] = 0x%04x\n", i, OUT[i]);

    return 0;
}
