
#include <riscv_vector.h> 
#include <stdint.h>
#include <stdio.h>

int main(void) {
    uint16_t input[16] = {0, 56, 184, 48, 176, 60, 193, 70, 157, 120, 0, 120, 120, 248, 121, 0};
    float   output[16];
    
        asm volatile (
        /* a0 = &input, a1 = &output */
        "mv         a0,     %[IN]\n"
        "mv         a1,     %[OUT]\n"

        /* --- paso 1: VL=16, E8 --- */
        "li         t0,     16\n"
        "vsetvli    t0, t0, e16,  m1, tu, mu\n"
        "vle16.v     v2,     (a0)          \n"   /* v2 <- uint8 */

        /* --- paso 2: E8 -> E16 --- */
        "vfwcvt.f.f.v  v6, v2            \n"   /* v4: f16 */

        /* --- paso 4: guardar como 32-bit fp --- */
        "li         t0,     16\n"
        "vsetvli    t0, t0, e32, m1, tu, mu\n"
        "vse32.v    v6,     (a1)          \n"
        :
        : [IN] "r"(input), [OUT] "r"(output)
        : "a0", "a1", "t0",
          /* declaramos los registros vectoriales que tocamos */
          "v2", "v4", "v6",
          "memory"
    );

    // /* --- comprobar resultado --- */
    // for (int i = 0; i < 16; ++i)
    //     printf("out[%d] = %d \n", i, (int) output[i]);

    return 0;

}
