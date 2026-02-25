#include <riscv_vector.h> 
#include <stdint.h>
#include <stdio.h>

int main(void) {
    uint8_t input1[8] = {0, 1, 4, 126, 127, 128, 240, 247};
    uint8_t input2[8] = {248, 255, 42, 170, 85, 15, 143};
    float   output[16];
    
        asm volatile (
        /* a0 = &input, a1 = &output */
        "mv         a0,     %[IN]\n"
        "mv         a1,     %[OUT]\n"

        /* --- paso 1: VL=16, E8 --- */
        "li         t0,     16\n"
        "vsetvli    t0, t0, e8,  m1, tu, mu\n"
        "vle8.v     v2,     (a0)          \n"   /* v2 <- uint8 */

        /* --- paso 2: E8 -> E16 --- */
        "vfwcvt.f.f.v  v4, v2            \n"   /* v4: f16 */

        /* configurar vtype a E16 antes de la 2.ª ampliación           */
        "li         t0,     16\n"
        "vsetvli    t0, t0, e16, m1, tu, mu\n"

        /* --- paso 3: E16 -> E32 --- */
        "vfwcvt.f.f.v  v6, v4            \n"   /* v6: f32 */

        /* --- paso 4: guardar como 32-bit fp --- */
        "li         t0,     16\n"
        "vsetvli    t0, t0, e32, m1, tu, mu\n"
        "vse32.v    v6,     (a1)          \n"
        :
        : [IN] "r"(input1), [OUT] "r"(output)
        : "a0", "a1", "t0",
          /* declaramos los registros vectoriales que tocamos */
          "v2", "v4", "v6",
          "memory"
    );

        asm volatile (
        /* a0 = &input, a1 = &output */
        "mv         a0,     %[IN]\n"
        "mv         a1,     %[OUT]\n"

        /* --- paso 1: VL=16, E8 --- */
        "li         t0,     16\n"
        "vsetvli    t0, t0, e8,  m1, tu, mu\n"
        "vle8.v     v2,     (a0)          \n"   /* v2 <- uint8 */

        /* --- paso 2: E8 -> E16 --- */
        "vfwcvt.f.f.v  v4, v2            \n"   /* v4: f16 */

        /* configurar vtype a E16 antes de la 2.ª ampliación           */
        "li         t0,     16\n"
        "vsetvli    t0, t0, e16, m1, tu, mu\n"

        /* --- paso 3: E16 -> E32 --- */
        "vfwcvt.f.f.v  v6, v4            \n"   /* v6: f32 */

        /* --- paso 4: guardar como 32-bit fp --- */
        "li         t0,     16\n"
        "vsetvli    t0, t0, e32, m1, tu, mu\n"
        "vse32.v    v6,     (a1)          \n"
        :
        : [IN] "r"(input2), [OUT] "r"(output+8)
        : "a0", "a1", "t0",
          /* declaramos los registros vectoriales que tocamos */
          "v2", "v4", "v6",
          "memory"
    );

    /* --- comprobar resultado --- */
    for (int i = 0; i < 16; ++i)
        printf("out[%d] = %d \n", i, (int) (output[i]*10000));

    return 0;

}
