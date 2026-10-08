#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include "../common/rvv_mx.h"

extern size_t N;
size_t avl;
size_t vl;

#define TEST_DATA(type, name, otype) \
	extern type name[] __attribute__((aligned(64))); \
	extern otype name ## _out[] __attribute__((aligned(64))); \
	type *name ## _; \
	otype *name ## _out_; \
	int name ## _neq; \
	otype name ## _res;

/*
	name - float name
	isew - input sew
	osew - output sew
	esew - operation sew
	ealt - operation sew
	ivle - RVV instruction for loading the input type
	ovle - RVV instruction for loading the output type
	ilmul- LMUL of input (1 if isew == esew, 2 if isew == 2 * esew)
	olmul- LMUL of output (1 if osew == esew, 2 if osew == 2 * esew)
	op   - operation, using v0 as the input and v24 as the output
	rm   - RISC-V rounding mode written to frm: 0 RNE, 1 RTZ, 2 RDN, 3 RUP, 4 RMM.
	       RVV floating-point instructions take their rounding mode from fcsr.frm,
	       there is no per-instruction rm field, so it must be set here.
*/
#define TEST(name, rm, isew, osew, esew, ealt, ivle, ovle, ilmul, olmul, op) \
	printf("Testing " #name "\n"); \
	asm volatile("csrwi frm, " #rm); \
	avl = N; \
	vl = 0; \
	name ## _ = name; /* input pointer */ \
	name ## _out_ = name ## _out; /* result pointer */ \
	while (avl > 0) { \
		VSETVLI_ALTFMT(vl, avl, isew, ilmul, 0); /* vsetvli */ \
		asm volatile(ivle " v0, (%0)" : : "r"(name ## _)); /* load A */ \
		VSETVLI_ALTFMT_X0(vl, esew, LMUL_M1, ealt); /* vsetvli */ \
		op; /* operation */ \
        VSETVLI_ALTFMT_X0(vl, osew, olmul, 0); /* vsetvli */ \
		asm volatile(ovle " v8, (%0)" : : "r"(name ## _out_)); /* load result */ \
		asm volatile("vmsne.vv v16, v24, v8"); /* compare */ \
		asm volatile("vfirst.m %0, v16" : "=r"(name ## _neq)); /* extract comparison */ \
		name ## _ += vl; /* increment input pointer */ \
		name ## _out_ += vl; /* increment result pointer */ \
		if (name ## _neq != -1) { /* fail if not equal */ \
			printf("Test failed\n"); \
			printf("Index: %d\n", avl); \
			printf("%d\n", name ## _neq); \
			for (size_t i = 0; i < vl; i ++) { \
				asm volatile("vmv.x.s %0, v24" : "=r"(name ## _res)); \
				printf("%#010x\n", name ## _res); \
				asm volatile("vmv.x.s %0, v8" : "=r"(name ## _res)); \
				printf("%#010x\nNext\n", name ## _res); \
				asm volatile("vslidedown.vi v24, v24, 1"); \
				asm volatile("vslidedown.vi v8, v8, 1"); \
			} \
			exit(1); \
		} \
		avl -= vl; \
	}

/* Narrowing is what exercises rounding, so each narrowing conversion carries one
   array pair per mode. The inputs are identical across modes, so a mismatch
   isolates the rounding mode rather than the operand. */
#define TEST_DATA_FRM(type, name, otype) \
	TEST_DATA(type, name ## _rne, otype) \
	TEST_DATA(type, name ## _rtz, otype) \
	TEST_DATA(type, name ## _rdn, otype) \
	TEST_DATA(type, name ## _rup, otype) \
	TEST_DATA(type, name ## _rmm, otype)

#define TEST_FRM(name, isew, osew, esew, ealt, ivle, ovle, ilmul, olmul, op) \
	TEST(name ## _rne, 0, isew, osew, esew, ealt, ivle, ovle, ilmul, olmul, op) \
	TEST(name ## _rtz, 1, isew, osew, esew, ealt, ivle, ovle, ilmul, olmul, op) \
	TEST(name ## _rdn, 2, isew, osew, esew, ealt, ivle, ovle, ilmul, olmul, op) \
	TEST(name ## _rup, 3, isew, osew, esew, ealt, ivle, ovle, ilmul, olmul, op) \
	TEST(name ## _rmm, 4, isew, osew, esew, ealt, ivle, ovle, ilmul, olmul, op)

TEST_DATA_FRM(uint32_t, fp16_narrow, uint16_t)
TEST_DATA_FRM(uint32_t, bf16_narrow, uint16_t)
TEST_DATA_FRM(uint16_t, e5m2_narrow, uint8_t)
TEST_DATA_FRM(uint16_t, e4m3_narrow, uint8_t)

TEST_DATA_FRM(uint16_t, e5m2_narrow_sat, uint8_t)
TEST_DATA_FRM(uint16_t, e4m3_narrow_sat, uint8_t)

TEST_DATA(uint16_t, fp16_widen, uint32_t)
TEST_DATA(uint16_t, bf16_widen, uint32_t)
TEST_DATA(uint8_t, e5m2_widen, uint16_t)
TEST_DATA(uint8_t, e4m3_widen, uint16_t)

int main() {

    TEST_FRM(fp16_narrow, SEW_E32, SEW_E16, SEW_E16, 0, "vle32.v", "vle16.v", LMUL_M2, LMUL_M1, asm volatile("vfncvt.f.f.w v24, v0"))
    TEST_FRM(bf16_narrow, SEW_E32, SEW_E16, SEW_E16, 1, "vle32.v", "vle16.v", LMUL_M2, LMUL_M1, asm volatile("vfncvt.f.f.w v24, v0"))
    TEST_FRM(e5m2_narrow, SEW_E16, SEW_E8, SEW_E8, 1, "vle16.v", "vle8.v", LMUL_M2, LMUL_M1, VFNCVTBF16_F_F_W(V24, V0))
    TEST_FRM(e4m3_narrow, SEW_E16, SEW_E8, SEW_E8, 0, "vle16.v", "vle8.v", LMUL_M2, LMUL_M1, VFNCVTBF16_F_F_W(V24, V0))

    TEST_FRM(e5m2_narrow_sat, SEW_E16, SEW_E8, SEW_E8, 1, "vle16.v", "vle8.v", LMUL_M2, LMUL_M1, VFNCVTBF16_SAT_F_F_W(V24, V0))
    TEST_FRM(e4m3_narrow_sat, SEW_E16, SEW_E8, SEW_E8, 0, "vle16.v", "vle8.v", LMUL_M2, LMUL_M1, VFNCVTBF16_SAT_F_F_W(V24, V0))

	TEST(fp16_widen, 0, SEW_E16, SEW_E32, SEW_E16, 0, "vle16.v", "vle32.v", LMUL_M1, LMUL_M2, asm volatile("vfwcvt.f.f.v v24, v0"))
    TEST(bf16_widen, 0, SEW_E16, SEW_E32, SEW_E16, 1, "vle16.v", "vle32.v", LMUL_M1, LMUL_M2, asm volatile("vfwcvt.f.f.v v24, v0"))
    TEST(e5m2_widen, 0, SEW_E8, SEW_E16, SEW_E8, 1, "vle8.v", "vle16.v", LMUL_M1, LMUL_M2, VFWCVTBF16_F_F_V(V24, V0))
    TEST(e4m3_widen, 0, SEW_E8, SEW_E16, SEW_E8, 0, "vle8.v", "vle16.v", LMUL_M1, LMUL_M2, VFWCVTBF16_F_F_V(V24, V0))

    printf("All tests passed\n");

    return 0;
}