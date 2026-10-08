#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include "rvv_mx.h"

extern size_t N;
size_t avl;
size_t vl;

#define TEST_DATA(type, name, otype) \
	extern type name ## _a[] __attribute__((aligned(64))); \
	extern type name ## _b[] __attribute__((aligned(64))); \
	extern otype name ## _out[] __attribute__((aligned(64))); \
	type *name ## _a_; \
	type *name ## _b_; \
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
*/
#define TEST(name, isew, osew, esew, ealt, ivle, ovle, ilmul, olmul, op) \
	printf("Testing " #name "\n"); \
	avl = N; \
	vl = 0; \
	name ## _a_ = name ## _a; /* input pointer */ \
	name ## _b_ = name ## _b; /* input pointer */ \
	name ## _out_ = name ## _out; /* result pointer */ \
	while (avl > 0) { \
		VSETVLI_ALTFMT(vl, avl, isew, ilmul, 0); /* vsetvli */ \
		asm volatile(ivle " v0, (%0)" : : "r"(name ## _a_)); /* load A */ \
		asm volatile(ivle " v4, (%0)" : : "r"(name ## _b_)); /* load B */ \
		VSETVLI_ALTFMT_X0(vl, esew, LMUL_M1, ealt); /* vsetvli */ \
		op; /* operation */ \
        VSETVLI_ALTFMT_X0(vl, osew, olmul, 0); /* vsetvli */ \
		asm volatile(ovle " v8, (%0)" : : "r"(name ## _out_)); /* load result */ \
		asm volatile("vmsne.vv v16, v24, v8"); /* compare */ \
		asm volatile("vfirst.m %0, v16" : "=r"(name ## _neq)); /* extract comparison */ \
		name ## _a_ += vl; /* increment input pointer */ \
		name ## _b_ += vl; /* increment input pointer */ \
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

#define TEST_DATA_BINARY_FMA(size, name, wsize) \
	TEST_DATA(size, name ## _mul, size) \
	TEST_DATA(size, name ## _add, size) \
	TEST_DATA(size, name ## _sub, size) \
	TEST_DATA(size, name ## _wmul, wsize) \
	TEST_DATA(size, name ## _wadd, wsize) \
	TEST_DATA(size, name ## _wsub, wsize)

TEST_DATA_BINARY_FMA(uint16_t, fp16, uint32_t)
TEST_DATA_BINARY_FMA(uint16_t, bf16, uint32_t)
TEST_DATA_BINARY_FMA(uint8_t, e4m3, uint16_t)
TEST_DATA_BINARY_FMA(uint8_t, e5m2, uint16_t)

#define TEST_BINARY_FMA(name, sew, wsew, ealt, vle, wvle, ops_name) \
	TEST(name ## _mul, sew, sew, sew, ealt, vle, vle, LMUL_M1, LMUL_M1, asm volatile("vfmul.vv v24, v0, v4")) \
	TEST(name ## _add, sew, sew, sew, ealt, vle, vle, LMUL_M1, LMUL_M1, asm volatile("vfadd.vv v24, v0, v4")) \
	TEST(name ## _sub, sew, sew, sew, ealt, vle, vle, LMUL_M1, LMUL_M1, asm volatile("vfsub.vv v24, v0, v4")) \
	TEST(name ## _wmul, sew, wsew, sew, ealt, vle, wvle, LMUL_M1, LMUL_M2, asm volatile("vfwmul.vv v24, v0, v4")) \
	TEST(name ## _wadd, sew, wsew, sew, ealt, vle, wvle, LMUL_M1, LMUL_M2, asm volatile("vfwadd.vv v24, v0, v4")) \
	TEST(name ## _wsub, sew, wsew, sew, ealt, vle, wvle, LMUL_M1, LMUL_M2, asm volatile("vfwsub.vv v24, v0, v4"))

int main() {
	
	TEST_BINARY_FMA(fp16, SEW_E16, SEW_E32, 0, "vle16.v", "vle32.v", fp16)
	TEST_BINARY_FMA(bf16, SEW_E16, SEW_E32, 1, "vle16.v", "vle32.v", bf16)
	TEST_BINARY_FMA(e4m3, SEW_E8, SEW_E16, 0, "vle8.v", "vle16.v", e4m3)
	TEST_BINARY_FMA(e5m2, SEW_E8, SEW_E16, 1, "vle8.v", "vle16.v", e5m2)

	printf("All tests passed\n");

	return 0;
}
