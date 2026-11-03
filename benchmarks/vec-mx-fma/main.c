#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include "rvv_mx.h"

extern size_t N;

// .vf arrays hold one scalar per BLOCK elements (gen_data.py's BLOCK). No
// strip crosses a block, so each strip takes its scalar from its first element.
#define BLOCK 16

#define DATA2(type, name, otype) \
	extern type name ## _a[] __attribute__((aligned(64))); \
	extern type name ## _b[] __attribute__((aligned(64))); \
	extern otype name ## _out[] __attribute__((aligned(64)));
#define DATA3(type, name, otype) \
	DATA2(type, name, otype) \
	extern otype name ## _c[] __attribute__((aligned(64)));

/*
	name  - array name
	rm    - RISC-V rounding mode written to frm: 0 RNE, 1 RTZ, 2 RDN, 3 RUP, 4 RMM
	sew   - operand sew; ealt - its altfmt
	osew  - result sew; olmul - result LMUL (M2 when widening)
	vle, ovle - loads for the operand and result types
	loadc - loads c into v24, or nothing
	op    - the operation: a in v0, b in v4 (.vv) or f0 (.vf), c and result in v24
*/
#define TEST(name, rm, sew, ealt, osew, olmul, vle, ovle, loadc, op) { \
	printf("Testing " #name "\n"); \
	asm volatile("csrwi frm, " #rm); \
	size_t avl = N, vl, i = 0; \
	while (avl > 0) { \
		VSETVLI_ALTFMT(vl, avl < BLOCK ? avl : BLOCK, sew, LMUL_M1, 0); \
		asm volatile(vle " v0, (%0)" : : "r"(&name ## _a[i])); \
		asm volatile(vle " v4, (%0)" : : "r"(&name ## _b[i])); \
		/* the scalar, NaN-boxed in the low 32 bits */ \
		uint32_t s = (~0u << (8 * sizeof(name ## _b[0]))) | name ## _b[i]; \
		VSETVLI_ALTFMT_X0(vl, osew, olmul, 0); \
		loadc; \
		VSETVLI_ALTFMT_X0(vl, sew, LMUL_M1, ealt); \
		asm volatile("fmv.w.x f0, %0\n\t" op : : "r"(s) : "f0"); \
		VSETVLI_ALTFMT_X0(vl, osew, olmul, 0); \
		asm volatile(ovle " v8, (%0)" : : "r"(&name ## _out[i])); \
		asm volatile("vmsne.vv v16, v24, v8"); \
		long neq; \
		asm volatile("vfirst.m %0, v16" : "=r"(neq)); \
		if (neq != -1) { \
			printf("Test failed\n"); \
			printf("Element: %d\n", (int)(i + neq)); \
			for (size_t j = 0; j < vl; j++) { \
				unsigned long r; \
				asm volatile("vmv.x.s %0, v24" : "=r"(r)); \
				printf("%#010lx\n", r); \
				asm volatile("vmv.x.s %0, v8" : "=r"(r)); \
				printf("%#010lx\nNext\n", r); \
				asm volatile("vslidedown.vi v24, v24, 1"); \
				asm volatile("vslidedown.vi v8, v8, 1"); \
			} \
			exit(1); \
		} \
		i += vl; \
		avl -= vl; \
	} \
}

#define TEST2(name, rm, sew, ealt, osew, olmul, vle, ovle, op) \
	TEST(name, rm, sew, ealt, osew, olmul, vle, ovle, , op)
#define TEST3(name, rm, sew, ealt, osew, olmul, vle, ovle, op) \
	TEST(name, rm, sew, ealt, osew, olmul, vle, ovle, \
	     asm volatile(ovle " v24, (%0)" : : "r"(&name ## _c[i])), op)

/* Each array is tested once per rounding mode, with the same inputs */
#define FRM(D, type, name, otype) \
	D(type, name ## _rne, otype) D(type, name ## _rtz, otype) D(type, name ## _rdn, otype) \
	D(type, name ## _rup, otype) D(type, name ## _rmm, otype)
#define RUN(T, name, ...) \
	T(name ## _rne, 0, __VA_ARGS__) T(name ## _rtz, 1, __VA_ARGS__) T(name ## _rdn, 2, __VA_ARGS__) \
	T(name ## _rup, 3, __VA_ARGS__) T(name ## _rmm, 4, __VA_ARGS__)

#define DATA_FMA(type, f, wtype) \
	FRM(DATA3, type, f ## _macc, type)  FRM(DATA3, type, f ## _nmacc, type) \
	FRM(DATA3, type, f ## _msac, type)  FRM(DATA3, type, f ## _nmsac, type) \
	FRM(DATA3, type, f ## _madd, type)  FRM(DATA3, type, f ## _wmacc, wtype) \
	FRM(DATA2, type, f ## _mul_vf, type) FRM(DATA2, type, f ## _add_vf, type) \
	FRM(DATA2, type, f ## _rsub_vf, type) FRM(DATA2, type, f ## _wmul_vf, wtype) \
	FRM(DATA2, type, f ## _wadd_vf, wtype) FRM(DATA3, type, f ## _macc_vf, type) \
	FRM(DATA3, type, f ## _wmacc_vf, wtype)

DATA_FMA(uint16_t, fp16, uint32_t)
DATA_FMA(uint16_t, bf16, uint32_t)
DATA_FMA(uint8_t, e4m3, uint16_t)
DATA_FMA(uint8_t, e5m2, uint16_t)

#define TEST_FMA(f, sew, wsew, ealt, vle, wvle) \
	RUN(TEST3, f ## _macc,     sew, ealt, sew,  LMUL_M1, vle, vle,  "vfmacc.vv v24, v0, v4") \
	RUN(TEST3, f ## _nmacc,    sew, ealt, sew,  LMUL_M1, vle, vle,  "vfnmacc.vv v24, v0, v4") \
	RUN(TEST3, f ## _msac,     sew, ealt, sew,  LMUL_M1, vle, vle,  "vfmsac.vv v24, v0, v4") \
	RUN(TEST3, f ## _nmsac,    sew, ealt, sew,  LMUL_M1, vle, vle,  "vfnmsac.vv v24, v0, v4") \
	RUN(TEST3, f ## _madd,     sew, ealt, sew,  LMUL_M1, vle, vle,  "vfmadd.vv v24, v0, v4") \
	RUN(TEST3, f ## _wmacc,    sew, ealt, wsew, LMUL_M2, vle, wvle, "vfwmacc.vv v24, v0, v4") \
	RUN(TEST2, f ## _mul_vf,   sew, ealt, sew,  LMUL_M1, vle, vle,  "vfmul.vf v24, v0, f0") \
	RUN(TEST2, f ## _add_vf,   sew, ealt, sew,  LMUL_M1, vle, vle,  "vfadd.vf v24, v0, f0") \
	RUN(TEST2, f ## _rsub_vf,  sew, ealt, sew,  LMUL_M1, vle, vle,  "vfrsub.vf v24, v0, f0") \
	RUN(TEST2, f ## _wmul_vf,  sew, ealt, wsew, LMUL_M2, vle, wvle, "vfwmul.vf v24, v0, f0") \
	RUN(TEST2, f ## _wadd_vf,  sew, ealt, wsew, LMUL_M2, vle, wvle, "vfwadd.vf v24, v0, f0") \
	RUN(TEST3, f ## _macc_vf,  sew, ealt, sew,  LMUL_M1, vle, vle,  "vfmacc.vf v24, f0, v0") \
	RUN(TEST3, f ## _wmacc_vf, sew, ealt, wsew, LMUL_M2, vle, wvle, "vfwmacc.vf v24, f0, v0")

int main() {
	TEST_FMA(fp16, SEW_E16, SEW_E32, 0, "vle16.v", "vle32.v")
	TEST_FMA(bf16, SEW_E16, SEW_E32, 1, "vle16.v", "vle32.v")
	TEST_FMA(e4m3, SEW_E8,  SEW_E16, 0, "vle8.v",  "vle16.v")
	TEST_FMA(e5m2, SEW_E8,  SEW_E16, 1, "vle8.v",  "vle16.v")

	printf("All tests passed\n");
	return 0;
}
