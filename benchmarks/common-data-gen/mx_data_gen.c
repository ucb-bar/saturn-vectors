#include <stdlib.h>
#include "mx_data_gen.h"
#include "rvv_mx.h"

fp_t random_float(double min, double max) {
    double val = min + (max - min) * ((double) rand() / (double) RAND_MAX);
    return *(fp_t *) &val;
}

// Narrowing conversion

fp_t fp64_to_fp32(fp_t a) {
    fp_t res;
	asm volatile("fmv.d.x f0, %0" :: "r"(a));
	asm volatile("fcvt.s.d f1, f0");
	asm volatile("fmv.x.s %0, f1" : "=r"(res));
	return res & 0xFFFFFFFF;
}

fp_t fp32_to_fp16(fp_t a) {
	fp_t res;
	asm volatile("fmv.s.x f0, %0" :: "r"(a));
	asm volatile("fcvt.h.s f1, f0");
	asm volatile("fmv.x.h %0, f1" : "=r"(res));
	return res & 0xFFFF;
}

fp_t fp32_to_bf16(fp_t a) {
	fp_t res;
	asm volatile("fmv.s.x f0, %0" :: "r"(a));
    FCVT_BF16_S(F1, F0);
	asm volatile("fmv.x.h %0, f1" : "=r"(res));
	return res & 0xFFFF;
}

fp_t bf16_to_e4m3(fp_t a) {
    fp_t res;
    VSETVLI_ALTFMT_X0(1, SEW_E16, LMUL_M1, 0);
    asm volatile("vmv.s.x v0, %0" :: "r"(a));
    VSETVLI_ALTFMT_X0(1, SEW_E8, LMUL_M1, 0);
    VFNCVTBF16_F_F_W(V8, V0);
    asm volatile("vmv.x.s %0, v8" : "=r"(res));
    return res & 0xFF;
}

fp_t bf16_to_e5m2(fp_t a) {
    fp_t res;
    VSETVLI_ALTFMT_X0(1, SEW_E16, LMUL_M1, 0);
    asm volatile("vmv.s.x v0, %0" :: "r"(a));
    VSETVLI_ALTFMT_X0(1, SEW_E8, LMUL_M1, 1);
    VFNCVTBF16_F_F_W(V8, V0);
    asm volatile("vmv.x.s %0, v8" : "=r"(res));
    return res & 0xFF;
}

fp_t bf16_to_e4m3_sat(fp_t a) {
    fp_t res;
    VSETVLI_ALTFMT_X0(1, SEW_E16, LMUL_M1, 0);
    asm volatile("vmv.s.x v0, %0" :: "r"(a));
    VSETVLI_ALTFMT_X0(1, SEW_E8, LMUL_M1, 0);
    VFNCVTBF16_SAT_F_F_W(V8, V0);
    asm volatile("vmv.x.s %0, v8" : "=r"(res));
    return res & 0xFF;
}

fp_t bf16_to_e5m2_sat(fp_t a) {
    fp_t res;
    VSETVLI_ALTFMT_X0(1, SEW_E16, LMUL_M1, 0);
    asm volatile("vmv.s.x v0, %0" :: "r"(a));
    VSETVLI_ALTFMT_X0(1, SEW_E8, LMUL_M1, 1);
    VFNCVTBF16_SAT_F_F_W(V8, V0);
    asm volatile("vmv.x.s %0, v8" : "=r"(res));
    return res & 0xFF;
}

// Widening conversion

fp_t fp16_to_fp32(fp_t a) {
	fp_t res;
	asm volatile("fmv.h.x f0, %0" :: "r"(a));
	asm volatile("fcvt.s.h f1, f0");
	asm volatile("fmv.x.s %0, f1" : "=r"(res));
	return res & 0xFFFFFFFF;
}

fp_t bf16_to_fp32(fp_t a) {
	fp_t res;
	asm volatile("fmv.h.x f0, %0" :: "r"(a));
    FCVT_S_BF16(F1, F0);
	asm volatile("fmv.x.s %0, f1" : "=r"(res));
	return res & 0xFFFFFFFF;
}

fp_t e4m3_to_bf16(fp_t a) {
    fp_t res;
    VSETVLI_ALTFMT_X0(1, SEW_E8, LMUL_M1, 0);
    asm volatile("vmv.s.x v0, %0" :: "r"(a));
    VFWCVTBF16_F_F_V(V8, V0);
    VSETVLI_ALTFMT_X0(1, SEW_E16, LMUL_M1, 0);
    asm volatile("vmv.x.s %0, v8" : "=r"(res));
    return res & 0xFFFF;
}

fp_t e5m2_to_bf16(fp_t a) {
    fp_t res;
    VSETVLI_ALTFMT_X0(1, SEW_E8, LMUL_M1, 1);
    asm volatile("vmv.s.x v0, %0" :: "r"(a));
    VFWCVTBF16_F_F_V(V8, V0);
    VSETVLI_ALTFMT_X0(1, SEW_E16, LMUL_M1, 0);
    asm volatile("vmv.x.s %0, v8" : "=r"(res));
    return res & 0xFFFF;
}

// FMA Binary

#define OP_BINARY(name, op, inst, isew, osew, esew, alt, mask) \
    fp_t name ## _ ## op(fp_t a, fp_t b) { \
        fp_t res; \
        VSETVLI_ALTFMT_X0(1, isew, LMUL_M1, 0); \
        asm volatile("vmv.s.x v0, %0" :: "r"(a)); \
        asm volatile("vmv.s.x v16, %0" :: "r"(b)); \
        VSETVLI_ALTFMT_X0(1, esew, LMUL_M1, alt); \
        asm volatile(inst " v8, v0, v16"); \
        VSETVLI_ALTFMT_X0(1, osew, LMUL_M1, 0); \
        asm volatile("vmv.x.s %0, v8" : "=r"(res)); \
        return res & mask; \
    }

#define OPS_FMA_BINARY(name, sew, wsew, alt, mask, wmask) \
    OP_BINARY(name, mul, "vfmul.vv", sew, sew, sew, alt, mask) \
    OP_BINARY(name, add, "vfadd.vv", sew, sew, sew, alt, mask) \
    OP_BINARY(name, sub, "vfsub.vv", sew, sew, sew, alt, mask) \
    OP_BINARY(name, wmul, "vfwmul.vv", sew, wsew, sew, alt, wmask) \
    OP_BINARY(name, wadd, "vfwadd.vv", sew, wsew, sew, alt, wmask) \
    OP_BINARY(name, wsub, "vfwsub.vv", sew, wsew, sew, alt, wmask)

OPS_FMA_BINARY(fp32, SEW_E32, SEW_E64, 0, 0xFFFFFFFF, 0xFFFFFFFFFFFFFFFF)
OPS_FMA_BINARY(fp16, SEW_E16, SEW_E32, 0, 0xFFFF, 0xFFFFFFFF)
OPS_FMA_BINARY(bf16, SEW_E16, SEW_E32, 1, 0xFFFF, 0xFFFFFFFF)

#define OP_BINARY_MX(name, op, wname) \
    fp_t name ## _ ## op(fp_t a, fp_t b) { \
        fp_t wide_a = name ## _to_ ## wname(a); \
        fp_t wide_b = name ## _to_ ## wname(b); \
        fp_t wide_res = wname ## _ ## op(wide_a, wide_b); \
        return wname ## _to_ ## name(wide_res); \
    }

#define OP_BINARY_MX_WIDEN(name, op, wname) \
    fp_t name ## _w ## op(fp_t a, fp_t b) { \
        fp_t wide_a = name ## _to_ ## wname(a); \
        fp_t wide_b = name ## _to_ ## wname(b); \
        return wname ## _ ## op(wide_a, wide_b); \
    }

#define OP_BINARY_MX_QWIDEN(name, op, wname, qname) \
    fp_t name ## _q ## op(fp_t a, fp_t b) { \
        fp_t wide_a = wname ## _to_ ## qname(name ## _to_ ## wname(a)); \
        fp_t wide_b = wname ## _to_ ## qname(name ## _to_ ## wname(b)); \
        return qname ## _ ## op(wide_a, wide_b); \
    }

#define OPS_FMA_BINARY_MX(name, wname, qname) \
    OP_BINARY_MX(name, mul, wname) \
    OP_BINARY_MX(name, add, wname) \
    OP_BINARY_MX(name, sub, wname) \
    OP_BINARY_MX_WIDEN(name, mul, wname) \
    OP_BINARY_MX_WIDEN(name, add, wname) \
    OP_BINARY_MX_WIDEN(name, sub, wname) \
    OP_BINARY_MX_QWIDEN(name, mul, wname, qname) \
    OP_BINARY_MX_QWIDEN(name, add, wname, qname) \
    OP_BINARY_MX_QWIDEN(name, sub, wname, qname)

OPS_FMA_BINARY_MX(e5m2, bf16, fp32)
OPS_FMA_BINARY_MX(e4m3, bf16, fp32)

// FMA Ternary

fp_t e4m3_wmacc(fp_t a, fp_t b, fp_t c) {
    fp_t prod = e4m3_wmul(a, b);
    return bf16_add(prod, c);
}

fp_t e5m2_wmacc(fp_t a, fp_t b, fp_t c) {
    fp_t prod = e5m2_wmul(a, b);
    return bf16_add(prod, c);
}

fp_t e4m3_qmacc(fp_t a, fp_t b, fp_t c) {
    fp_t prod = e4m3_qmul(a, b);
    return fp32_add(prod, c);
}

fp_t e5m2_qmacc(fp_t a, fp_t b, fp_t c) {
    fp_t prod = e5m2_qmul(a, b);
    return fp32_add(prod, c);
}

// Generation

fp_t gen_fp32(GenMode mode, double min, double max) {
    switch (mode) {
        case GM_INF:
            return 0x7f800000;
        case GM_NINF:
            return 0xff800000;
        case GM_NAN:
            return 0x7fc00000;
        case GM_NNAN:
            return 0xffc00000;
        case GM_ZERO:
            return 0x00000000;
        case GM_NZERO:
            return 0x80000000;
        case GM_RAND:
            return fp64_to_fp32(random_float(min, max));
    }
}

fp_t gen_fp16(GenMode mode, double min, double max) {
    switch (mode) {
        case GM_INF:
            return 0x7c00;
        case GM_NINF:
            return 0xfc00;
        case GM_NAN:
            return 0x7e00;
        case GM_NNAN:
            return 0xfe00;
        case GM_ZERO:
            return 0x0000;
        case GM_NZERO:
            return 0x8000;
        case GM_RAND:
            return fp32_to_fp16(fp64_to_fp32(random_float(min, max)));
    }
}

fp_t gen_bf16(GenMode mode, double min, double max) {
    switch (mode) {
        case GM_INF:
            return 0x7f80;
        case GM_NINF:
            return 0xff80;
        case GM_NAN:
            return 0x7fc0;
        case GM_NNAN:
            return 0xffc0;
        case GM_ZERO:
            return 0x0000;
        case GM_NZERO:
            return 0x8000;
        case GM_RAND:
            return fp32_to_bf16(fp64_to_fp32(random_float(min, max)));
    }
}

fp_t gen_e4m3(GenMode mode, double min, double max) {
    switch (mode) {
        case GM_INF:
            return 0x7f;
        case GM_NINF:
            return 0xff;
        case GM_NAN:
            return 0x7f;
        case GM_NNAN:
            return 0xff;
        case GM_ZERO:
            return 0x00;
        case GM_NZERO:
            return 0x80;
        case GM_RAND:
            return bf16_to_e4m3(fp32_to_bf16(fp64_to_fp32(random_float(min, max))));
    }
}

fp_t gen_e5m2(GenMode mode, double min, double max) {
    switch (mode) {
        case GM_INF:
            return 0x7c;
        case GM_NINF:
            return 0xfc;
        case GM_NAN:
            return 0x7f;
        case GM_NNAN:
            return 0xff;
        case GM_ZERO:
            return 0x00;
        case GM_NZERO:
            return 0x80;
        case GM_RAND:
            return bf16_to_e5m2(fp32_to_bf16(fp64_to_fp32(random_float(min, max))));
    }
}

// Formatting

void print_header() {
    printf(".section .data,\"aw\",@progbits\n");
}

void print_scratchpad(char *name, char *suffix, size_t len) {
    printf(".global %s%s\n", name, suffix);
    printf(".balign 64\n");
    printf("%s%s:\n", name, suffix);
    printf("    .space %d\n", len);
}

void print_uint32(char *name, uint32_t value) {
    printf(".global %s\n", name);
    printf(".balign 64\n");
    printf("%s:\n", name);
    printf("    .word 0x%0*X\n", 8, value);
    printf("    .word 0x00000000\n");
}

void print_array(char *name, char *suffix, fp_t *array, size_t esize, size_t n) {
    printf(".global %s%s\n", name, suffix);
    printf(".balign 64\n");
    printf("%s%s:\n", name, suffix);
    for (size_t i = 0; i < n / (4 / esize); i ++) {
        printf("    .word 0x");
        for (int j = 4 / esize - 1; j >= 0; j --) {
            printf("%0*X", esize * 2, array[i * (4 / esize) + j]);
        }
        printf("\n");
    }
}