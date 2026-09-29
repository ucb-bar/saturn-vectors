#include <stdio.h>
#include <stdint.h>

typedef enum GenMode {
	GM_INF,
	GM_NINF,
	GM_NAN,
	GM_NNAN,
	GM_ZERO,
	GM_NZERO,
	GM_RAND
} GenMode;

typedef uint64_t fp_t;
typedef fp_t (*mx_generator)(GenMode, double, double);
typedef fp_t (*mx_op_unary)(fp_t);
typedef fp_t (*mx_op_binary)(fp_t, fp_t);
typedef fp_t (*mx_op_ternary)(fp_t, fp_t, fp_t);

fp_t random_float(double min, double max);

fp_t fp64_to_fp32(fp_t a);
fp_t fp32_to_fp16(fp_t a);
fp_t fp32_to_bf16(fp_t a);
fp_t bf16_to_e4m3(fp_t a);
fp_t bf16_to_e5m2(fp_t a);
fp_t bf16_to_e4m3_sat(fp_t a);
fp_t bf16_to_e5m2_sat(fp_t a);

fp_t fp16_to_fp32(fp_t a);
fp_t bf16_to_fp32(fp_t a);
fp_t e4m3_to_bf16(fp_t a);
fp_t e5m2_to_bf16(fp_t a);

#define _DEF_OPS_FMA_BINARY(name) \
	fp_t name ## _mul(fp_t a, fp_t b); \
	fp_t name ## _add(fp_t a, fp_t b); \
	fp_t name ## _sub(fp_t a, fp_t b); \
	fp_t name ## _wmul(fp_t a, fp_t b); \
	fp_t name ## _wadd(fp_t a, fp_t b); \
	fp_t name ## _wsub(fp_t a, fp_t b); \
	fp_t name ## _qmul(fp_t a, fp_t b); \
	fp_t name ## _qadd(fp_t a, fp_t b); \
	fp_t name ## _qsub(fp_t a, fp_t b);
_DEF_OPS_FMA_BINARY(fp32);
_DEF_OPS_FMA_BINARY(fp16);
_DEF_OPS_FMA_BINARY(bf16);
_DEF_OPS_FMA_BINARY(e5m2);
_DEF_OPS_FMA_BINARY(e4m3);

fp_t e4m3_wmacc(fp_t a, fp_t b, fp_t c);
fp_t e5m2_wmacc(fp_t a, fp_t b, fp_t c);
fp_t e4m3_qmacc(fp_t a, fp_t b, fp_t c);
fp_t e5m2_qmacc(fp_t a, fp_t b, fp_t c);

fp_t gen_fp32(GenMode mode, double min, double max);
fp_t gen_fp16(GenMode mode, double min, double max);
fp_t gen_bf16(GenMode mode, double min, double max);
fp_t gen_e4m3(GenMode mode, double min, double max);
fp_t gen_e5m2(GenMode mode, double min, double max);

void print_header();
void print_scratchpad(char *name, char *suffix, size_t len);
void print_uint32(char *name, uint32_t value);
void print_array(char *name, char *suffix, fp_t *array, size_t esize, size_t n);