#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include "../../common-data-gen/mx_data_gen.h"

void test(char *name, size_t isize, size_t osize, int m, int n, int k, double min, double max, mx_generator gen, mx_op_ternary macc, GenMode gen_mode_a, GenMode gen_mode_b) {

    fp_t a[m][k];
    fp_t b[k][n];
    fp_t c[m][n];
    fp_t at[k][m];
    fp_t bt[n][k];
    memset(c, 0, m * n * sizeof(fp_t));

    for (size_t i = 0; i < m; i ++) {
        for (size_t j = 0; j < k; j ++) {
            a[i][j] = gen(gen_mode_a, min, max);
            at[j][i] = a[i][j];
        }
    }

    for (size_t i = 0; i < k; i ++) {
        for (size_t j = 0; j < n; j ++) {
            b[i][j] = gen(gen_mode_b, min, max);
        }
    }

    for (size_t i = 0; i < m; i ++) {
        for (size_t j = 0; j < n; j ++) {
            for (size_t k0 = 0; k0 < k; k0 ++) {
                c[i][j] = macc(a[i][k0], b[k0][j], c[i][j]);
            }
        }
    }

    // print_array(name, "_a", (fp_t *) a, isize, m * k);
    print_array(name, "_at", (fp_t *) at, isize, k * m);
    print_array(name, "_b", (fp_t *) b, isize, n * k);
    print_array(name, "_c", (fp_t *) c, osize, m * n);
    print_scratchpad(name, "_out", m * n * osize);
}

#define M 64
#define N 64
#define K 16

int main() {

    print_header();

    print_uint32("M", M);
    print_uint32("N", N);
    print_uint32("K", K);

    const GenMode modes[] = {GM_INF, GM_NINF, GM_NAN, GM_NNAN, GM_ZERO, GM_NZERO, GM_RAND};
    const char *mode_names[] = {"inf", "ninf", "nan", "nnan", "zero", "nzero", "rand"};
    const int num_modes = sizeof(modes) / sizeof(modes[0]);

    for (int j = 0; j < num_modes; j++) {
        char e4m3_name[64], e5m2_name[64];
        snprintf(e4m3_name, sizeof(e4m3_name), "e4m3_rand_%s", mode_names[j]);
        snprintf(e5m2_name, sizeof(e5m2_name), "e5m2_rand_%s", mode_names[j]);
        test(e4m3_name, 1, 4, M, N, K, -448, 448, gen_e4m3, e4m3_qmacc, GM_RAND, modes[j]);
        test(e5m2_name, 1, 4, M, N, K, -57344, 57344, gen_e5m2, e5m2_qmacc, GM_RAND, modes[j]);
    }
    return 0;
}