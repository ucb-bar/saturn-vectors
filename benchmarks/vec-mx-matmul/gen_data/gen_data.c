#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include "../../common-data-gen/mx_data_gen.h"

void test(char *name, size_t isize, size_t osize, int m, int n, int k, double min, double max, mx_generator gen, mx_op_ternary macc) {

    fp_t a[m][k];
    fp_t b[k][n];
    fp_t c[m][n];
    fp_t at[k][m];
    fp_t bt[n][k];
    memset(c, 0, m * n * sizeof(fp_t));

    for (size_t i = 0; i < m; i ++) {
        for (size_t j = 0; j < k; j ++) {
            a[i][j] = gen(GM_RAND, min, max);
            at[j][i] = a[i][j];
        }
    }

    for (size_t i = 0; i < k; i ++) {
        for (size_t j = 0; j < n; j ++) {
            b[i][j] = gen(GM_RAND, min, max);
            bt[j][i] = b[i][j];
        }
    }

    for (size_t i = 0; i < m; i ++) {
        for (size_t j = 0; j < n; j ++) {
            for (size_t k0 = 0; k0 < k; k0 ++) {
                c[i][j] = macc(a[i][k0], b[k0][j], c[i][j]);
            }
        }
    }

    print_array(name, "_a", (fp_t *) a, isize, m * k);
    print_array(name, "_b", (fp_t *) b, isize, n * k);
    print_array(name, "_c", (fp_t *) c, osize, m * n);
    print_array(name, "_at", (fp_t *) a, isize, m * k);
    print_array(name, "_bt", (fp_t *) b, isize, n * k);
    print_scratchpad(name, "_out", m * n * osize);
}

#define M 32
#define N 32
#define K 32

int main() {

    print_header();

    print_uint32("M", M);
    print_uint32("N", N);
    print_uint32("K", K);

    test("e4m3", 1, 2, M, N, K, -3e1, 3e1, gen_e4m3, e4m3_wmacc);
    test("e5m2", 1, 2, M, N, K, -1e2, 1e2, gen_e5m2, e5m2_wmacc);

    return 0;
}