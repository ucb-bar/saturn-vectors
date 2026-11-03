// Verilator testbench for P3109Rounder as the conversion unit uses it.
//
// Sweeps every BF16 pattern through every rounding mode (round-to-odd
// included), both formats and both saturation settings, and compares the code
// and the exception flags against
// expected_<domain>.bin, which models/dump_expected.py writes from gfloat,
// rto_ref and flags_ref.
//
// Built by run_rounder_check.sh, once per domain.

#include <cstdio>
#include <cstdlib>
#include <memory>
#include <vector>
#include "verilated.h"
#include VTOP_HEADER

int main(int argc, char** argv) {
    if (argc < 2) { fprintf(stderr, "usage: %s <expected.bin>\n", argv[0]); return 2; }

    FILE* f = fopen(argv[1], "rb");
    if (!f) { perror("expected"); return 2; }
    // code and flags per case; 65536 patterns x 6 modes x 2 formats x 2 sat
    std::vector<unsigned char> expected(2 * 65536 * 6 * 2 * 2);
    if (fread(expected.data(), 1, expected.size(), f) != expected.size() || fgetc(f) != EOF) {
        fprintf(stderr, "expected file has the wrong size\n"); return 2;
    }
    fclose(f);

    auto ctx = std::make_unique<VerilatedContext>();
    auto dut = std::make_unique<VTOP>(ctx.get());

    size_t i = 0, bad = 0;
    // The five frm modes plus round-to-odd (6), which vfncvt.rod uses.
    const int   mode_codes[] = {0, 1, 2, 3, 4, 6};
    const char* modes[]      = {"rne", "rtz", "rdn", "rup", "rmm", "rod"};

    for (int sat = 0; sat < 2; sat++) {
        for (int altfmt = 0; altfmt < 2; altfmt++) {
            for (int m = 0; m < 6; m++) {
                size_t bad_here = 0;
                for (int bits = 0; bits < 65536; bits++, i++) {
                    dut->io_in = bits;
                    dut->io_altfmt = altfmt;
                    dut->io_roundingMode = mode_codes[m];
                    dut->io_sat = sat;
                    dut->eval();
                    unsigned got = dut->io_out & 0xFF;
                    unsigned got_flags = dut->io_exceptionFlags & 0x1F;
                    unsigned want = expected[2 * i], want_flags = expected[2 * i + 1];
                    if (got != want || got_flags != want_flags) {
                        if (bad_here == 0)
                            printf("   MISMATCH %s sat=%d %s: bf16 0x%04X got 0x%02X/%02X "
                                   "want 0x%02X/%02X (code/flags)\n",
                                   altfmt ? "binary8p3" : "binary8p4", sat, modes[m],
                                   bits, got, got_flags, want, want_flags);
                        bad_here++;
                    }
                }
                printf("   %-10s sat=%d %s: %5zu/65536\n",
                       altfmt ? "binary8p3" : "binary8p4", sat, modes[m],
                       65536 - bad_here);
                bad += bad_here;
            }
        }
    }

    printf("   TOTAL MISMATCHES: %zu  (of %zu cases)\n", bad, i);
    return bad ? 1 : 0;
}
