// Drives one OuterProductCell, taken from an OPU config's generated Verilog,
// with the records opu_vectors.py writes. Per record: load c into a register
// (mvin), multiply-accumulate a x b into it, wait for the FMA pipeline, read
// the register back.
#include <cstdint>
#include <cstdio>
#include <memory>
#include <vector>
#include "verilated.h"
#include "VOuterProductCell.h"

#define WAIT 4  // cycles after the macc; the FMA pipeline takes 2

struct Rec {
    uint8_t  altfmt, a, b, pad;
    uint32_t c, expect;
};
static_assert(sizeof(Rec) == 12, "record layout must match opu_vectors.py");

int main(int argc, char** argv) {
    if (argc < 2) { fprintf(stderr, "usage: %s <opu.bin>\n", argv[0]); return 2; }
    FILE* f = fopen(argv[1], "rb");
    if (!f) { perror(argv[1]); return 2; }
    fseek(f, 0, SEEK_END);
    long bytes = ftell(f);
    fseek(f, 0, SEEK_SET);
    if (bytes <= 0 || bytes % sizeof(Rec)) { fprintf(stderr, "bad record file\n"); return 2; }
    std::vector<Rec> recs(bytes / sizeof(Rec));
    if (fread(recs.data(), sizeof(Rec), recs.size(), f) != recs.size()) { perror("read"); return 2; }
    fclose(f);

    auto ctx = std::make_unique<VerilatedContext>();
    auto t = std::make_unique<VOuterProductCell>(ctx.get());
    auto tick = [&] { t->clock = 0; t->eval(); t->clock = 1; t->eval(); };
    auto idle = [&] {
        t->io_macc = 0; t->io_fp8 = 0; t->io_mvin = 0;
        t->io_mvin_bcast = 0; t->io_mvin_bcast_col = 0;
    };

    idle();
    t->reset = 1;
    for (int i = 0; i < 5; i++) tick();
    t->reset = 0;

    size_t bad = 0, n[2] = {0, 0}, bad_fmt[2] = {0, 0};
    for (size_t k = 0; k < recs.size(); k++) {
        const Rec& r = recs[k];
        t->io_mrf_idx = k % 16;
        t->io_mvin = 1;
        t->io_mvin_data = r.c;
        tick();
        idle();
        t->io_macc = 1; t->io_fp8 = 1; t->io_altfmt = r.altfmt;
        t->io_in_l = r.a; t->io_in_t = r.b;
        tick();
        idle();
        for (int i = 0; i < WAIT; i++) tick();
        t->eval();
        uint32_t got = t->io_out;
        n[r.altfmt]++;
        if (got != r.expect) {
            bad_fmt[r.altfmt]++;
            if (bad++ < 8)
                printf("   MISMATCH altfmt=%d a=%02x b=%02x c=%08x: got %08x, want %08x\n",
                       r.altfmt, r.a, r.b, r.c, got, r.expect);
        }
    }
    for (int i = 0; i < 2; i++)
        printf("   altfmt %d: %zu/%zu multiply-accumulates agree\n", i, n[i] - bad_fmt[i], n[i]);
    printf("   TOTAL MISMATCHES: %zu  (of %zu)\n", bad, recs.size());
    return bad ? 1 : 0;
}
