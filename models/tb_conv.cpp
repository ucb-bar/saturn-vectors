// Drives FPConvBlock, taken from a P3109 config's generated Verilog, with the
// records conv_vectors.py writes: one conversion of four lanes per clock.
// Codes and exception flags are compared LATENCY cycles later. Build with
// -DHAS_SCALE for a block config, whose FPConvBlock has a scale input.
#include <cstdint>
#include <cstdio>
#include <memory>
#include <vector>
#include "verilated.h"
#include "VFPConvBlock.h"

// Two register stages; after the tick that loads record k, io_out holds record k-1
#define LATENCY 1

struct Rec {
    uint8_t  fmt, frm, flags, in_eew;   // flags: bit0 widen, bit1 narrow, bit2 rto, bit3 sat
    uint32_t pad;
    uint64_t in, scale, expect, mask, exc;
};
static_assert(sizeof(Rec) == 48, "record layout must match conv_vectors.py");

int main(int argc, char** argv) {
    if (argc < 2) { fprintf(stderr, "usage: %s <conv.bin>\n", argv[0]); return 2; }
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
    auto t = std::make_unique<VFPConvBlock>(ctx.get());
    const uint8_t* exc[8] = {&t->io_exc_0, &t->io_exc_1, &t->io_exc_2, &t->io_exc_3,
                             &t->io_exc_4, &t->io_exc_5, &t->io_exc_6, &t->io_exc_7};
    auto tick = [&] { t->clock = 0; t->eval(); t->clock = 1; t->eval(); };
    auto drive = [&](const Rec& r, bool valid) {
        t->io_valid = valid;
        t->io_in = r.in;
#ifdef HAS_SCALE
        t->io_scale = r.scale;
#endif
        t->io_in_eew = r.in_eew;
        t->io_widen = r.flags & 1;
        t->io_narrow = (r.flags >> 1) & 1;
        t->io_rto = (r.flags >> 2) & 1;
        t->io_sat = (r.flags >> 3) & 1;
        t->io_frm = r.frm;
        t->io_signed = 0;
        t->io_i2f = 0;
        t->io_f2i = 0;
        t->io_truncating = 0;
        t->io_in_altfmt = r.fmt & 1;
    };

    Rec idle{};
    t->reset = 1;
    drive(idle, false);
    for (int i = 0; i < 5; i++) tick();
    t->reset = 0;

    size_t bad = 0, n[2] = {0, 0}, bad_fmt[2] = {0, 0};
    for (size_t k = 0; k < recs.size() + LATENCY; k++) {
        drive(k < recs.size() ? recs[k] : idle, k < recs.size());
        tick();
        if (k < LATENCY) continue;
        const Rec& q = recs[k - LATENCY];
        int fmt = q.fmt & 1;
        uint64_t got = t->io_out & q.mask, want = q.expect & q.mask;
        uint64_t got_exc = 0;
        for (int l = 0; l < 4; l++) got_exc |= (uint64_t)(*exc[2 * l] & 0x1F) << (8 * l);
        n[fmt]++;
        if (got != want || got_exc != q.exc) {
            if (bad++ < 8)
                printf("   MISMATCH record %zu %s %s frm=%d%s%s scale=%016llx in=%016llx: "
                       "got %016llx exc %08llx, want %016llx exc %08llx\n",
                       k - LATENCY, fmt ? "binary8p3" : "binary8p4", (q.flags & 1) ? "widen" : "narrow",
                       q.frm, (q.flags & 4) ? " rod" : "", (q.flags & 8) ? " sat" : "",
                       (unsigned long long)q.scale, (unsigned long long)q.in,
                       (unsigned long long)got, (unsigned long long)got_exc,
                       (unsigned long long)want, (unsigned long long)q.exc);
            bad_fmt[fmt]++;
        }
    }
    for (int c = 0; c < 2; c++)
        printf("   %s: %zu/%zu cycles agree\n", c ? "binary8p3" : "binary8p4", n[c] - bad_fmt[c], n[c]);
    printf("   TOTAL MISMATCHES: %zu  (of %zu cycles)\n", bad, recs.size());
    return bad ? 1 : 0;
}
