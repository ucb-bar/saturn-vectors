// mmt4d_params.h -- params struct + flags (mirrors IREE).
//
// A reduced local stand-in for IREE's iree_uk_mmt4d_params_t
// (runtime/src/iree/builtins/ukernel/mmt4d_internal.h), keeping only the
// fields a standalone tile function needs: K (= K1, the number of K1 panel
// groups; actual reduction depth is K1*K0), M0, N0, K0, and flags. The tile
// function signature mirrors iree_uk_mmt4d_tile_func_t so Phase 2 porting is
// close to a rename: `tile_func(out_tile, lhs_panel, rhs_panel, params)`.
//
// Panel layout (must match IREE's mmt4d exactly):
//   lhs_panel: [K1][M0][K0], K0-contiguous
//   rhs_panel: [K1][N0][K0], K0-contiguous (i.e. rhs is logically [N][K],
//              not [K][N] -- see runtime/.../mmt4d_riscv_64_xsmtvdot.c)
//   out_tile:  [M0][N0], row-major
//
// for k1 in [0, K1): for m0, n0, k0:
//     out[m0][n0] += lhs[k1][m0][k0] * rhs[k1][n0][k0]

#ifndef MMT4D_PARAMS_H
#define MMT4D_PARAMS_H

#include <stdint.h>

typedef struct mmt4d_params_t {
  int64_t K;      // K1: number of [M0][K0]/[N0][K0] panel groups.
  int32_t M0;
  int32_t N0;
  int32_t K0;
  uint32_t flags;
} mmt4d_params_t;

// Matches IREE_UK_FLAG_MMT4D_ACCUMULATE's bit value
// (runtime/src/iree/builtins/ukernel/exported_bits.h) for Phase 2 ease, even
// though this project defines its own flags namespace.
#define MMT4D_FLAG_ACCUMULATE 0x100u

// Tile function signature, matching iree_uk_mmt4d_tile_func_t
// (runtime/src/iree/builtins/ukernel/mmt4d_internal.h) modulo the params
// struct type.
typedef void (*mmt4d_tile_func_t)(void* out_tile, const void* lhs_panel,
                                   const void* rhs_panel,
                                   const mmt4d_params_t* params);

#endif // MMT4D_PARAMS_H
