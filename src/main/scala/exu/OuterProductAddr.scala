package saturn.exu

import chisel3._
import chisel3.util._

// TSS-addressed row/column indexing into the outer-product array's per-cell register
// file, shared by every unit that moves one tile row/column between the array and the
// outside world: sf.vtmv.v.t/t.v (OuterProductSequencer, TSS from rs1), sf.vtse32
// (OuterProductSequencer, TSS from rs2), and sf.vtle32 (LoadSequencer's response path
// in Backend, TSS from rs2). `tssIdx` selects which row/column of the tile is being
// moved (fixed for the whole instruction, TSS[tssIdxBits-1:0]); `colIdx` walks the
// dLen-wide element groups along that row/column, 0 .. col_last.
case class OuterProductAddr(
  mrfIdx: UInt, rowIdxCell: UInt, colIdxCell: UInt,
  moveCluster: UInt, rowMoveCluster: UInt, colMoveCluster: UInt
)

object OuterProductAddr {
  def apply(
    tile: UInt, col: Bool, tssIdx: UInt, colIdx: UInt,
    yDim: Int, xDim: Int, clusterYdim: Int, clusterXdim: Int, S: Int, R: Int
  ): OuterProductAddr = {
    def subBits(x: UInt): UInt = if (R > 1) x(log2Ceil(R)-1, 0) else 0.U(0.W)

    // tile row/col r = scalarSub*S + i*yDim + I; I/i (cluster/cell within cluster)
    // come from tssIdx, the sub-tile from tssIdx's upper bits. Column moves transpose.
    val rowMoveCluster = tssIdx(log2Ceil(yDim)-1, 0)
    val colMoveCluster = tssIdx(log2Ceil(xDim)-1, 0)
    val moveCluster = Mux(col, colMoveCluster, rowMoveCluster)

    val scalarCell = (tssIdx >> log2Ceil(yDim))(log2Ceil(clusterYdim)-1, 0)
    val scalarSub  = (tssIdx >> log2Ceil(S))
    val iterCell   = colIdx(log2Ceil(clusterXdim)-1, 0)
    val iterSub    = (colIdx >> log2Ceil(clusterXdim))

    val moveRowSub = Mux(col, subBits(iterSub), subBits(scalarSub))
    val moveColSub = Mux(col, subBits(scalarSub), subBits(iterSub))
    val mrfIdx = Cat(tile, moveRowSub, moveColSub)

    OuterProductAddr(mrfIdx, scalarCell, iterCell, moveCluster, rowMoveCluster, colMoveCluster)
  }
}
