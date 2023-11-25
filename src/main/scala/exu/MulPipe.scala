package vector.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import vector.common._

class ElementWiseMulPipe(depth: Int)(implicit p: Parameters) extends PipelinedFunctionalUnit(depth, false)(p) {

  lazy val ctrl_table = Seq(
    (OPMFunct6.mul, Seq())
  )

  override def accepts(f3: UInt, f6: UInt): Bool = VecDecode(f3, f6, ctrl_table.map(_._1))

}
