// http://fpgacpu.ca/fpga/Bitmask_1_Bit_at_Rightmost_0_Bit.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class Bitmask1BitAtRightmost0Bit(width: Int) extends Component {
  val io = new Bundle {
    val wordIn  = in Bits (width bits)
    val wordOut = out Bits (width bits)
  }

  io.wordOut := ~io.wordIn & (io.wordIn.asUInt + 1).asBits
}
