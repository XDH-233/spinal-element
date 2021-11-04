// http://fpgacpu.ca/fpga/Turn_Off_Rightmost_1_Bit.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class TurnOffRightmost1Bit(width: Int) extends Component {
  val io = new Bundle {
    val wordIn  = in Bits (width bits)
    val wordOut = out Bits (width bits)
  }

  io.wordOut := io.wordIn & (io.wordIn.asUInt - 1).asBits
}

object TurnOffRightmost1Bit {
  def apply(width: Int, wordIn: Bits): TurnOffRightmost1Bit = {
    val ret = new TurnOffRightmost1Bit(width)
    ret.io.wordIn := wordIn
    ret
  }
}
