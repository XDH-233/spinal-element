// http://fpgacpu.ca/fpga/Turn_On_Rightmost_0_Bit.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class TurnOnRightmost0Bit(width: Int) extends Component {
  val io = new Bundle {
    val wordIn  = in Bits (width bits)
    val wordOut = out Bits (width bits)
  }

  io.wordOut := io.wordIn | (io.wordIn.asUInt + 1).asBits
}

object TurnOnRightmost0Bit {
  def apply(width: Int, wordIn: Bits): TurnOnRightmost0Bit = {
    val ret = new TurnOnRightmost0Bit(width)
    ret.io.wordIn := wordIn
    ret
  }
}
