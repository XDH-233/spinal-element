// http://fpgacpu.ca/fpga/Bitmask_Thermometer_to_Rightmost_1_Bit.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class BitmaskThermometerToRightmost1Bit(width: Int) extends Component {
  val io = new Bundle {
    val wordIn  = in Bits (width bits)
    val wordOut = out Bits (width bits)
  }
  io.wordOut := io.wordIn ^ (io.wordIn.asUInt - 1).asBits
}

object BitmaskThermometerToRightmost1Bit {
  def apply(width: Int, wordIn: Bits): BitmaskThermometerToRightmost1Bit = {
    val ret = new BitmaskThermometerToRightmost1Bit(width)
    ret.io.wordIn := wordIn
    ret
  }
}
