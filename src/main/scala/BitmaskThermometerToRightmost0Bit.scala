// http://fpgacpu.ca/fpga/Bitmask_Thermometer_to_Rightmost_0_Bit.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

/** @param width
  */
case class BitmaskThermometerToRightmost0Bit(width: Int) extends Component {
  val io = new Bundle {
    val wordIn  = in Bits (width bits)
    val wordOut = out Bits (width bits)
  }

  io.wordOut := (io.wordIn.asUInt + 1).asBits ^ io.wordIn
}

object BitmaskThermometerToRightmost0Bit {
  def apply(width: Int, wordIn: Bits): BitmaskThermometerToRightmost0Bit = {
    val ret = BitmaskThermometerToRightmost0Bit(width)
    ret.io.wordIn := wordIn
    ret
  }
}
