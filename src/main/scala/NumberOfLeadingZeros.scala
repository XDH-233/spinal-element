//http://fpgacpu.ca/fpga/Number_of_Leading_Zeros.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class NumberOfLeadingZeros(width: Int) extends Component {
  val io = new Bundle {
    val wordIn  = in Bits (width bits)
    val wordOut = out Bits (log2Up(width) + 1 bits)
  }

  val trailingZeros = NumberOfTrailingZeros(width)
  trailingZeros.io.wordIn := io.wordIn.reversed
  io.wordOut              := trailingZeros.io.wordOut
}
