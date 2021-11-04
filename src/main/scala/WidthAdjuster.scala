//http://fpgacpu.ca/fpga/Width_Adjuster.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class WidthAdjuster(wordWidthIn: Int, wordWidthOut: Int, sign: Boolean) extends Component {
  val io = new Bundle {
    val originalInput  = in Bits (wordWidthIn bits)
    val adjustedOutput = out Bits (wordWidthOut bits)
  }

  if (sign)
    io.adjustedOutput := io.originalInput.asSInt.resize(wordWidthOut).asBits
  else
    io.adjustedOutput := io.originalInput.resize(wordWidthOut)
}
