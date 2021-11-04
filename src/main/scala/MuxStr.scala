//http://fpgacpu.ca/fpga/Multiplexer_Binary_Structural.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class MuxStr(wordWidth: Int, inputCount: Int) extends Component {
  val io = new Bundle {
    val selector = in UInt (log2Up(inputCount) bits)
    val wordsIn  = in Bits (wordWidth * inputCount bits)
    val wordOut  = out Bits (wordWidth bits)
  }

  val selectorCov = BinaryToOneHot(log2Up(inputCount), inputCount, bianryIn = io.selector.asBits)
  val muxOneHot = MuxOneHot(wordWidth, inputCount, selectors = selectorCov.io.oneHotOut, wordsIn = io.wordsIn, wordOut = io.wordOut)
}
