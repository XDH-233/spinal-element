//http://fpgacpu.ca/fpga/Multiplexer_Binary_Structural.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class MuxStr(wordWidth: Int, inputCount: Int) extends Component{
    val io = new Bundle{
        val selector = in UInt(log2Up(inputCount) bits)
        val wordsIn = in Bits(wordWidth * inputCount bits)
        val wordOut = out Bits(wordWidth bits)
    }

    val selectorCov = BinaryToOneHot(log2Up(inputCount), inputCount)
    selectorCov.io.binaryIn := selector.asBits
    val muxOneHot = MuxOneHot(wordWidth, inputCount)
    muxOneHot.io.selectors := selectorCov.io.oneHotOut
    muxOneHot.io.wordsIn := io.wordsIn
    io.wordOut := muxOneHot.io.wordOut
}

