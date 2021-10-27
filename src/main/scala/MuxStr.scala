import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class muxStr(wordWidth: Int, inputCount: Int) extends Component{
    val selector = in UInt(log2Up(inputCount) bits)
    val wordsIn = in Bits(wordWidth * inputCount bits)
    val wordOut = out Bits(wordWidth bits)

    val selectorCov = binaryToOneHot(log2Up(inputCount), inputCount)
    selectorCov.binaryIn := selector.asBits
    val MuxOneHot = muxOneHot(wordWidth, inputCount)
    MuxOneHot.selectors := selectorCov.oneHotOut
    MuxOneHot.wordsIn := wordsIn
    wordOut := MuxOneHot.wordOut
}

