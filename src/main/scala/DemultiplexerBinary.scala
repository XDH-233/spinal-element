import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class demultiplexerBinary(wordWidth: Int, outputCount: Int, broadcast: Boolean) extends Component{
    val selector = in UInt(log2Up(outputCount ) bits)
    val wordIn = in Bits(wordWidth bits)
    val wordsOut = out Bits(wordWidth * outputCount bits)
    val validsOut = out Bits(outputCount bits)

    val binToOneHot = binaryToOneHot(log2Up(outputCount), outputCount)
    binToOneHot.binaryIn := selector.asBits
    val demuxOneHot = demultiplexerOneHot(wordWidth, outputCount, broadcast)
    demuxOneHot.wordIn := wordIn
    demuxOneHot.selectors := binToOneHot.oneHotOut
    validsOut := demuxOneHot.validsOut
    wordsOut := demuxOneHot.wordsOut
}

