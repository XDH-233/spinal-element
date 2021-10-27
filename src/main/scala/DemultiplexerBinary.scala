//http://fpgacpu.ca/fpga/Demultiplexer_Binary.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class DemultiplexerBinary(wordWidth: Int, outputCount: Int, broadcast: Boolean) extends Component{
    val io = new Bundle{
        val selector = in UInt(log2Up(outputCount ) bits)
        val wordIn = in Bits(wordWidth bits)
        val wordsOut = out Bits(wordWidth * outputCount bits)
        val validsOut = out Bits(outputCount bits)
    }
    noIoPrefix()
    import io._

    val binToOneHot = BinaryToOneHot(log2Up(outputCount), outputCount)
    binToOneHot.io.binaryIn := selector.asBits
    val demuxOneHot = DemultiplexerOneHot(wordWidth, outputCount, broadcast)
    demuxOneHot.io.wordIn := wordIn
    demuxOneHot.io.selectors := binToOneHot.io.oneHotOut
    validsOut := demuxOneHot.io.validsOut
    wordsOut := demuxOneHot.io.wordsOut
}

