//http://fpgacpu.ca/fpga/Multiplexer_One_Hot.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class MuxOneHot(wordWidth: Int, wordCount: Int) extends Component {
    val io = new Bundle {
        val selectors = in Bits (wordCount bits)
        val wordsIn   = in Bits (wordWidth * wordCount bits)
        val wordOut   = out Bits (wordWidth bits)
    }

    val wordsInSelected = Bits(wordWidth * wordCount bits)
    val annullerArr     = Array.fill(wordCount)(Annuller(wordWidth, implementation.MUX))
    annullerArr.zipWithIndex.foreach { case (ann, index) =>
        ann.io.annul                                                          := ~io.selectors(index)
        ann.io.dataIn                                                         := Vec(io.wordsIn.subdivideIn(wordWidth bits).map(_.asBits))(index)
        wordsInSelected((index + 1) * wordWidth - 1 downto index * wordWidth) := ann.io.dataOut
    }

    val reducerOR = WordReducer(wordWidth, wordCount, reducerOp.OR)
    reducerOR.io.wordsIn := wordsInSelected
    io.wordOut           := reducerOR.io.wordOut
}
