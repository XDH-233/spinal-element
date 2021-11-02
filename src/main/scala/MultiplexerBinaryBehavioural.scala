// http://fpgacpu.ca/fpga/Multiplexer_Binary_Behavioural.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class MultiplexerBinaryBehavioural(wordWidth: Int, inputCount: Int) extends Component {
    val io = new Bundle {
        val selector = in UInt (log2Up(inputCount) bits)
        val wordsIn  = in Bits (wordWidth * inputCount bits)
        val wordOut  = out Bits (wordWidth bits)
    }

    io.wordOut := Vec(io.wordsIn.subdivideIn(wordWidth bits).map(_.asBits)).read(io.selector)
}

object MultiplexerBinaryBehavioural {
    def apply(wordWidth: Int, inputCount: Int, selector: UInt, wordsIn: Bits): MultiplexerBinaryBehavioural = {
        val ret = MultiplexerBinaryBehavioural(wordWidth, inputCount)
        ret.io.selector := selector
        ret.io.wordsIn  := wordsIn
        ret
    }
}
