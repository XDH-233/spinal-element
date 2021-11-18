//http://fpgacpu.ca/fpga/Word_Reverser.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class WordReverser(wordWidth: Int, wordCount: Int) extends Component {
  val io = new Bundle {
    val wordsIn  = in Bits (wordWidth * wordCount bits)
    val wordsOut = out Bits (wordWidth * wordCount bits)
  }

  io.wordsOut := Vec(io.wordsIn.subdivideIn(wordWidth bits).map(_.asBits)).reduce(_ ## _)
}

object WordReverser {
  def apply(wordWidth: Int, wordCount: Int, wordsIn: Bits, wordsOut: Bits): WordReverser = {
    val ret = new WordReverser(wordWidth, wordCount)
    ret.io.wordsIn := wordsIn
    wordsOut       := ret.io.wordsOut
    ret
  }
}
