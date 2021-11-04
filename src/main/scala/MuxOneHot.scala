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
  val annullerArr = Range(0, wordCount).map(i =>
    Annuller(
      width   = wordWidth,
      impWay  = implementation.MUX,
      annul   = ~io.selectors(i),
      dataIn  = io.wordsIn((i + 1) * wordWidth - 1 downto i * wordWidth),
      dataOut = wordsInSelected((i + 1) * wordWidth - 1 downto i * wordWidth)
    )
  )
  val reducerOR = WordReducer(wordWidth, wordCount, reducerOp.OR, wordsIn = wordsInSelected, wordOut = io.wordOut)
}

object MuxOneHot {
  def apply(wordWidth: Int, wordCount: Int, selectors: Bits, wordsIn: Bits, wordOut: Bits): MuxOneHot = {
    val ret = new MuxOneHot(wordWidth, wordCount)
    ret.io.selectors := selectors
    ret.io.wordsIn   := wordsIn
    wordOut          := ret.io.wordOut
    ret
  }
}
