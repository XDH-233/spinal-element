//http://fpgacpu.ca/fpga/Word_Reverser.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class WordReverser(wordWidth: Int, wordCount: Int) extends Component {
  val io = new Bundle {
    val wordsIn  = in Bits (wordWidth * wordCount bits)
    val wordsOut = out Vec (Bits(wordWidth bits), wordCount)
  }

  io.wordsOut.zip(Vec(io.wordsIn.subdivideIn(wordWidth bits).reverse.map(_.asBits))).foreach { case (o, i) => o := i }
}
