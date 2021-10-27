import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class wordReverser(wordWidth: Int, wordCount: Int) extends Component{
    val wordsIn = in Bits(wordWidth * wordCount bits)
    val wordsOut = out Vec(Bits(wordWidth bits), wordCount)
    wordsOut.zip(Vec(wordsIn.subdivideIn(wordWidth bits).reverse.map(_.asBits))).foreach{case(o, i)=> o:=i}
}

