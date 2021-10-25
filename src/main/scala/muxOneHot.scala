import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class muxOneHot(wordWidth: Int, wordCount: Int) extends Component{
    val selectors = in Bits(wordCount bits)
    val wordsIn = in Bits(wordWidth * wordCount bits)
    val wordOut = out Bits(wordWidth bits)

    val wordsInSelected = Bits(wordWidth * wordCount bits)
    val annullerArr = Array.fill(wordCount)( annuller(wordWidth, implementation.MUX))
    annullerArr.zipWithIndex.foreach{case(ann, index )=>
        ann.annul := ~selectors(index)
        ann.dataIn := Vec(wordsIn.subdivideIn(wordWidth bits).map(_.asBits))(index)
        wordsInSelected((index + 1) * wordWidth - 1 downto index * wordWidth) := ann.dataOut
    }

    val reducerOR = wordReducer(wordWidth, wordCount, reducerOp.OR)
    reducerOR.wordsIn := wordsInSelected
    wordOut := reducerOR.wordOut
}

