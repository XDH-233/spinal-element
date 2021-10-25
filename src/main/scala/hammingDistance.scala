import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class hammingDistance(width: Int) extends Component{
    val wordA, wordB = in Bits(width bits)
    val distance = out Bits(log2Up(width) + 1 bits)

    val reducerXOR = wordReducer(width, 2, reducerOp.XOR)
    reducerXOR.wordsIn := (wordA ## wordB)
    val popCount = populationCount(width)
    popCount.wordIn:= reducerXOR.wordOut
    distance := popCount.countOut
}