// http://fpgacpu.ca/fpga/Hamming_Distance.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class HammingDistance(width: Int) extends Component{
    val io = new Bundle{
        val wordA, wordB = in Bits(width bits)
        val distance = out Bits(log2Up(width) + 1 bits)
    }
    noIoPrefix()
    import io._

    val reducerXOR = WordReducer(width, 2, reducerOp.XOR)
    reducerXOR.io.wordsIn := (wordA ## wordB)
    val popCount = PopulationCount(width)
    popCount.io.wordIn:= reducerXOR.io.wordOut
    distance := popCount.io.countOut
}