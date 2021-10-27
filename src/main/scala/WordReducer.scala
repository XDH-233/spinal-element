import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._


case class wordReducer(wordWidth: Int, wordCount: Int, op: reducerOp.Value) extends Component{
    val wordsIn = in Bits(wordWidth * wordCount bits)
    val wordOut = out Bits(wordWidth bits)
    // Should I take Map in count ?
    op match {
        case reducerOp.AND => wordOut := Vec(wordsIn.subdivideIn(wordWidth bits).map(_.asBits)).reduce(_ & _)
        case reducerOp.NAND => wordOut := Vec(wordsIn.subdivideIn(wordWidth bits).map(_.asBits)).reduce((l,r) => ~(l & r))
        case reducerOp.OR => wordOut := Vec(wordsIn.subdivideIn(wordWidth bits)map(_.asBits)).reduce(_ | _)
        case reducerOp.NOR => wordOut := Vec(wordsIn.subdivideIn(wordWidth bits).map(_.asBits)).reduce((l,r) => ~(l | r))
        case reducerOp.XOR => wordOut := Vec(wordsIn.subdivideIn(wordWidth bits).map(_.asBits)).reduce(_ ^ _)
        case reducerOp.XNOR => wordOut := Vec(wordsIn.subdivideIn(wordWidth bits).map(_.asBits)).reduce((l,r) => ~(l ^ r))
    }
}