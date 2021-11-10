//http://fpgacpu.ca/fpga/Word_Reducer.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class WordReducer(wordWidth: Int, wordCount: Int, op: reducerOp.Value) extends Component {
  val io = new Bundle {
    val wordsIn = in Bits (wordWidth * wordCount bits)
    val wordOut = out Bits (wordWidth bits)
  }

  op match {
    case reducerOp.AND  => io.wordOut := Vec(io.wordsIn.subdivideIn(wordWidth bits).map(_.asBits)).reduce(_ & _)
    case reducerOp.NAND => io.wordOut := Vec(io.wordsIn.subdivideIn(wordWidth bits).map(_.asBits)).reduce((l, r) => ~(l & r))
    case reducerOp.OR   => io.wordOut := Vec(io.wordsIn.subdivideIn(wordWidth bits) map (_.asBits)).reduce(_ | _)
    case reducerOp.NOR  => io.wordOut := Vec(io.wordsIn.subdivideIn(wordWidth bits).map(_.asBits)).reduce((l, r) => ~(l | r))
    case reducerOp.XOR  => io.wordOut := Vec(io.wordsIn.subdivideIn(wordWidth bits).map(_.asBits)).reduce(_ ^ _)
    case reducerOp.XNOR => io.wordOut := Vec(io.wordsIn.subdivideIn(wordWidth bits).map(_.asBits)).reduce((l, r) => ~(l ^ r))
  }
}

object WordReducer {
  def apply(wordWidth: Int, wordCount: Int, op: reducerOp.Value, wordsIn: Bits): WordReducer = {
    val ret = WordReducer(wordWidth, wordCount, op)
    ret.io.wordsIn := wordsIn
    ret
  }

  def apply(wordWidth: Int, wordCount: Int, op: reducerOp.Value, wordsIn: Bits, wordOut: Bits): WordReducer = {
    val ret = WordReducer(wordWidth, wordCount, op)
    ret.io.wordsIn := wordsIn
    wordOut        := ret.io.wordOut
    ret
  }
}
