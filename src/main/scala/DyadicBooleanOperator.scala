// http://fpgacpu.ca/fpga/Dyadic_Boolean_Operator.html
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
object dyadicBooleanOperations {
    def dyadicTruthTableWidth = 4
    def dyadicSelectorWidth   = 2
}
case class DyadicBooleanOperator(width: Int) extends Component {
    val io = new Bundle {
        val truthTable   = in Bits (dyadicBooleanOperations.dyadicTruthTableWidth bits)
        val wordA, wordB = in Bits (width bits)
        val result       = out Bits (width bits)
    }
    val muxArr = Range(0, width).map { i =>
        MultiplexerBinaryBehavioural(1, dyadicBooleanOperations.dyadicTruthTableWidth, (io.wordA(i) ## io.wordB(i)).asUInt, io.truthTable)
    }
    muxArr.zipWithIndex.foreach { case (m, i) => io.result(i) := m.io.wordOut.asBool }
}

object DyadicBooleanOperator {
    def apply(width: Int, wordA: Bits, wordB: Bits, truthTable: Bits): DyadicBooleanOperator = {
        val ret = new DyadicBooleanOperator(width)
        ret.io.wordA      := wordA
        ret.io.wordB      := wordB
        ret.io.truthTable := truthTable
        ret
    }
}
