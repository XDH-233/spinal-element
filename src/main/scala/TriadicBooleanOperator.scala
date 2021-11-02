// http://fpgacpu.ca/fpga/Triadic_Boolean_Operator.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class TriadicBooleanOperator(width: Int) extends Component {
    val io = new Bundle {
        val truthTable0, truthTable1 = in Bits (dyadicBooleanOperations.dyadicTruthTableWidth bits)
        val wordA, wordB, wordC      = in Bits (width bits)
        val dual                     = in Bool ()
        val result0, result1         = out Bits (width bits)
    }
    val wordD                       = Mux(io.dual, ~io.wordC, io.wordC)
    val firstDyadicBooleanOperator  = DyadicBooleanOperator(width, wordA = io.wordA, wordB = io.wordB, truthTable = io.truthTable0)
    val secondDyadicBooleanOperator = DyadicBooleanOperator(width, wordA = io.wordA, wordB = io.wordB, truthTable = io.truthTable1)
    val select0 = MuxB2to1(width, wordIn0 = firstDyadicBooleanOperator.io.result, wordIn1 = secondDyadicBooleanOperator.io.result, bitmask = io.wordC)
    val select1 = MuxB2to1(width, wordIn0 = firstDyadicBooleanOperator.io.result, wordIn1 = secondDyadicBooleanOperator.io.result, bitmask = wordD)
    io.result0 := select0.io.wordOut
    io.result1 := select1.io.wordOut
}

object TriadicBooleanOperator {
    def apply(width: Int, truthTable0: Bits, truthTable1: Bits, wordA: Bits, wordB: Bits, wordC: Bits, dual: Bool): TriadicBooleanOperator = {
        val ret = new TriadicBooleanOperator(width)
        ret.io.truthTable0 := truthTable0
        ret.io.truthTable1 := truthTable1
        ret.io.wordA       := wordA
        ret.io.wordB       := wordB
        ret.io.wordC       := wordC
        ret.io.dual        := dual
        ret
    }
}
