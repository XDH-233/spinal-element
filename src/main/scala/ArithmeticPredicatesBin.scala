// http://fpgacpu.ca/fpga/Arithmetic_Predicates_Binary.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class ArithmeticPredicatesBin(wordWidth: Int) extends Component{
    val io = new Bundle{
        val A, B = in Bits(wordWidth bits)
        val eq_, ltU, lteU, gtU, gteU,   ltS, lteS, gtS, gteS = out Bool()
    }
    noIoPrefix()
    import io._
    val adderSub = AdderSubtractorBin(wordWidth)
    adderSub.io.addOrSub := True // sub
    adderSub.io.carryIn := False
    adderSub.io.A := A
    adderSub.io.B := B
    val diff = adderSub.io.sum
    val negtive = adderSub.io.sum.msb

    eq_ := diff === 0
    ltU := ~adderSub.io.carryOut //
    lteU := (eq_ || ltU)
    gtU := adderSub.io.carryOut && ~eq_
    gteU := eq_ || gtU

    ltS := negtive =/= adderSub.io.overflow
    lteS := eq_ || ltS
    gtS := (negtive === adderSub.io.overflow ) && ~eq_
    gteS := gtS || eq_
}




