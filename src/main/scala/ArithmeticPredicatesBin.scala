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
    val adderSub = AdderSubtractorBin(wordWidth)
    adderSub.io.addOrSub := True // sub
    adderSub.io.carryIn := False
    adderSub.io.A := io.A
    adderSub.io.B := io.B
    val diff = adderSub.io.sum
    val negtive = adderSub.io.sum.msb

    io.eq_ := diff === 0
    io.ltU := ~adderSub.io.carryOut //
    io.lteU := (io.eq_ || io.ltU)
    io.gtU := adderSub.io.carryOut && ~io.eq_
    io.gteU := io.eq_ || io.gtU

    io.ltS := negtive =/= adderSub.io.overflow
    io.lteS := io.eq_ || io.ltS
    io.gtS := (negtive === adderSub.io.overflow ) && ~io.eq_
    io.gteS := io.gtS || io.eq_
}




