import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class arithmeticPredicatesBin(wordWidth: Int) extends Component{
    val A, B = in Bits(wordWidth bits)
    val eq, ltU, lteU, gtU, gteU,   ltS, lteS, gtS, gteS = out Bool()
    eq := A === B
    ltU := A.asUInt < B.asUInt
    lteU := A.asUInt <= B.asUInt
    gtU := A.asUInt > B.asUInt
    gteU := A.asUInt >= B.asUInt
    ltS := A.asSInt < B.asSInt
    lteS := A.asSInt <= B.asSInt
    gtS := A.asSInt > B.asSInt
    gteS := A.asSInt >= B.asSInt
}