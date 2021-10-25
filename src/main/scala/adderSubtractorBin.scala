import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class adderSubtractorBin(width: Int) extends Component{
    val addOrSub = in Bool() // 0->add
    val carryIn = in Bool()
    val A , B = in Bits(width bits)
    val sum, carries = out Bits(width bits)
    val overflow = out Bool()
    val carryOut = out Bool()

    val widthAdU = widthAdjuster(1, width, false)
    val widthAdS = widthAdjuster(1, width, true)
    widthAdU.originalInput := carryIn.asBits
    widthAdS.originalInput := carryIn.asBits


}