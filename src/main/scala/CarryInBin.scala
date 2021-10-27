import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class carryInBin(width: Int) extends Component{
    val A , B, sum = in Bits(width bits)
    val carryIn = out Bits (width bits)
    carryIn := A ^ B ^ sum

}