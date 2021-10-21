import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class numberOfLeadingZeros(width: Int) extends Component{
    val wordIn = in Bits(width bits)
    val wordOut = out Bits(log2Up(width) + 1 bits)
    val trailingZeros = numberOfTrailingZeros(width)
    trailingZeros.wordIn := wordIn.reversed
    wordOut := trailingZeros.wordOut
}

