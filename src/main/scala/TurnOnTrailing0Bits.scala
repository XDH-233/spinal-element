// http://fpgacpu.ca/fpga/Turn_On_Trailing_0_Bits.html
// 10101000 -> 10101111
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class TurnOnTrailing0Bits(width: Int) extends Component {
    val io = new Bundle {
        val wordIn  = in Bits (width bits)
        val wordOut = out Bits (width bits)
    }
    io.wordOut := io.wordIn | (io.wordIn.asUInt - 1).asBits
}

object TurnOnTrailing0Bits {
    def apply(width: Int, wordIn: Bits): TurnOnTrailing0Bits = {
        val ret = new TurnOnTrailing0Bits(width)
        ret.io.wordIn := wordIn
        ret
    }
}
