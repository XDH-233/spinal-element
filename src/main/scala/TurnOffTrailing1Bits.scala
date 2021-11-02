// http://fpgacpu.ca/fpga/Turn_Off_Trailing_1_Bits.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class TurnOffTrailing1Bits(width: Int) extends Component {
    val io = new Bundle {
        val wordIn  = in Bits (width bits)
        val wordOut = out Bits (width bits)
    }
    io.wordOut := io.wordIn & (io.wordIn.asUInt + 1).asBits
}

object TurnOffTrailing1Bits {
    def apply(width: Int, wordIn: Bits): TurnOffTrailing1Bits = {
        val ret = new TurnOffTrailing1Bits(width)
        ret.io.wordIn := wordIn
        ret
    }
}
