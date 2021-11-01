// http://fpgacpu.ca/fpga/Bitmask_Turn_On_Trailing_0_Bits.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class BitmaskTurnOnTrailing0Bits(width: Int) extends Component {
    val io = new Bundle {
        val wordIn  = in Bits (width bits)
        val wordOut = out Bits (width bits)
    }
    io.wordOut := ~io.wordIn & (io.wordIn.asUInt - 1).asBits
}

object BitmaskTurnOnTrailing0Bits {
    def apply(width: Int, wordIn: Bits): BitmaskTurnOnTrailing0Bits = {
        val ret = new BitmaskTurnOnTrailing0Bits(width)
        ret.io.wordIn := wordIn
        ret
    }
}
