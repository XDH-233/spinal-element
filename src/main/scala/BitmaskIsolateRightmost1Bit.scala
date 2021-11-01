// http://fpgacpu.ca/fpga/Bitmask_Isolate_Rightmost_1_Bit.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class BitmaskIsolateRightmost1Bit(width: Int) extends Component {
    val io = new Bundle {
        val wordIn  = in Bits (width bits)
        val wordOut = out Bits (width bits)
    }
    io.wordOut := io.wordIn & (0 - io.wordIn.asUInt).asBits
}

object BitmaskIsolateRightmost1Bit {
    def apply(width: Int, wordIn: Bits): BitmaskIsolateRightmost1Bit = {
        val ret = BitmaskIsolateRightmost1Bit(width)
        ret.io.wordIn := wordIn
        ret
    }

}
