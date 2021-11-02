// http://fpgacpu.ca/fpga/Turn_Off_Rightmost_Contiguous_1_Bits.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class TurnOffRightmostContiguous1Bits(width: Int) extends Component {
    val io = new Bundle {
        val wordIn  = in Bits (width bits)
        val wordOut = out Bits (width bits)
    }
    val rightmost1 = BitmaskIsolateRightmost1Bit(width, io.wordIn)
    io.wordOut := (rightmost1.io.wordOut.asUInt + io.wordIn.asUInt).asBits & io.wordIn
}

object TurnOffRightmostContiguous1Bits {
    def apply(width: Int, wordIn: Bits): TurnOffRightmostContiguous1Bits = {
        val ret = new TurnOffRightmostContiguous1Bits(width)
        ret.io.wordIn := wordIn
        ret
    }
}
