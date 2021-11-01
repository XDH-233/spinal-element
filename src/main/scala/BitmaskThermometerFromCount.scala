// http://fpgacpu.ca/fpga/Bitmask_Thermometer_from_Count.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class BitmaskThermometerFromCount(width: Int) extends Component {
    val countWidth: Int = log2Up(width + 1)
    val io = new Bundle {
        val countIn = in UInt (countWidth bits)
        val wrodOut = out Bits (width bits)
    }
    io.wrodOut := ((U(1, width bits) |<< io.countIn) - 1).asBits
}

object BitmaskThermometerFromCount {
    def apply(width: Int, countIn: UInt): BitmaskThermometerFromCount = {
        val ret = BitmaskThermometerFromCount(width)
        ret.io.countIn := countIn
        ret
    }
}
