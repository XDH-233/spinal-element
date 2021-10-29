//http://fpgacpu.ca/fpga/CarryIn_Binary.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class CarryInBin(width: Int) extends Component {
    val io = new Bundle {
        val A, B, sum = in Bits (width bits)
        val carryIn   = out Bits (width bits)
    }
    io.carryIn := io.A ^ io.B ^ io.sum

}
