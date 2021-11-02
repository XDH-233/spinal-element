// http://fpgacpu.ca/fpga/Register.html
// A Synchronous Register to Store and Control Data

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class Register(width: Int, resetValue: Int) extends Component {
    val io = new Bundle {
        val dataIn  = in Bits (width bits)
        val dataOut = out Bits (width bits)
    }
    val config    = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH, clockEnableActiveLevel = HIGH)
    val clkDomain = ClockDomain.external("reg", withClockEnable = true)
    val clkArea = new ClockingArea(clkDomain) {
        val reg = RegNext(io.dataIn) init (resetValue)
        io.dataOut := reg
    }
}

object Register {
    def apply(width: Int, resetValue: Int, dataIn: Bits): Register = {
        val ret = new Register(width, resetValue)
        ret.io.dataIn := dataIn
        ret
    }
}
