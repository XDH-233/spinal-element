// http://fpgacpu.ca/fpga/Register_Toggle.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class RegisterToggle(width: Int = 8, resetValue: Int = 0) extends Component {
  val io = new Bundle {
    val dataIn  = in Bits (width bits)
    val toggle  = in Bool ()
    val dataOut = out Bits (width bits)
  }

  val config    = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH, clockEnableActiveLevel = HIGH)
  val clkDomain = ClockDomain.external("reg", config = config, withClockEnable = true)
  val clkArea = new ClockingArea(clkDomain) {
    val reg = RegNext(Mux(io.toggle, ~io.dataOut, io.dataIn)) init (resetValue)
    io.dataOut := reg
  }
}

object RegisterToggle {
  def apply(width: Int, resetValue: Int, toggle: Bool, dataIn: Bits): RegisterToggle = {
    val ret = new RegisterToggle(width, resetValue)
    ret.io.dataIn := dataIn
    ret.io.toggle := toggle
    ret
  }
}
