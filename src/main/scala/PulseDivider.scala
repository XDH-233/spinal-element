// http://fpgacpu.ca/fpga/Pulse_Divider.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import lib._

case class PulseDivider(wordWidth: Int, initialDivisor: Int) extends Component {
  val io = new Bundle {
    val restart   = in Bool ()
    val divisor   = in UInt (wordWidth bits)
    val pulsesIn  = in Bool ()
    val pulseOut  = out Bool ()
    val divByZero = out Bool ()
  }

  val counter = Counter(wordWidth bits)
  counter.value init (0)
  val divisorReg   = Reg(UInt(wordWidth bits)) init (initialDivisor)
  val run          = io.pulsesIn && counter.value =/= divisorReg - 1
  val divisionDone = io.pulsesIn && (counter.value === divisorReg - 1)
  val clear        = divisionDone || io.restart || io.divByZero
  when(clear) {
    counter.clear()
    divisorReg := io.divisor
  }
  when(run) {
    counter.increment()
  }
  io.pulseOut  := divisionDone && ~io.restart
  io.divByZero := divisorReg === 0
}

object PulseDivider {
  def apply(wordWidth: Int, initialDivisor: Int, restart: Bool, divisor: UInt, pulsesIn: Bool): PulseDivider = {
    val ret = new PulseDivider(wordWidth, initialDivisor)
    ret.io.restart  := restart
    ret.io.divisor  := divisor
    ret.io.pulsesIn := pulsesIn
    ret
  }
}
