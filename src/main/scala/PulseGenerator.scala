// http://fpgacpu.ca/fpga/Pulse_Generator.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class PulseGenerator() extends Component {
  val io = new Bundle {
    val levelIn         = in Bool ()
    val pulsePosEdgeOut = out Bool ()
    val pulseNegEdgeOut = out Bool ()
    val pulseAnyEdgeOut = out Bool ()
  }

  val levelInDelay = RegNext(io.levelIn) init (False)
  io.pulsePosEdgeOut := io.levelIn && ~levelInDelay
  io.pulseNegEdgeOut := ~io.levelIn && levelInDelay
  io.pulseAnyEdgeOut := io.pulsePosEdgeOut || io.pulseNegEdgeOut
}

object PulseGenerator {
  def apply(levelIn: Bool, pulseAnyEdgeOut: Bool): PulseGenerator = {
    val ret = new PulseGenerator()
    ret.io.levelIn  := levelIn
    pulseAnyEdgeOut := ret.io.pulseAnyEdgeOut
    ret
  }
}
