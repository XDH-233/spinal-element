// http://fpgacpu.ca/fpga/Word_Change_Detector.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class WordChangeDetector(wordWidth: Int) extends Component {
  val io = new Bundle {
    val inputWord   = in Bits (wordWidth bits)
    val outputPulse = out Bool ()
  }

  val bitChange = Bits(wordWidth bits)
  Range(0, wordWidth).map(i => PulseGenerator(io.inputWord(i), bitChange(i)))
  io.outputPulse := bitChange === 0
}
