// http://fpgacpu.ca/fpga/Register_Pipeline.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class RegisterPipeline(width: Int, pipeDepth: Int) extends Component {
  val io = new Bundle {
    val parallelLoad = in Bool ()
    val parallelIn   = in Bits (width * pipeDepth bits)
    val parallelOut  = out Bits (width * pipeDepth bits)
    val pipeIn       = in Bits (width bits)
    val pipeOut      = out Bits (width bits)
  }
  val regs                            = Array.fill(pipeDepth)(Reg(Bits(width bits)) init (0))
  val multiplexerBinaryBehaviouralArr = Array.fill(pipeDepth)(MultiplexerBinaryBehavioural(width, 2))
  multiplexerBinaryBehaviouralArr.foreach(_.io.selector := io.parallelLoad.asUInt)
  multiplexerBinaryBehaviouralArr.head.io.wordsIn       := io.parallelIn(width - 1 downto 0) ## io.pipeIn
  Range(1, pipeDepth).foreach(i => multiplexerBinaryBehaviouralArr(i).io.wordsIn := io.parallelIn((i + 1) * width - 1 downto i * width) ## regs(i - 1))
  multiplexerBinaryBehaviouralArr.zip(regs).foreach { case (m, r) => r := m.io.wordOut }
  io.pipeOut     := regs.last
  io.parallelOut := regs.reverse.reduce(_ ## _)
}

object RegisterPipeline {
  def apply(width: Int, pipeDepth: Int, parallelLoad: Bool, parallelIn: Bits, pipeIn: Bits): RegisterPipeline = {
    val ret = new RegisterPipeline(width, pipeDepth)
    ret.io.parallelLoad := parallelLoad
    ret.io.parallelIn   := parallelIn
    ret.io.pipeIn       := pipeIn
    ret
  }
}
