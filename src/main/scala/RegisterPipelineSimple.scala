// http://fpgacpu.ca/fpga/Register_Pipeline_Simple.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class RegisterPipelineSimple(width: Int, pipeDepth: Int) extends Component {
  val io = new Bundle {
    val pipeIn  = in Bits (width bits)
    val pipeOut = out Bits (width bits)
  }
  if (pipeDepth == 0)
    io.pipeOut  := io.pipeIn
  else {
    val regs = Array.fill(pipeDepth)(Reg(Bits(width bits)) init (0))
    Range(1, pipeDepth).foreach(i => regs(i) := regs(i - 1))
    regs.head  := io.pipeIn
    io.pipeOut := regs.last
  }
}

object RegisterPipelineSimple {
  def apply(width: Int, pipeDepth: Int, pipeIn: Int): RegisterPipelineSimple = {
    val ret = new RegisterPipelineSimple(width, pipeDepth)
    ret.io.pipeIn := pipeIn
    ret
  }
}
