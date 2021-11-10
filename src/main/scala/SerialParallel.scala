// http://fpgacpu.ca/fpga/Serial_Parallel.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class SerialParallel(width: Int) extends Component {
  val io = new Bundle {
    val serialIn    = in Bool ()
    val parallelOut = master Stream (Bits(width bits))
  }

  io.parallelOut.valid.clear()
  val counter = Counter(0, width - 1)
  val shiftRegister = RegisterPipeline(
    width        = 1,
    pipeDepth    = width,
    en           = ~(counter === 0 && !io.parallelOut.ready),
    parallelLoad = False,
    parallelIn   = B(0, width bits),
    pipeIn       = io.serialIn.asBits,
    parallelOut  = io.parallelOut.payload
  )
  when(counter.value === 0) {
    io.parallelOut.valid := True
    when(io.parallelOut.ready) {
      counter.increment()
    }
  } otherwise {
    counter.increment()
  }
}

object SerialParallel {
  def apply(width: Int, serialIn: Bool): SerialParallel = {
    val ret = new SerialParallel(width)
    ret.io.serialIn := serialIn
    ret
  }
}
