// http://fpgacpu.ca/fpga/Parallel_Serial.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class ParallelSerial(width: Int) extends Component {
  val io = new Bundle {
    val parallelIn = slave Stream (Bits(width bits))
    val serialOut  = out Bool ()
  }

  val counter = Counter(0, width - 1)
  io.parallelIn.ready := counter.value === 0
  val shiftRegister = RegisterPipeline(
    width        = 1,
    pipeDepth    = width,
    parallelLoad = io.parallelIn.fire,
    parallelIn   = io.parallelIn.payload,
    pipeIn       = False.asBits,
    en           = (counter.value =/= 0 || io.parallelIn.fire)
  )
  when((counter.value === 0 && io.parallelIn.fire) || counter.value =/= 0) {
    counter.increment()
  }
  io.serialOut := shiftRegister.io.pipeOut.asBool
}

object ParallelSerial {
  def apply(width: Int, parallelIn: Stream[Bits]): ParallelSerial = {
    val ret = new ParallelSerial(width)
    parallelIn <> ret.io.parallelIn
    ret
  }
}
