// http://fpgacpu.ca/fpga/Counter_Binary.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import java.lang.Package
import lib.CounterEnhanced

/** @param start
  *   count from start
  * @param end
  *   count to end
  * @param incVal
  *   the value for each increment/decrement
  */
case class CounterBinary(start: BigInt, end: BigInt, incVal: BigInt) extends Component {
  val width = log2Up(end + 1)
  val io = new Bundle {
    val upDown    = in Bool ()
    val run       = in Bool ()
    val load      = in Bool ()
    val loadCount = in UInt (width bits)
    val overflow  = out Bool ()
    val underflow = out Bool ()
    val count     = out UInt (width bits)
  }

  val counterEnhanced = CounterEnhanced(start, end, incVal)
  io.count     := counterEnhanced.value
  io.overflow  := RegNext(counterEnhanced.willOverflow) init (False)
  io.underflow := RegNext(counterEnhanced.willUnderflow) init (False)
  when(io.run) {
    when(io.load) {
      counterEnhanced.load(io.loadCount)
    } elsewhen (io.upDown) {
      counterEnhanced.decrement()
    } otherwise {
      counterEnhanced.increment()
    }
  }
}
