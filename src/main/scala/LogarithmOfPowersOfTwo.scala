// http://fpgacpu.ca/fpga/Logarithm_of_Powers_of_Two.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class LogarithmOfPowersOfTwo(width: Int) extends Component {
    val io = new Bundle {
        val oneHotIn           = in Bits (width bits)
        val logarithmOut       = out UInt (log2Up(width) bits)
        val logarithmUndefined = out Bool ()
    }
    val allLogarithms = Vec(UInt(log2Up(width) bits), width)
    allLogarithms.zipWithIndex.foreach { case (l, i) => l := Mux(io.oneHotIn(i), U(i, log2Up(width) bits), U(0, log2Up(width) bits)) }
    io.logarithmOut       := allLogarithms.reduce(_ | _)
    io.logarithmUndefined := (io.oneHotIn === 0)
}

object LogarithmOfPowersOfTwo {
    def apply(width: Int, oneHotIn: Bits): LogarithmOfPowersOfTwo = {
        val ret = LogarithmOfPowersOfTwo(width)
        ret.io.oneHotIn := oneHotIn
        ret
    }
}
