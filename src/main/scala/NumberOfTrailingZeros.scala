// http://fpgacpu.ca/fpga/Number_of_Trailing_Zeros.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class NumberOfTrailingZeros(width: Int) extends Component{
    val io = new Bundle{
        val wordIn = in Bits(width bits)
        val wordOut = out Bits(log2Up(width) + 1 bits)
    }

    val lsb1 = Bits(width bits)
    lsb1 := io.wordIn & (0 - io.wordIn.asSInt).asBits
    val all_log = Bits(width * log2Up(width) bits)
    lsb1.asBools.zipWithIndex.foreach{case(bit, index) => all_log((index + 1) * log2Up(width) - 1 downto index * log2Up(width)) := Mux(bit, B(index, log2Up(width) bits), B(0, log2Up(width) bits))}
    io.wordOut := Mux(io.wordIn === 0, B(width,  log2Up(width) + 1 bits), all_log.subdivideIn(log2Up(width) bits).map(_.asBits).reduce(_ | _).resize(log2Up(width) + 1))
}

