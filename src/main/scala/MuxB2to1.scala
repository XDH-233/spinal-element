//http://fpgacpu.ca/fpga/Multiplexer_Bitwise_2to1.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class MuxB2to1(width: Int) extends Component {
  val io = new Bundle {
    val wordIn0, wordIn1, bitmask = in Bits (width bits)
    val wordOut                   = out Bits (width bits)
  }

  val muxBinBehavArr = Range(0, width).map { i =>
    MultiplexerBinaryBehavioural(1, 2, selector = io.bitmask(i).asUInt(1 bits), wordsIn = (io.wordIn1(i) ## io.wordIn0(i)))
  }
  muxBinBehavArr.zipWithIndex.foreach { case (m, i) => io.wordOut(i) := m.io.wordOut.asBool }
}

object MuxB2to1 {
  def apply(width: Int, wordIn0: Bits, wordIn1: Bits, bitmask: Bits): MuxB2to1 = {
    val ret = new MuxB2to1(width)
    ret.io.wordIn0 := wordIn0
    ret.io.wordIn1 := wordIn1
    ret.io.bitmask := bitmask
    ret
  }
}
