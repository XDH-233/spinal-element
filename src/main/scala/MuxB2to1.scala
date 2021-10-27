//http://fpgacpu.ca/fpga/Multiplexer_Bitwise_2to1.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._


case class MuxB2to1(width: Int) extends Component {
    val io = new Bundle{
        val wordIn0, wordIn1, bitmask = in Bits (width bits)
        val wordOut                   = out Bits (width bits)
    }
    noIoPrefix()
    import io._

    val muxBinBehavArr            = Array.fill(width)(MuxBinBehavVerilog(1, 2))
    muxBinBehavArr.zipWithIndex.foreach { case (mux, index) =>
        mux.selector := bitmask(index).asUInt(1 bits)
        mux.words_in := (wordIn1(index) ## wordIn0(index))
        wordOut(index) := mux.word_out.asBool
    }
}

