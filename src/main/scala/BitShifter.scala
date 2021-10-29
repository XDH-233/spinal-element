//http://fpgacpu.ca/fpga/Bit_Shifter.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class BitShifter(width: Int) extends Component {
    val io = new Bundle {
        val shiftDirection                     = in Bool ()
        val shiftAmount                        = in UInt (log2Up(width) + 1 bits) // 1->right
        val wordIn, wordInLeft, wordInRight    = in Bits (width bits)
        val wordOut, wordOutLeft, wordOutRight = out Bits (width bits)
    }

    val tmp = Bits(width * 3 bits)
    when(io.shiftDirection) {
        tmp := (io.wordInLeft ## io.wordIn ## io.wordInRight) |>> io.shiftAmount
    } otherwise {
        tmp := (io.wordInLeft ## io.wordIn ## io.wordInRight) |<< io.shiftAmount
    }
    io.wordOutLeft  := tmp(width * 3 - 1 downto width * 2)
    io.wordOut      := tmp(width * 2 - 1 downto width)
    io.wordOutRight := tmp(width - 1 downto 0)
}

object BitShifter {
    def apply(Width: Int, shiftDirection: Bool, shiftAmount: UInt, wordIn: Bits, wordInLeft: Bits, wordInRight: Bits): BitShifter = {
        val ret = BitShifter(Width)
        ret.io.shiftDirection := shiftDirection
        ret.io.shiftAmount    := shiftAmount
        ret.io.wordIn         := wordIn
        ret.io.wordInLeft     := wordInLeft
        ret.io.wordInRight    := wordInRight
        ret
    }
}
