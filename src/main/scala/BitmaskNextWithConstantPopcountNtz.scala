// http://fpgacpu.ca/fpga/Bitmask_Next_with_Constant_Popcount_ntz.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class BitmaskNextWithConstantPopcountNtz(width: Int) extends Component {
    val io = new Bundle {
        val wordIn  = in Bits (width bits)
        val wordOut = out Bits (width bits)
    }
    val findSmallest         = BitmaskIsolateRightmost1Bit(width = width, io.wordIn)
    val calcRipple           = AdderSubtractorBin(width = width, addOrSub = False, carryIn = False, A = io.wordIn, B = findSmallest.io.wordOut)
    val calcFinalShiftAmount = LogarithmOfPowersOfTwo(width, findSmallest.io.wordOut)
    val changedBits: Bits    = io.wordIn ^ calcRipple.io.sum
    val changedBitsCorrected = Mux(calcRipple.io.carryOut, changedBits, changedBits |>> 2)
    val moveChangedBits = BitShifter(
        width          = width,
        shiftDirection = True,
        shiftAmount    = calcFinalShiftAmount.io.logarithmOut.resize(log2Up(width) + 1),
        wordIn         = changedBitsCorrected,
        wordInLeft     = B(0, width bits),
        wordInRight    = B(0, width bits)
    )
    io.wordOut := moveChangedBits.io.wordOut | calcRipple.io.sum
}
