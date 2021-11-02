import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class BitmaskNextWithConstantPopcountPop(width: Int) extends Component {
    val io = new Bundle {
        val wordIn  = in Bits (width bits)
        val wordOut = out Bits (width bits)
    }
    // find smallest
    val findSmallest = BitmaskIsolateRightmost1Bit(width)
    findSmallest.io.wordIn := io.wordIn

    val calcRipple      = AdderSubtractorBin(width = width, addOrSub = False, carryIn = False, A = io.wordIn, B = findSmallest.io.wordOut)
    val calcChangedBits = HammingDistance(width, wordA = io.wordIn, wordB = calcRipple.io.sum)

    val distanceAdjustment: Bits = Mux(calcRipple.io.carryOut, B(0, width bits), B(2, width bits))

    val calcAdjustedDistance =
        AdderSubtractorBin(width = width, addOrSub = True, carryIn = False, A = calcChangedBits.io.distance.resize(width), B = distanceAdjustment)

    val calcShiftedOnes = BitShifter(
        width          = width,
        shiftDirection = False,
        shiftAmount    = calcAdjustedDistance.io.sum.resize(log2Up(width) + 1).asUInt,
        wordIn         = B(1, width bits),
        wordInLeft     = B(0, width bits),
        wordInRight    = B(0, width bits)
    )

    val calcLostOnes = AdderSubtractorBin(width = width, addOrSub = True, carryIn = False, A = calcShiftedOnes.io.wordOut, B = B(1, width bits))
    io.wordOut := calcRipple.io.sum | calcLostOnes.io.sum
}
