import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class BitmaskNextwithConstantPopcount(width: Int) extends Component {
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
        Width          = width,
        shiftDirection = False,
        shiftAmount    = calcAdjustedDistance.io.sum.resize(log2Up(width) + 1).asUInt,
        wordIn         = B(1, width bits),
        wordInLeft     = B(0, width bits),
        wordInRight    = B(0, width bits)
    )

    val calcLostOnes = AdderSubtractorBin(width = width, addOrSub = True, carryIn = False, A = calcShiftedOnes.io.wordOut, B = B(1, width bits))
    io.wordOut := calcRipple.io.sum | calcLostOnes.io.sum
}

// verilog source code --> BlackBox --> Component

case class Bitmask_Next_with_Constant_Popcount_pop(width: Int) extends BlackBox {
    addGenerics(("WORD_WIDTH", width))
    val word_in  = in Bits (width bits)
    val word_out = out Bits (width bits)
    addRTLPath("./src/verilog/Bitmask_Next_with_Constant_Popcount_pop.v")
    addRTLPath("./src/verilog/Adder_Subtractor_Binary.v")
    addRTLPath("./src/verilog/Hamming_Distance.v")
    addRTLPath("./src/verilog/Word_Reducer.v")
    addRTLPath("./src/verilog/Bit_Reducer.v")
    addRTLPath("./src/verilog/Bit_Shifter.v")
    addRTLPath("./src/verilog/Population_Count.v")
    addRTLPath("./src/verilog/Width_Adjuster.v")
    addRTLPath("./src/verilog/CarryIn_Binary.v")
    addRTLPath("./src/verilog/Bitmask_Isolate_Rightmost_1_Bit.v")
}

case class BitmaskNextwithConstantPopcountVerilog(width: Int) extends Component {
    val io = new Bundle {
        val wordIn  = in Bits (width bits)
        val wordOut = out Bits (width bits)
    }
    val bitmask_Next_with_Constant_Popcount_pop = Bitmask_Next_with_Constant_Popcount_pop(width)
    bitmask_Next_with_Constant_Popcount_pop.word_in := io.wordIn
    io.wordOut                                      := bitmask_Next_with_Constant_Popcount_pop.word_out
}
