// http://fpgacpu.ca/fpga/Adder_Subtractor_Binary.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class AdderSubtractorBin(width: Int) extends Component {
    val io = new Bundle {
        val addOrSub     = in Bool () // 0->add
        val carryIn      = in Bool ()
        val A, B         = in Bits (width bits)
        val sum, carries = out Bits (width bits)
        val overflow     = out Bool ()
        val carryOut     = out Bool ()
    }

    val widthAdU = WidthAdjuster(1, width, sign = false)
    val widthAdS = WidthAdjuster(1, width, sign = true)
    widthAdU.io.originalInput := io.carryIn.asBits
    widthAdS.io.originalInput := io.carryIn.asBits

    val BSelected            = Mux(io.addOrSub, ~io.B, io.B)
    val negationOffset: UInt = Mux(io.addOrSub, U(1, width bits), U(0, width bits))
    val carryInSelected      = Mux(io.addOrSub, widthAdS.io.adjustedOutput, widthAdU.io.adjustedOutput)

    val res = Bits(width + 1 bits)
    res := (io.A.resize(width + 1).asUInt + BSelected.resize(width + 1).asUInt + negationOffset
        .resize(width + 1) + carryInSelected.asSInt.resize(width + 1).asUInt).asBits
    io.carryOut := res.msb
    io.sum      := res(width - 1 downto 0)
    io.overflow := io.carries.msb =/= io.carryOut

    val carryInB = CarryInBin(width)
    carryInB.io.A   := io.A
    carryInB.io.B   := BSelected
    carryInB.io.sum := io.sum
    io.carries      := carryInB.io.carryIn
}

object AdderSubtractorBin {
    // Simplify the process of instantiation and wiring
    def apply(width: Int, addOrSub: Bool, carryIn: Bool, A: Bits, B: Bits): AdderSubtractorBin = {
        val adderSubtractorBin = AdderSubtractorBin(width)
        adderSubtractorBin.io.addOrSub := addOrSub
        adderSubtractorBin.io.carryIn  := carryIn
        adderSubtractorBin.io.A        := A
        adderSubtractorBin.io.B        := B
        adderSubtractorBin
    }
}
//verilog souce code in http://fpgacpu.ca/fpga/Adder_Subtractor_Binary.html
case class Adder_Subtractor_Binary(width: Int) extends BlackBox {
    addGenerics(("WORD_WIDTH", width))
    val add_sub      = in Bool ()
    val carry_in     = in Bool ()
    val A, B         = in Bits (width bits)
    val sum, carries = out Bits (width bits)
    val carry_out    = out Bool ()
    val overflow     = out Bool ()
    addRTLPath("./src/verilog/Adder_Subtractor_Binary.v")
    addRTLPath("./src/verilog/Width_Adjuster.v")
    addRTLPath("./src/verilog/CarryIn_Binary.v")
}

case class AddSubVerilog(W: Int) extends Component {
    val addSub       = in Bool ()
    val carryIn      = in Bool ()
    val A, B         = in Bits (W bits)
    val sum, carries = out Bits (W bits)
    val carryOut     = out Bool ()
    val overflow     = out Bool ()
    val v_AdderSub   = Adder_Subtractor_Binary(W)
    v_AdderSub.add_sub  := addSub
    v_AdderSub.A        := A
    v_AdderSub.B        := B
    v_AdderSub.carry_in := carryIn
    carryOut            := v_AdderSub.carry_out
    overflow            := v_AdderSub.overflow
    carries             := v_AdderSub.carries
    sum                 := v_AdderSub.sum
}
