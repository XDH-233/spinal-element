// http://fpgacpu.ca/fpga/Address_Translator_Arithmetic.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class AddressTranslatorArithmetic(width: Int) extends Component {
    val io = new Bundle {
        val offset     = in UInt (width bits)
        val inputAddr  = in UInt (width bits)
        val outputAddr = out UInt (width bits)
    }

    val adderSubtractorBin = AdderSubtractorBin(width = width, addOrSub = True, carryIn = False, A = io.inputAddr.asBits, B = io.offset.asBits)
    io.outputAddr := adderSubtractorBin.io.sum.asUInt
}
