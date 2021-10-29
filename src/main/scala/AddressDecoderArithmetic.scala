// http://fpgacpu.ca/fpga/Address_Decoder_Arithmetic.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class AddressDecoderArithmetic(width: Int) extends Component {
    val io = new Bundle {
        val addr, baseAddr, boundAddr = in UInt (width bits)
        val hit                       = out Bool ()
    }
    val lowerBound = ArithmeticPredicatesBin(width)
    lowerBound.io.A := io.addr.asBits
    lowerBound.io.B := io.baseAddr.asBits

    val upperBound = ArithmeticPredicatesBin(width)
    upperBound.io.A := io.addr.asBits
    upperBound.io.B := io.boundAddr.asBits

    io.hit := lowerBound.io.gteU && upperBound.io.lteU
}
