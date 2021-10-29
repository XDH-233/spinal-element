// http://fpgacpu.ca/fpga/Address_Decoder_Static.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class AddressDecoderStatic(width: Int, base: Int, bound: Int) extends Component {
    require(log2Up(bound) < width)
    val io = new Bundle {
        val addr = in UInt (width bits)
        val hit  = out Bool ()
    }
    val perAddrMatch = Vec(Bool, bound - base + 1)
    for (i <- base to bound) {
        perAddrMatch(i - base) := U(i, width bits) === io.addr
    }
    io.hit := perAddrMatch.reduce(_ || _)
}
