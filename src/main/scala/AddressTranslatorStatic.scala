// http://fpgacpu.ca/fpga/Address_Translator_Static.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class AddressTranslatorStatic(outputAddrWidth: Int, inputAddrBase: Int) extends Component {
    val outAddrDepth = BigInt(2).pow(outputAddrWidth).toInt
    require(inputAddrBase < outAddrDepth)
    val io = new Bundle {
        val inputAddr  = in UInt (outputAddrWidth bits)
        val outputAddr = out UInt (outputAddrWidth bits)
    }
    val initialContents = Array.fill(outAddrDepth)(U(0, outputAddrWidth bits))
    Range(0, outAddrDepth).foreach(i => initialContents((inputAddrBase + i) % outAddrDepth) = U(i, outputAddrWidth bits))
    val ram = Mem(UInt(outputAddrWidth bits), initialContent = initialContents)
    io.outputAddr := ram.readAsync(io.inputAddr)
}
