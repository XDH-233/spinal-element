// http://fpgacpu.ca/fpga/Address_Decoder_Behavioural.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class AddressDecoderBeha(addrWidth: Int) extends Component{
    val io = new Bundle{
        val baseAddr = in UInt (addrWidth bits)
        val boundAddr = in UInt(addrWidth bits)
        val addr = in UInt(addrWidth bits)
        val hit = out Bool()
    }

    io.hit := (io.addr >= io.baseAddr) && (io.addr <= io.boundAddr)
}