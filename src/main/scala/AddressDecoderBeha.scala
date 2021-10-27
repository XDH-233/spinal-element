import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class addressDecoderBeha(addrWidth: Int) extends Component{
    val baseAddr = in UInt (addrWidth bits)
    val boundAddr = in UInt(addrWidth bits)
    val addr = in UInt(addrWidth bits)
    val hit = out Bool()
    hit := (addr >= baseAddr) && (addr <= boundAddr)
}