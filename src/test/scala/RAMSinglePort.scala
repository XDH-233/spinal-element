// http://fpgacpu.ca/fpga/RAM_Single_Port.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class RAMSinglePort(wordWidth: Int, depth: Int = 128, useInitFile: Boolean = false, initValue: Seq[BigInt] = null, readNewData: Boolean = false)
    extends Component {
  val io = new Bundle {
    val wren      = in Bool ()
    val addr      = in UInt (log2Up(depth) bits)
    val writeData = in Bits (wordWidth bits)
    val readData  = out Bits (wordWidth bits)
  }

  val mem = if (useInitFile) Mem(Bits(wordWidth bits), initValue.map(B(_))) else Mem(Bits(wordWidth bits), depth)
  // TODO the write first situation neew to be implemented in https://spinalhdl.github.io/SpinalDoc-RTD/master/SpinalHDL/Sequential%20logic/memory.html#automatic-blackboxing
  io.readData := mem.readWriteSync(io.addr, io.writeData, True, io.wren)
}

object RAMSinglePort {
  // not use initFile, not read new data
  def apply(wordWidth: Int, depth: Int, readNewData: Boolean, wren: Bool, addr: UInt, writeData: Bits, readData: Bits): RAMSinglePort = {
    val ret = new RAMSinglePort(wordWidth, depth)
    ret.io.wren      := wren
    ret.io.addr      := addr
    ret.io.writeData := writeData
    readData         := ret.io.readData
    ret
  }
  //
}
