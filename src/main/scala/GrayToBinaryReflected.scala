// http://fpgacpu.ca/fpga/Gray_to_Binary_Reflected.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class GrayToBinaryReflected(wordWidth: Int) extends Component {
  val io = new Bundle {
    val grayIn    = in Bits (wordWidth bits)
    val binaryOut = out Bits (wordWidth bits)
  }

  import lib.bitsConvert._
  io.binaryOut := io.grayIn.grayToBin
}
