// http://fpgacpu.ca/fpga/Binary_to_Gray_Reflected.htmlhttp://fpgacpu.ca/fpga/Binary_to_Gray_Reflected.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class BinaryToGrayReflected(wordWidth: Int) extends Component {
  val io = new Bundle {
    val binaryIn = in Bits (wordWidth bits)
    val grayOut  = out Bits (wordWidth bits)
  }

  import lib.bitsConvert._
  io.grayOut := io.binaryIn.toGray
}
