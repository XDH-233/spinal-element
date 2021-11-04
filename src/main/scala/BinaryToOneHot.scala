// http://fpgacpu.ca/fpga/Binary_to_One_Hot.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class BinaryToOneHot(binaryWidth: Int, outputWidth: Int) extends Component {
  val io = new Bundle {
    val binaryIn  = in Bits (binaryWidth bits)
    val oneHotOut = out Bits (outputWidth bits)
  }

  var addrWidth = log2Up(outputWidth)
  if (binaryWidth > log2Up(outputWidth))
    addrWidth = binaryWidth
  val addrDecoBehaArr = Array.fill(outputWidth)(AddressDecoderBeha(addrWidth))
  addrDecoBehaArr.zipWithIndex.foreach { case (dec, index) =>
    dec.io.baseAddr     := index
    dec.io.boundAddr    := index
    dec.io.addr         := io.binaryIn.resize(addrWidth).asUInt
    io.oneHotOut(index) := dec.io.hit
  }
}

object BinaryToOneHot {
  def apply(binaryWidth: Int, outputWidth: Int, bianryIn: Bits): BinaryToOneHot = {
    val ret = new BinaryToOneHot(binaryWidth, outputWidth)
    ret.io.binaryIn := bianryIn
    ret
  }
}
