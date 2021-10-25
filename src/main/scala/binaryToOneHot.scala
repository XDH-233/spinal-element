import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class binaryToOneHot(binaryWidth: Int, outputWidth: Int) extends Component{
    val binaryIn = in Bits(binaryWidth bits)
    val oneHotOut = out Bits(outputWidth bits)
    var addrWidth = log2Up(outputWidth)
    if(binaryWidth > log2Up(outputWidth))
        addrWidth = binaryWidth
    val addrDecoBehaArr = Array.fill(outputWidth)(addressDecoderBeha(addrWidth))
    addrDecoBehaArr.zipWithIndex.foreach{case(dec, index)=>
        dec.baseAddr := index
        dec.boundAddr := index
        dec.addr := binaryIn.resize(addrWidth).asUInt
        oneHotOut(index) := dec.hit
    }
}

