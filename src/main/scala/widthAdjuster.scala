import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class widthAdjuster(wordWidthIn: Int, wordWidthOut: Int, sign: Boolean) extends Component{
    val originalInput = in Bits(wordWidthIn bits)
    val adjustedOutput = out Bits(wordWidthOut bits)
    if(sign)
        adjustedOutput := originalInput.asSInt.resize(wordWidthOut).asBits
    else
        adjustedOutput := originalInput.resize(wordWidthOut)
}
