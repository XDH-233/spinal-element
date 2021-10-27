import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class bitShifter(width: Int) extends Component{
    val shiftDirection = in Bool()
    val shiftAmount = in UInt(log2Up(width) + 1 bits) // 1->right
    val wordIn, wordInLeft, wordInRight = in Bits(width bits)
    val wordOut, wordOutLeft, wordOutRight = out Bits(width bits)

    val tmp = Bits(width * 3 bits)
    when(shiftDirection){
        tmp := (wordInLeft ## wordIn ## wordInRight) |>> shiftAmount
    }otherwise{
        tmp := (wordInLeft ## wordIn ## wordInRight) |<< shiftAmount
    }
    wordOutLeft := tmp(width * 3- 1 downto width * 2)
    wordOut := tmp(width * 2 - 1 downto width)
    wordOutRight := tmp(width - 1 downto 0)
}