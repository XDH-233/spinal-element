import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class demultiplexerOneHot(wordWidth: Int, outputCount: Int, broadcast: Boolean) extends Component{
    val selectors = in  Bits(outputCount bits)
    val wordIn = in Bits(wordWidth bits)
    val wordsOut = out Bits(wordWidth * outputCount bits)
    val validsOut = out Bits(outputCount bits)

    validsOut := selectors
    if(broadcast){
        val wordOutVec = Vec(Bits(wordWidth bits), outputCount)
        wordOutVec.foreach(_:=wordIn)
        wordsOut := wordOutVec.reduce(_ ## _)
    }
    else{
        val annullerArr = Array.fill(outputCount)(Annuller(wordWidth, implementation.AND))
        annullerArr.zipWithIndex.foreach{case(ann, index)=>
            ann.annul := ~selectors(index)
            ann.dataIn := wordIn
            wordsOut((index + 1) * wordWidth - 1 downto index * wordWidth) := ann.dataOut
        }
    }
}

