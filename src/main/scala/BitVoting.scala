import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class bitVoting(width: Int) extends Component{
    val wordIn = in Bits(width bits)
    val unanimityOnes, unanimityZeros, majority, minority = out Bool()
    val tie = if(width % 2 == 0) out Bool() else null
    unanimityOnes.clear()
    unanimityZeros.clear()
    majority.clear()
    minority.clear()
    if(width % 2 ==0)
        tie.clear()
    val popCount = populationCount(width)
    popCount.wordIn := wordIn
    when(popCount.countOut === width){
        unanimityOnes.set()
    }
    when(popCount.countOut === 0){
        unanimityZeros.set()
    }
    when(popCount.countOut.asUInt >= width / 2 + 1){
        majority.set()
    }
    when(popCount.countOut.asUInt <= width - width / 2 - 1){
        minority.set()
    }
    if(width % 2== 0){
        when(popCount.countOut=== width / 2){
            tie.set()
        }
    }
}

