//http://fpgacpu.ca/fpga/Bit_Voting.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class BitVoting(width: Int) extends Component{
    val io = new Bundle{
        val wordIn = in Bits(width bits)
        val unanimityOnes, unanimityZeros, majority, minority = out Bool()
        val tie = if(width % 2 == 0) out Bool() else null
    }
    noIoPrefix()
    import io._

    unanimityOnes.clear()
    unanimityZeros.clear()
    majority.clear()
    minority.clear()
    if(width % 2 ==0)
        tie.clear()
    val popCount = PopulationCount(width)
    popCount.io.wordIn := wordIn
    when(popCount.io.countOut === width){
        unanimityOnes.set()
    }
    when(popCount.io.countOut === 0){
        unanimityZeros.set()
    }
    when(popCount.io.countOut.asUInt >= width / 2 + 1){
        majority.set()
    }
    when(popCount.io.countOut.asUInt <= width - width / 2 - 1){
        minority.set()
    }
    if(width % 2== 0){
        when(popCount.io.countOut=== width / 2){
            tie.set()
        }
    }
}

