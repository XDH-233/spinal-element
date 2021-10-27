//http://fpgacpu.ca/fpga/Width_Adjuster.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class WidthAdjuster(wordWidthIn: Int, wordWidthOut: Int, sign: Boolean) extends Component{
    val originalInput = in Bits(wordWidthIn bits)
    val adjustedOutput = out Bits(wordWidthOut bits)
    if(sign)
        adjustedOutput := originalInput.asSInt.resize(wordWidthOut).asBits
    else
        adjustedOutput := originalInput.resize(wordWidthOut)
}

object widthAdjusterSim extends App{
    SimConfig.withWave.compile(new WidthAdjuster(1, 8, true)).doSim{ dut=>
        import dut._
        for(s <- 0 until 100){
            originalInput.randomize()
            sleep(1)
        }
    }
}
