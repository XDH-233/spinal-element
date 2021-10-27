import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._


case class populationCount(width: Int) extends Component{
    val wordIn = in Bits(width bits)
    val countOut = out Bits(log2Up(width) + 1 bits)
    countOut := wordIn.subdivideIn(1 bits).map(_.asUInt).map(_.resize(log2Up(width)  + 1)).reduceBalancedTree(_ + _).asBits
}


// verilog source code in  http://fpgacpu.ca/fpga/Population_Count.v
case class Population_Count(wordWidth: Int, popCountWidth:Int) extends BlackBox{
    addGenerics(("WORD_WIDTH ", wordWidth), ("POPCOUNT_WIDTH", popCountWidth))
    val word_in = in Bits(wordWidth bits)
    val count_out = out Bits(popCountWidth bits)
    addRTLPath("./src/verilog/Population_Count.v")
}

case class PopulationCountV(wordWidth: Int, popCountWidth: Int) extends Component{
    val word_in = in Bits(wordWidth bits)
    val count_out = out Bits(popCountWidth bits)
    val u_Population_Count = Population_Count(wordWidth, popCountWidth)
    u_Population_Count.word_in := word_in
    count_out := u_Population_Count.count_out
}

object PopulationCountVSim extends App{
    SimConfig.withWave.compile(new PopulationCountV(32, 6)).doSim{dut=>
        import dut._
        for(s <- 0 until 100){
            word_in.randomize()
            sleep(1)
        }
    }
}

