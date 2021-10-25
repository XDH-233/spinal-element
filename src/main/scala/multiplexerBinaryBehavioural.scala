import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class multiplexerBinaryBehavioural(wordWidth: Int, inputCount: Int) extends Component{
    val selector = in UInt(log2Up(inputCount) bits)
    val wordsIn = in Bits(wordWidth * inputCount bits)
    val wordOut = out Bits(wordWidth bits)
    wordOut := Vec(wordsIn.subdivideIn(wordWidth bits).map(_.asBits)).read(selector)




}
// verilog source code in http://fpgacpu.ca/fpga/Multiplexer_Binary_Behavioural.v
case class Multiplexer_Binary_Behavioural(wordWidth: Int, addrWidth: Int, inputCount: Int) extends BlackBox{
    addGenerics(("WORD_WIDTH", wordWidth), ("ADDR_WIDTH", addrWidth), ("INPUT_COUNT", inputCount))
    val selector = in UInt(addrWidth bits)
    val words_in = in Bits(wordWidth * inputCount bits)
    val word_out = out Bits(wordWidth bits)
    addRTLPath("./src/verilog/muxBinaryBehavioral.v")
}

case class muxBinBehav(width: Int, count: Int) extends Component{
    val selector = in UInt(log2Up(count) bits)
    val words_in = in Bits(width * count bits)
    val word_out = out Bits(width bits)

    val b_muxBinBahav = Multiplexer_Binary_Behavioural(width, log2Up(count), count)
    b_muxBinBahav.selector := selector
    b_muxBinBahav.words_in := words_in
    word_out := b_muxBinBahav.word_out
}

object muxBinBehavSim extends App{
    SimConfig.withWave.compile(new muxBinBehav(8, 5)).doSim{dut=>
        import dut._
        for(s <- 0 until 100){
            words_in.randomize()
            selector.randomize()
            sleep(1)
        }
    }
}