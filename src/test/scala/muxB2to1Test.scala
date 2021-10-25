import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class muxB2to1Test extends AnyFlatSpec {
    for(w <- 1 to 16){
        s"${w} bits input" should "work right" in simNow(w)
    }
    def simNow(W: Int)={
        SimConfig.withWave.compile(new muxB2to1(8)).doSim{dut=>
            import dut._
            for(s <- 0 until 1000){
                wordIn0.randomize()
                wordIn1.randomize()
                bitmask.randomize()
                sleep(1)

                val word0 = wordIn0.toBigInt
                val word1 = wordIn1.toBigInt
                val mask = bitmask.toBigInt
                for(w <- 0 until W){
                    val selRes = if(mask.testBit(w)) word1.testBit(w) else word0.testBit(w)
                    assert(wordOut.toBigInt.testBit(w) == selRes)
                }
            }
        }
    }
}
