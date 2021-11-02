import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class NumberOfTrailingZerosTest extends AnyFlatSpec {
    for(w <- 2 to 9){
        s"NumberOfTrailingZeros ${w} bits" should "work correctly " in simNow(w)
    }
    def simNow(W: Int) = {
        SimConfig.withWave
            .compile {
                val dut = new NumberOfTrailingZeros(W)
                dut.all_log.simPublic()
                dut.lsb1.simPublic()
                dut
            }
            .doSim { dut =>
                import dut._
                import lib.simSupport._
                import io._

                wordIn #= 0
                sleep(1)
                for (s <- 0 until 1000) {
                    wordIn.randomize()
                    sleep(1)
                    val dataIn = wordIn.toBigInt
                    val allLog = all_log.toBigInt.divide(log2Up(W), W)
                    val printBlock = {
                    }
                    var count  = 0
                    var addNum = 1
                    for (b <- 0 until width) {
                        if (dataIn.testBit(b)) {
                            addNum = 0
                        }
                        count += addNum
                    }
                    assert(wordOut.toInt == count)
                }
            }
    }
}
