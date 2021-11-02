import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class PopulationCountTest extends AnyFlatSpec {
    for (t <- 2 until 9) {
        s"${t} bits input" should "work right" in simNow(t)
    }
    def simNow(W: Int): Unit = {
        SimConfig.withWave
            .compile {
                val dut = new PopulationCount(W)

                dut
            }
            .doSim { dut =>
                import dut._
                import io._
                for (s <- 0 until 100) {
                    wordIn.randomize()
                    sleep(1)
                    val dataIn = wordIn.toBigInt
                    var count  = 0
                    for (w <- 0 until 1000) {
                        if (dataIn.testBit(w))
                            count += 1
                    }
                    assert(countOut.toInt == count)
                }
            }
    }
}
