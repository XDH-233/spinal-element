import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class HammingDistanceTest extends AnyFlatSpec {
    import lib.simSupport._
    for (t <- 2 until 9) {
        s"${t} bits input" should "work right" in simNow(t)
    }
    def simNow(W: Int) = {
        SimConfig.withWave
            .compile {
                val dut = new HammingDistance(W)

                dut
            }
            .doSim { dut =>
                import dut._
                import io._

                for (s <- 0 until 100) {
                    wordA.randomize()
                    wordB.randomize()
                    sleep(1)
                    val A    = wordA.toBigInt
                    val B    = wordB.toBigInt
                    var dist = 0
                    for (w <- 0 until W) {
                        if (A.testBit(w) ^ B.testBit(w))
                            dist += 1
                    }
                    assert(distance.toInt == dist)
                }
            }
    }
}
