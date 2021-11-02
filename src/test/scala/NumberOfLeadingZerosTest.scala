import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class NumberOfLeadingZerosTest extends AnyFlatSpec {
    for (t <- 2 until 9) {
        s"${t} bits input" should "work right" in simNow(t)
    }

    def simNow(W: Int) = {
        SimConfig
            .compile {
                val dut = new NumberOfLeadingZeros(W)

                dut
            }
            .doSim { dut =>
                import dut._
                import io._

                for (s <- 0 until 1000) {
                    wordIn.randomize()
                    sleep(1)
                    val dataIn = wordIn.toBigInt
                    var count  = 0
                    var addNum = 1
                    for (w <- (0 until W).reverse) {
                        if (dataIn.testBit(w))
                            addNum = 0
                        count += addNum
                    }
                }
            }
    }
}
