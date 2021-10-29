import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class CarryInBinTest extends AnyFlatSpec {
    for (w <- 0 to 10) {
        s"${w} bits input " should "work correctly" in simNow(w)
    }
    def simNow(W: Int) = {
        SimConfig.withWave
            .compile {
                val dut = new CarryInBin(W)

                dut
            }
            .doSim { dut =>
                import dut._
                import io._

                for (s <- 0 until 1000) {
                    A.randomize()
                    B.randomize()
                    sum.randomize()
                    sleep(1)
                    val dataA   = A.toBigInt
                    val dataB   = B.toBigInt
                    val dataSum = sum.toBigInt
                    assert(carryIn.toBigInt == (dataA ^ dataB ^ dataSum))
                }
            }
    }
}
