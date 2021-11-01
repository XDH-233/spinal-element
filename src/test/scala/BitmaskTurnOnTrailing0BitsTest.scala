import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class BitmaskTurnOnTrailing0BitsTest extends AnyFlatSpec {
    for (w <- 1 to 9) {
        s"${w}  bits input BitmaskTurnOnTrailing0Bits " should "work correctly " in simNow(w)
    }
    def simNow(W: Int) = {
        SimConfig.withWave
            .compile {
                val dut = new BitmaskTurnOnTrailing0Bits(W)
                dut
            }
            .doSim { dut =>
                import dut._
                import io._
                wordIn #= 0
                sleep(1)
                for (s <- 0 until 1000) {
                    wordIn.randomize()
                    sleep(1)
                    val dataIn  = wordIn.toBigInt
                    val dataOut = wordOut.toBigInt
                    if (dataIn.testBit(0))
                        assert(dataOut == BigInt(0), "end with 1") // pass
                    else {
                        var countTailingZero = 0
                        var addNum           = 1
                        for (w <- 0 until W) {
                            if (dataIn.testBit(w))
                                addNum       = 0
                            countTailingZero = countTailingZero + addNum
                        }
                        assert(dataOut == BigInt(2).pow(countTailingZero) - 1)
                    }
                }
            }
    }
}
