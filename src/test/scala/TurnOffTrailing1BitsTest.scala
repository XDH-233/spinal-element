import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class TurnOffTrailing1BitsTest extends AnyFlatSpec {
    for (w <- 1 to 9) {
        s"${w}  bits input TurnOffTrailing1Bits " should "work correctly " in simNow(w)
    }
    def simNow(W: Int) = {
        SimConfig.withWave
            .compile {
                val dut = new TurnOffTrailing1Bits(W)
                dut
            }
            .doSim { dut =>
                import dut._
                import io._
                for (s <- 0 until 1000) {
                    wordIn.randomize()
                    sleep(1)
                    val dataIn  = wordIn.toBigInt
                    val dataOut = wordOut.toBigInt
                    var posi    = -1
                    var addNum  = 1
                    for (p <- 0 until W) {
                        posi = posi + addNum
                        if (!dataIn.testBit(p)) {
                            addNum = 0
                        }
                    }
                    val gold = dataIn - BigInt(2).pow(posi) + 1
                    if (dataIn == BigInt(2).pow(W) - 1)
                        assert(dataOut == 0, "full \"1\"")
                    else
                        assert(dataOut == gold)
                }
            }
    }
}
