import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.cpu.riscv.impl.Utils.WB
import spinal.lib.fsm._

class Bitmask0BitAtRightmost1BitTest extends AnyFlatSpec {
    for (w <- 1 to 8) {
        s"${w} bits input Bitmask0BitAtRightmost1Bit" should "work correctly" in simNow(w)
    }
    def simNow(W: Int) = {
        SimConfig.withWave
            .compile {
                val dut = new Bitmask0BitAtRightmost1Bit(W)

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
                    var pos     = 0
                    var addNum  = 1
                    for (w <- 0 until W) {
                        if (dataIn.testBit(w))
                            addNum = 0
                        pos += addNum
                    }
                    for (w <- 0 until W) {
                        if (w == pos)
                            assert(dataOut.testBit(w) == false, "0")
                        else
                            assert(dataOut.testBit(w) == true, "1")
                    }
                }
            }
    }
}
