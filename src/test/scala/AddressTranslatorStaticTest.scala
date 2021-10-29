import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class AddressTranslatorStaticTest extends AnyFlatSpec {
    for (w <- 2 to 8) {
        for (b <- 0 until BigInt(2).pow(w).toInt) {
            s"AddressTranslatorStatic, width: ${w}, base: ${b}" should "work correctly" in simNow(w, b)
        }
    }
    def simNow(W: Int, B: Int) = {
        SimConfig.withWave
            .compile {
                val dut = new AddressTranslatorStatic(W, B)

                dut
            }
            .doSim { dut =>
                import dut._
                import io._

                for (s <- 0 until BigInt(2).pow(W).toInt) {
                    inputAddr.randomize()
                    sleep(1)
                    val addrIn = inputAddr.toInt
                    assert(outputAddr.toInt == (addrIn - B + dut.outAddrDepth) % dut.outAddrDepth)
                }
            }
    }
}
