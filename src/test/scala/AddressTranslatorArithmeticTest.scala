import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class AddressTranslatorArithmeticTest extends AnyFlatSpec {
    for (w <- 2 to 9) {
        s"${w} bits addr" should "work correctly" in simNow(w)
    }
    def simNow(W: Int) = {
        SimConfig.withWave
            .compile {
                val dut = new AddressTranslatorArithmetic(W)
                dut
            }
            .doSim { dut =>
                import dut._
                import io._

                for (s <- 0 until 1000) {
                    io.inputAddr.randomize()
                    io.offset.randomize()
                    sleep(1)
                    val addrIn  = io.inputAddr.toBigInt
                    val offset  = io.offset.toBigInt
                    val addrOut = io.outputAddr.toBigInt
                    if (addrIn < offset)
                        assert(addrOut == (BigInt(2).pow(W) + addrIn - offset), "addrIn < offset")
                    else
                        assert(addrOut == addrIn - offset)
                }
            }
    }
}
