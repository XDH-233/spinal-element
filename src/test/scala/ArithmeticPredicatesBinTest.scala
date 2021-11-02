import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class ArithmeticPredicatesBinTest extends AnyFlatSpec {
    for (w <- 2 to 9) {
        s"${w} bits design" should "work correctly " in simNow(w)
    }
    def simNow(W: Int) = {
        SimConfig
            .compile {
                val dut = new ArithmeticPredicatesBin(W)

                dut
            }
            .doSim { dut =>
                import dut._
                import io._
                import lib.simSupport._

                for (s <- 0 until 1000) {
                    A.randomize()
                    B.randomize()
                    sleep(1)
                    val dataAU = A.toBigInt
                    val dataAS = A.toSignBigInt
                    val dataBU = B.toBigInt
                    val dataBS = B.toSignBigInt
                    assert(eq_.toBoolean  == (dataAU == dataBU), "eq")
                    assert(ltU.toBoolean  == (dataAU < dataBU), "ltU")
                    assert(lteU.toBoolean == (dataAU <= dataBU), "lteU")
                    assert(gtU.toBoolean  == (dataAU > dataBU), "gtU")
                    assert(gteU.toBoolean == (dataAU >= dataBU), "gteU")
                    assert(ltS.toBoolean  == (dataAS < dataBS), "ltS")
                    assert(lteS.toBoolean == (dataAS <= dataBS), "lteS")
                    assert(gtS.toBoolean  == (dataAS > dataBS), "gtS")
                    assert(gteS.toBoolean == (dataAS >= dataBS), "gteS")
                }
            }
    }
}
