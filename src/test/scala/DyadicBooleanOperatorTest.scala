import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class DyadicBooleanOperatorTest extends AnyFlatSpec {
    for (w <- 1 to 9) {
        s"DyadicBooleanOperator ${w} bits" should "work right " in simNow(w)
    }
    def simNow(W: Int) = {
        SimConfig.withWave
            .compile {
                val dut = new DyadicBooleanOperator((W))
                dut
            }
            .doSim { dut =>
                import dut._
                import io._
                import lib.simSupport._

                for (s <- 0 until 1000) {
                    wordA.randomize()
                    wordB.randomize()
                    truthTable.randomize()
                    sleep(1)
                    val A     = wordA.toBigInt
                    val B     = wordB.toBigInt
                    val table = truthTable.toBigInt.divide(1, dyadicBooleanOperations.dyadicTruthTableWidth)
                    val res   = result.toBigInt
                    Range(0, W).foreach { w =>
                        var sel = 0
                        if (A.testBit(w))
                            sel += 2
                        if (B.testBit(w))
                            sel += 1
                        val got  = res.testBit(w)
                        val gold = table(sel) == BigInt(1)
                        assert(got == gold, s"${w} bit")
                    }
                }
            }
    }
}
