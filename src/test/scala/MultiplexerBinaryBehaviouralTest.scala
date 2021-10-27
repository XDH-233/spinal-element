import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class multiplexerBinaryBehaviouralTest extends AnyFlatSpec {
    simNow(8, 5)
//    for (w <- 1 to  32) {
//        for (c <- 2 to 16) {
//            s"word width: ${w} input count: ${c} " should "work right " in simNow(w, c)
//        }
//    }

    def simNow(width: Int, count: Int) = {
        SimConfig.withWave.compile {
            val dut = new MultiplexerBinaryBehavioural(width, count)

            dut
        }.doSim { dut =>
            import dut._
            import lib.simSupport._
            for (s <- 0 until 1000) {
                wordsIn.randomize()
                selector.randomize()
                sleep(1)
                val dataIn    = wordsIn.toBigInt
                val sel       = selector.toInt
                val dataInArr = dataIn.divide(width, count)
                var gold      = BigInt(0)
                if (sel < count) {
                    gold = dataInArr(sel)
                }
                else
                    gold = dataInArr(count - 1)
                assert(wordOut.toBigInt == gold)
            }
        }
    }
}
