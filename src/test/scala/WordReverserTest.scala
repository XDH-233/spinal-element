import org.scalatest.flatspec._

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class WordReverserTest extends AnyFlatSpec {
    for (w <- 1 to 8) {
        for (c <- 2 to 7) {
            s"width: ${w}, count: ${c} input" should "work correctly" in simNow(w, c)
        }
    }
    def simNow(W: Int, C: Int) = {
        SimConfig.withWave.compile(new WordReverser(wordWidth = W, wordCount = C)).doSim { dut =>
            import dut._
            import io._
            for (s <- 0 until 100) {
                wordsIn.randomize()
                sleep(1)
                val dataIn    = wordsIn.toBigInt
                var dataInStr = dataIn.toString(2)
                if (dataInStr.length < wordWidth * wordCount) {
                    dataInStr = ("0" * (wordWidth * wordCount - dataInStr.length)) + dataInStr
                }
                val c = dataInStr.grouped(wordWidth).map(BigInt(_, 2)).toArray.zip(wordsOut.map(_.toBigInt)).foreach { case (gold, res) => assert(res == gold) }
            }
        }
    }
}
