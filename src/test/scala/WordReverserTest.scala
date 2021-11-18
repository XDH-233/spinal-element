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
  def simNow(W: Int = 4, C: Int = 5) = {
    SimConfig.withWave.compile(new WordReverser(wordWidth = W, wordCount = C)).doSim { dut =>
      import dut._
      import io._
      import lib.simSupport._

      for (s <- 0 until 100) {
        wordsIn.randomize()
        sleep(1)
        val dataInArr  = wordsIn.toBigInt.divide(W, C)
        val dataOutArr = wordsOut.toBigInt.divide(W, C)
        dataInArr.reverse.zip(dataOutArr).foreach { case (i, o) => assert(i == o) }
      }
    }
  }
}
