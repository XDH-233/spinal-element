import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class MuxOneHotTest extends AnyFlatSpec {
  for (w <- 1 to 9) {
    for (c <- 2 to 5) {
      s"width: ${w}, count: ${c} input" should "work right" in simNow(w, c)
    }
  }

  def simNow(W: Int, C: Int) = {
    SimConfig.withWave
      .compile {
        val dut = new MuxOneHot(W, C)

        dut
      }
      .doSim { dut =>
        import dut._
        import lib.simSupport._
        import io._

        for (s <- 0 until 1000) {
          selectors.randomOneHot
          wordsIn.randomize()
          sleep(1)
          val dataInArr = wordsIn.toBigInt.divide(W, C)
          var sel       = 0
          val selIn     = selectors.toBigInt
          for (i <- 0 until C) {
            if (selIn.testBit(i))
              sel = i
          }
          assert(wordOut.toBigInt == dataInArr(sel))
        }
      }
  }
}
