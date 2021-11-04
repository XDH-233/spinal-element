import org.scalatest.flatspec.AnyFlatSpec
import spinal.core
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class LogarithmOfPowersOfTwoTest extends AnyFlatSpec {
  for (w <- 2 to 9) {
    s"${w} bits input log2()" should "work correctly" in simNow(w)
  }
  def simNow(W: Int) = {
    SimConfig.withWave
      .compile {
        val dut = new LogarithmOfPowersOfTwo(W)

        dut
      }
      .doSim { dut =>
        import dut._
        import io._
        import lib.simSupport._
        oneHotIn #= 0
        sleep(1)
        assert(logarithmUndefined.toBoolean == true, "zero input")
        for (s <- 0 until 1000) {
          oneHotIn.randomOneHot
          sleep(1)
          val dataIn  = oneHotIn.toInt
          val dataOut = logarithmOut.toInt
          assert(dataOut == log2Up(dataIn))
        }
      }
  }
}
