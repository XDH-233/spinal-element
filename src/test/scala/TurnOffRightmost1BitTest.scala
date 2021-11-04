import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class TurnOffRightmost1BitTest extends AnyFlatSpec {
  for (w <- 1 to 8) {
    s"${w} bits input TurnOffRightmost1Bit" should "work correctly" in simNow(w)
  }
  def simNow(W: Int) = {
    SimConfig.withWave
      .compile {
        val dut = new TurnOffRightmost1Bit(W)
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
          var posi    = -1
          var addNum  = 1
          for (p <- 0 until W) {
            posi = posi + addNum
            if (dataIn.testBit(p)) {
              addNum = 0
            }
          }
          val gold = dataIn - BigInt(2).pow(posi)
          if (dataIn == BigInt(0))
            assert(dataOut == BigInt(0), "0")
          else
            assert(dataOut == gold)
        }
      }
  }

}
