import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class BitmaskTurnOffTrailing1BitsTest extends AnyFlatSpec {
  for (w <- 1 to 9) {
    s"${w}  bits input BitmaskTurnOffTrailing1Bits " should "work correctly " in simNow(w)
  }
  def simNow(W: Int) = {
    SimConfig.withWave
      .compile {
        val dut = new BitmaskTurnOffTrailing1Bits(W)
        dut
      }
      .doSim { dut =>
        import dut._
        import io._
        wordIn #= 0
        sleep(1)
        for (s <- 0 until 1000) {
          wordIn.randomize()
          sleep(1)
          val dataIn  = wordIn.toBigInt
          val dataOut = wordOut.toBigInt
          if (!dataIn.testBit(0))
            assert(dataOut == BigInt(2).pow(W) - 1, "end with 0")
          else {
            var countTailingOne = 0
            var addNum          = 1
            for (w <- 0 until W) {
              if (!dataIn.testBit(w))
                addNum        = 0
              countTailingOne = countTailingOne + addNum
            }
            assert(dataOut == (BigInt(2).pow(W) - BigInt(2).pow(countTailingOne)))
          }
        }
      }
  }
}
