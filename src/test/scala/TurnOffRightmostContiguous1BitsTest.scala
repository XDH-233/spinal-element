import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class TurnOffRightmostContiguous1BitsTest extends AnyFlatSpec {
  for (w <- 1 to 9) {
    s"${w}  bits input TurnOffRightmostContiguous1Bits " should "work correctly " in simNow(w)
  }
  def simNow(W: Int) = {
    SimConfig.withWave
      .compile {
        val dut = new TurnOffRightmostContiguous1Bits(W)
        dut
      }
      .doSim { dut =>
        import dut._
        import io._
        for (s <- 0 until 1000) {
          wordIn.randomize()
          sleep(1)
          val dataIn      = wordIn.toBigInt
          val dataOut     = wordOut.toBigInt
          var startPosi   = -1
          var countOne    = 0
          var countAddNum = 0
          var posiAddNum  = 1
          for (p <- 0 until W) {
            startPosi = startPosi + posiAddNum
            if (dataIn.testBit(p) && countOne == 0) {
              posiAddNum  = 0
              countAddNum = 1
            }
            if (countOne > 0 && !dataIn.testBit(p))
              countAddNum = 0
            countOne      = countOne + countAddNum
          }
          val gold = dataIn - (BigInt(2).pow(countOne) - 1) * BigInt(2).pow(startPosi)
          assert(dataOut == gold)
        }
      }
  }
}
