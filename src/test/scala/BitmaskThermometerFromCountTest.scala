import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class BitmaskThermometerFromCountTest extends AnyFlatSpec {
  for (w <- 1 to 8) {
    s"${w} bits input BitmaskThermometerFromCount" should "work correctly" in simNow(w)
  }
  def simNow(W: Int) = {
    SimConfig.withWave
      .compile {
        val dut = new BitmaskThermometerFromCount(W)

        dut
      }
      .doSim { dut =>
        import dut._
        import io._

        for (s <- 0 until 1000) {
          countIn.randomize()
          sleep(1)
          val count   = io.countIn.toInt
          val dataOut = io.wrodOut.toBigInt
          var gold    = BigInt(2).pow(count) - 1
          if (count > W)
            gold = BigInt(2).pow(W) - 1
          assert(dataOut == gold)
        }
      }
  }
}
