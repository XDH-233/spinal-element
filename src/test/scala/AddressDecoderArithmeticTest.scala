import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class AddressDecoderArithmeticTest extends AnyFlatSpec {
  for (i <- 1 to 9) {
    s"${i} bits input" should "work right " in simNow(i)
  }
  def simNow(W: Int) = {
    SimConfig.withWave
      .compile {
        val dut = new AddressDecoderArithmetic((W))

        dut
      }
      .doSim { dut =>
        import dut._
        import dut.io._
        for (s <- 0 until 1000) {
          baseAddr.randomize()
          boundAddr.randomize()
          addr.randomize()
          sleep(1)
          val base  = baseAddr.toBigInt
          val bound = boundAddr.toBigInt
          val Addr  = addr.toBigInt
          assert(hit.toBoolean == ((Addr >= base) && (Addr <= bound)))
        }
      }
  }
}
