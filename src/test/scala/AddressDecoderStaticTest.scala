import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class AddressDecoderStaticTest extends AnyFlatSpec {
  for (base <- 0 to 5) {
    for (bound <- base to base + 6) {
      for (w <- log2Up(bound) + 1 to log2Up(bound) + 3) {
        s"base: ${base}, bound: ${bound}, width: ${w}" should "work correctly" in simNow(w, base, bound)
      }
    }
  }
  def simNow(W: Int, Base: Int, Bound: Int) = {
    SimConfig
      .compile {
        val dut = new AddressDecoderStatic(W, Base, Bound)

        dut
      }
      .doSim { dut =>
        import dut._
        import io._

        for (s <- 0 until 1000) {
          addr.randomize()
          sleep(1)
          val gold = addr.toInt >= Base && addr.toInt <= Bound
          assert(hit.toBoolean == gold)
        }
      }
  }
}
