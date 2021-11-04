import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import scala.collection.mutable.Queue

class RegisterToggleTest extends AnyFlatSpec {
  for (w <- 1 to 9) {
    s"RegisterToggle ${w} bits" should "work correctly " in simNow(w, 0)
  }
  def simNow(W: Int = 8, R: Int = 0) = {
    SimConfig.withWave
      .compile(new RegisterToggle(W, R))
      .doSim { dut =>
        import dut._
        import lib.simSupport._
        clkDomain.forkStimulus(10)
        val dataQ = Queue[BigInt]()
        io.toggle #= false
        io.dataIn #= 0
        dataQ.enqueue(BigInt(0))
        clkDomain.waitSampling()
        for (s <- 0 until 1000) {
          io.toggle.randomize()
          io.dataIn.randomize()
          clkDomain.waitSampling()
          if (io.toggle.toBoolean) {
            dataQ.enqueue(io.dataOut.toBigInt.negate(W))
          } else {
            dataQ.enqueue(io.dataIn.toBigInt)
          }
          assert(io.dataOut.toBigInt == dataQ.dequeue())
        }
      }
  }
}
