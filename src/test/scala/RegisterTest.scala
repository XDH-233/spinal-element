import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.collection.mutable.Queue

class RegisterTest extends AnyFlatSpec {
  for (w <- 1 to 9) {
    s"Register ${w} bits" should "work correctly " in simNow(w, 0)
  }
  def simNow(W: Int = 8, R: Int = 0) = {
    SimConfig.withWave
      .compile(new Register(W, R))
      .doSim { dut =>
        import dut._
        import lib.simSupport._
        clkDomain.forkStimulus(10)
        val dataQ = Queue[BigInt]()
        io.dataIn #= 0
        dataQ.enqueue(BigInt(0))
        clkDomain.waitSampling()
        for (s <- 0 until 1000) {
          io.dataIn.randomize()
          clkDomain.waitSampling()
          dataQ.enqueue(io.dataIn.toBigInt)
          assert(io.dataOut.toBigInt == dataQ.dequeue())
        }
      }
  }
}
