import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import scala.collection.mutable.Queue

class RegisterPipelineSimpleTest extends AnyFlatSpec {
  for (w <- 1 to 9) {
    for (p <- 0 to 5) {
      if (p == 0)
        s"RegisterPipelineSimple ${w} bits 0 pipeDepth" should "work correctly" in simNowZeroPipeDepth(w)
      else
        s"RegisterPipelineSimple ${w} bits ${p} pipeDepth" should "work correctly" in simNow(w, p)
    }
  }
  def simNow(W: Int = 8, P: Int = 4) = {
    SimConfig.withWave
      .withConfig(
        SpinalConfig(
          defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
          defaultClockDomainFrequency  = FixedFrequency(100 MHz)
        )
      )
      .compile(new RegisterPipelineSimple(W, P))
      .doSim { dut =>
        import dut._
        val regQ = Queue[BigInt]()
        dut.clockDomain.forkStimulus(10)
        io.pipeIn #= 0
        Range(0, pipeDepth).foreach(i => regQ.enqueue(BigInt(0)))
        clockDomain.waitSampling()
        for (s <- 0 until 1000) {
          io.pipeIn.randomize()
          clockDomain.waitSampling()
          regQ.enqueue(io.pipeIn.toBigInt)
          assert(io.pipeOut.toBigInt == regQ.dequeue())
        }
      }
  }
  def simNowZeroPipeDepth(W: Int = 8) = {
    SimConfig.withWave
      .compile {
        val dut = new RegisterPipelineSimple(W, 0)
        dut
      }
      .doSim { dut =>
        import dut._
        import io._

        for (s <- 0 until 1000) {
          pipeIn.randomize()
          sleep(1)
          assert(pipeIn.toBigInt == pipeOut.toBigInt)
        }
      }
  }
}
