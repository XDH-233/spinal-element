import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.collection.mutable

class PulseGeneratorTest extends AnyFlatSpec {
  simNow()

  def simNow() = {
    SimConfig.withWave
      .withConfig(
        SpinalConfig(
          defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
          defaultClockDomainFrequency  = FixedFrequency(100 MHz)
        )
      )
      .compile {
        val dut = new PulseGenerator()
        dut
      }
      .doSim { dut =>
        import dut._
        val levelQ = mutable.Queue[Boolean]()
        dut.clockDomain.forkStimulus(10)
        io.levelIn #= false
        levelQ.enqueue(false)
        clockDomain.waitSampling()
        for (s <- 0 until 1000) {
          io.levelIn.randomize()
          clockDomain.waitSampling()
          val level = io.levelIn.toBoolean
          levelQ.enqueue(level)
          val lastLevel = levelQ.dequeue()
          assert(io.pulsePosEdgeOut.toBoolean == (!lastLevel && level), "pos")
          assert(io.pulseNegEdgeOut.toBoolean == (lastLevel && !level), "neg")
          assert(io.pulseAnyEdgeOut.toBoolean == (lastLevel ^ level), "any")

        }
      }

  }
}
