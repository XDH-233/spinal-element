import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.collection.mutable

class WordChangeDetectorTest extends AnyFlatSpec {
  for (w <- 1 to 9) {
    s"WordChangeDetector ${w} bits input" should "work correctly " in simNow(w)
  }

  def simNow(W: Int) = {
    SimConfig.withWave
      .withConfig(
        SpinalConfig(
          defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
          defaultClockDomainFrequency  = FixedFrequency(100 MHz)
        )
      )
      .compile {
        val dut = new WordChangeDetector(W)
        dut
      }
      .doSim { dut =>
        import dut._
        val wordQ = mutable.Queue[BigInt]()
        dut.clockDomain.forkStimulus(10)
        io.inputWord #= 0
        wordQ.enqueue(BigInt(0))
        clockDomain.waitSampling()
        for (s <- 1 until 1000) {
          io.inputWord.randomize()
          clockDomain.waitSampling()
          val word = io.inputWord.toBigInt
          wordQ.enqueue(word)
          val last = wordQ.dequeue()
          assert(io.outputPulse.toBoolean == (word == last))
        }
      }

  }
}
