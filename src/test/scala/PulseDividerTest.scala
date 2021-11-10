import org.scalatest.flatspec.AnyFlatSpec
import spinal.core
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class PulseDividerTest extends AnyFlatSpec {
  for (w <- 2 to 9) {
    s"PulseDivider ${w} bits" should "work correctly " in simNow(w, 1)
  }
  def simNow(W: Int, I: Int) = {
    SimConfig.withWave
      .withConfig(
        SpinalConfig(
          defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
          defaultClockDomainFrequency  = FixedFrequency(100 MHz)
        )
      )
      .compile {
        val dut = new PulseDivider(W, I)
        dut
      }
      .doSimUntilVoid { dut =>
        import dut._
        dut.clockDomain.forkStimulus(10)

        io.pulsesIn #= false
        io.divisor  #= 1
        io.restart  #= false
        clockDomain.waitSampling()
        val restartThread = fork {
          for (s <- 0 until 1000) {
            val R = scala.util.Random.nextBoolean()
            io.restart #= R
            if (R) {
              clockDomain.waitSampling()
            } else {
              val holdTime = scala.util.Random.nextInt(BigInt(2).pow(W).toInt - 2) + 2
              clockDomain.waitSampling(holdTime)
            }
          }
        }
        val pulseCountThread = fork {
          var divisor = initialDivisor
          var count   = 0
          var getZero = false
          for (s <- 0 until 1000) {
            io.pulsesIn.randomize()
            io.divisor.randomize()
            clockDomain.waitSampling()
            val restart = io.restart.toBoolean
            val pulseIn = io.pulsesIn.toBoolean
            if (getZero) {
              assert(io.divByZero.toBoolean, "div by zero")
              getZero = false
            }
            if (restart) {
              divisor = io.divisor.toInt
              count   = 0
              if (divisor == 0) {
                getZero = true
              }
            } else {
              if (pulseIn) {
                if (count == divisor - 1) {
                  divisor = io.divisor.toInt
                  count   = 0
                  if (divisor == 0) {
                    getZero = false
                  }
                  assert(io.pulseOut.toBigInt == BigInt(1))
                } else {
                  count += 1
                }
              }
            }
          }
          simSuccess()
        }

      }

  }
}
