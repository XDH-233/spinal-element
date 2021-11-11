import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import scala.util.Random._

class DutyCycleGeneratorTest extends AnyFlatSpec {
  for(t <- 0 until 10){
    val h = nextInt(100) + 1
    val l = nextInt(100) + 1
    val f = nextBoolean()
    val level = if(f) "high" else "low"
    val w = log2Up(scala.math.max(h, l))
    s"testcase ${t}, ${h} high cycles, ${l} low cycles and ${level} first" should " work correctly" in simNow(w, h,l,f)
  }

  def simNow(W: Int = 8, H: Int, L: Int, F: Boolean) = {
    SimConfig.withWave
      .withConfig(
        SpinalConfig(
          defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
          defaultClockDomainFrequency  = FixedFrequency(100 MHz)
        )
      )
      .compile {
        val dut = new DutyCycleGenerator(W)
        dut
      }
      .doSim { dut =>
        import dut._
        dut.clockDomain.forkStimulus(10)
        dut.init
        dut.test(H, L, F)

      }
    implicit class DutyCycleGenSimMeth(dut: DutyCycleGenerator) {
      import dut._

      def init = {
        io.start      #= false
        io.firstPhase #= false
        io.highCycles #= 0
        io.lowCycles  #= 0
        clockDomain.waitSampling()
      }

      def test(high: Int, low: Int, highFirst: Boolean) = {
        io.start #= false
        clockDomain.waitSampling()
        io.start      #= true
        io.lowCycles  #= low
        io.highCycles #= high
        io.firstPhase #= highFirst
        for (c <- 0 until high + low) {
          clockDomain.waitSampling()
          if (c > 0) {
            if (highFirst) {
              assert(io.dutyCycleOut.toBoolean == (c <= high), "high first")
            } else {
              assert(io.dutyCycleOut.toBoolean == (c > low), "low first")
            }
          }

        }

      }
    }
  }
}
