import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import scala.collection.mutable.Queue

class ParallelSerialTest extends AnyFlatSpec {
  for (w <- 2 to 9) {
    s"ParallelSerial ${w} bits input" should "work correctly" in simNow(w)
  }

  def simNow(W: Int = 8) = {
    SimConfig.withWave
      .withConfig(
        SpinalConfig(
          defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
          defaultClockDomainFrequency  = FixedFrequency(100 MHz)
        )
      )
      .compile {
        val dut = new ParallelSerial(W)
        dut.counter.value.simPublic()
        dut
      }
      .doSim { dut =>
        import dut._
        import lib.simSupport._

        val paraQ = Queue[Array[BigInt]]()
        dut.clockDomain.forkStimulus(10)
        io.parallelIn.valid   #= false
        io.parallelIn.payload #= 0
        clockDomain.waitSampling()
        paraQ.enqueue(Array.fill(W)(BigInt(0)))
        for (s <- 0 until 1000) {
          io.parallelIn.valid.randomize()
          io.parallelIn.payload.randomize()
          clockDomain.waitSampling()
          val count = counter.value.toInt
          if (io.parallelIn.valid.toBoolean && io.parallelIn.ready.toBoolean) {
            val parallelIn = io.parallelIn.payload.toBigInt.divide(1, W)
            assert(io.serialOut.toBigInt == paraQ.dequeue().head, "fire")
            paraQ.enqueue(parallelIn)
          } else {
            if (count == 0) {
              assert(io.serialOut.toBigInt == paraQ.head(0), "counter is 0 but handshake fail")
            } else {
              assert(io.serialOut.toBigInt == paraQ.head(W - count))
            }
          }
        }
      }
  }
}
