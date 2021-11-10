import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import scala.collection.mutable.Queue

class SerialParallelTest extends AnyFlatSpec {
  for (w <- 2 to 7) {
    s"SerialParallel ${w} bits" should "work correctly" in simNow(w)
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
        val dut = new SerialParallel(W)
        dut.counter.value.simPublic()
        dut
      }
      .doSim { dut =>
        import dut._
        val dataQ = Queue[BigInt]()
        for (i <- 0 until W) {
          dataQ.enqueue(BigInt(0))
        }
        dut.clockDomain.forkStimulus(10)
        io.serialIn          #= false
        io.parallelOut.ready #= false
        clockDomain.waitSampling()
        for (s <- 0 until 2000) {
          io.serialIn.randomize()
          io.parallelOut.ready.randomize()
          clockDomain.waitSampling()
          val Count = counter.value.toInt
          if (Count == 0 && io.parallelOut.ready.toBoolean) {
            val gold = BigInt(dataQ.map(_.toString(2)).map(_.toString).reduce(_ + _), 2)
            assert(io.parallelOut.payload.toBigInt == gold)
          }
          if (Count != 0 || (Count == 0 && io.parallelOut.ready.toBoolean)) {
            dataQ.enqueue(io.serialIn.toBigInt)
            dataQ.dequeue()
          }
        }
      }
  }
}
