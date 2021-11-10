import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import scala.collection.mutable.Queue

class RegisterPipelineTest extends AnyFlatSpec {
  for (w <- 1 to 5) {
    for (p <- 1 to 4) {
      s"RegisterPipeline ${w} bits ${p} pipeDepth" should "work correctly" in simNow(w, p)
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
      .compile {
        val dut = new RegisterPipeline(W, P)
        dut.regs.foreach(_.simPublic())
        dut
      }
      .doSim { dut =>
        import dut._
        import lib.simSupport._
        val dataQ = Queue[BigInt]()
        dut.clockDomain.forkStimulus(10)
        io.parallelLoad #= false
        io.en           #= false
        io.pipeIn       #= 0
        io.parallelIn   #= 0
        clockDomain.waitSampling()
        for (i <- 0 until pipeDepth) {
          dataQ.enqueue(BigInt(0))
        }
        for (s <- 0 until 1000) {
          io.parallelLoad.randomize()
          io.en.randomize()
          io.pipeIn.randomize()
          io.parallelIn.randomize()
          clockDomain.waitSampling()
          val parallelIn  = io.parallelIn.toBigInt.divide(W, P)
          val parallelOut = io.parallelOut.toBigInt.divide(W, pipeDepth)
          if (io.parallelLoad.toBoolean) {
            for (i <- 0 until pipeDepth) {
              assert(parallelOut(i) == dataQ(pipeDepth - 1 - i), s"paraOut, ${i}")
            }
            if (io.en.toBoolean) {
              parallelIn.reverse.foreach(dataQ.enqueue(_))
              for (i <- 0 until pipeDepth) {
                dataQ.dequeue()
              }
            }
          } else {
            if (io.en.toBoolean) {
              dataQ.enqueue(io.pipeIn.toBigInt)
              assert(io.pipeOut.toBigInt == dataQ.dequeue())
            } else {
              assert(io.pipeOut.toBigInt == dataQ.head)
            }

          }
        }
      }
  }
}
