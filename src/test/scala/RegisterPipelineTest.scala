import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import scala.collection.mutable.Queue

class RegisterPipelineTest extends AnyFlatSpec {
    simNow()
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
                io.pipeIn       #= 0
                io.parallelIn   #= 0
                clockDomain.waitSampling()
                for (i <- 0 until pipeDepth) {
                    dataQ.enqueue(BigInt(0))
                }
                for (s <- 0 until 1000) {
                    io.parallelLoad.randomize()
                    io.pipeIn.randomize()
                    io.parallelIn.randomize()
                    clockDomain.waitSampling()
                    if (io.parallelLoad.toBoolean) {
                        val parallelIn  = io.parallelIn.toBigInt.divide(W, P)
                        val parallelOut = io.parallelOut.toBigInt.divide(W, pipeDepth)
                        for (i <- 0 until pipeDepth) {
                            assert(parallelOut(i) == dataQ(pipeDepth - 1 - i), s"paraOut, ${i}")
                        }
                        parallelIn.reverse.foreach(dataQ.enqueue(_))
                        for (i <- 0 until pipeDepth) {
                            dataQ.dequeue()
                        }
                    } else {
                        dataQ.enqueue(io.pipeIn.toBigInt)
                        assert(io.pipeOut.toBigInt == dataQ.dequeue())
                    }
                }
            }
    }
}
