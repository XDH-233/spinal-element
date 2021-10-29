import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import scala.collection.mutable.Queue

class PipeSkidBufferTest extends AnyFlatSpec {
    "pipeSkidBuffer" should "work normally " in {
        SimConfig.withWave
            .withConfig(
                SpinalConfig(
                    defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
                    defaultClockDomainFrequency  = FixedFrequency(100 MHz)
                )
            )
            .compile(new PipeSkidBuffer(8))
            .doSim { dut =>
                import dut._
                import scala.util.Random
                import io._

                dut.clockDomain.forkStimulus(10)
                Input.valid   #= false
                Input.payload #= 0
                Output.ready  #= false
                clockDomain.waitSampling()
                val Que = Queue[BigInt]()
                for (s <- 0 until 10000) {
                    val inputValid   = Random.nextBoolean()
                    val outputReady  = Random.nextBoolean()
                    val inputPayload = BigInt(dut.width, Random)

                    Input.valid   #= inputValid
                    Input.payload #= inputPayload
                    Output.ready  #= outputReady
                    clockDomain.waitSampling()
                    val inputReady    = Input.ready.toBoolean
                    val outputValid   = Output.valid.toBoolean
                    val outputPayload = Output.payload.toBigInt
                    if (inputValid && inputReady) {
                        Que.enqueue(inputPayload)
                    }
                    if (outputValid && outputReady) {
                        assert(outputPayload == Que.dequeue()) // assert
                    }
                }
            }
    }
}
