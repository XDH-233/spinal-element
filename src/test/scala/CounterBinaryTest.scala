import org.scalatest.flatspec.AnyFlatSpec
import spinal.core
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.collection.mutable._
import scala.util.Random._

class CounterBinaryTest extends AnyFlatSpec {
  for (t <- 0 until 20) {
    val s = nextInt(7)
    val e = s + nextInt(10) + 1
    val v = nextInt(e - s) + 1
    s"testCase ${t}: CounterBinary from ${s} to ${e}, step is ${v}" should "work correctly" in simNow(s, e, v)
  }

  def simNow(S: BigInt, E: BigInt, I: BigInt): Unit = {
    SimConfig.withWave
      .withConfig(
        SpinalConfig(
          defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
          defaultClockDomainFrequency  = FixedFrequency(100 MHz)
        )
      )
      .compile {
        val dut = new CounterBinary(S, E, I)
        dut
      }
      .doSim { dut =>
        import dut._
        val countQ = Queue[BigInt]()
        dut.clockDomain.forkStimulus(10)
        io.upDown    #= false
        io.load      #= false
        io.run       #= false
        io.loadCount #= S
        clockDomain.waitSampling()
        countQ.enqueue(start)
        for (s <- 0 until 1000) {
          io.upDown.randomize()
          io.loadCount #= scala.util.Random.nextInt((end + 1 - start).toInt) + start
          io.run.randomize()
          io.load.randomize()
          clockDomain.waitSampling()
          val count     = io.count.toBigInt
          val lastCount = countQ.head
          // assert about overflow/underflow
          if (lastCount + incVal > E && count == S) {
            assert(io.overflow.toBoolean, "overflow")
          }
          if (lastCount - incVal < S && count == E) {
            assert(io.underflow.toBoolean, "underflow")
          }
          if (S == 0 && isPow2(E + 1)) { // full use  width
            if (io.run.toBoolean) {
              if (io.load.toBoolean) { // load
                countQ.enqueue(io.loadCount.toBigInt)
              } else if (!io.upDown.toBoolean) { // inc
                if (count + I > E) { // overflow
                  countQ.enqueue((count + I) % (E + 1))
                } else
                  countQ.enqueue(count + I)
              } else { // dec
                if (count - incVal < S)
                  countQ.enqueue((count - incVal + E + 1) % (E + 1))
                else
                  countQ.enqueue(count - incVal)
              }
            } else {
              countQ.enqueue(count)
            }
          } else { // not full use width
            if (io.run.toBoolean) {
              if (io.load.toBoolean) { // load
                countQ.enqueue(io.loadCount.toBigInt)
              } else if (!io.upDown.toBoolean) { // inc
                if (count + I > E) { // overflow
                  countQ.enqueue(S)
                } else
                  countQ.enqueue(count + I)
              } else { // dec
                if (count - incVal < S)
                  countQ.enqueue(E)
                else
                  countQ.enqueue(count - incVal)
              }
            } else {
              countQ.enqueue(count)
            }
          }
          assert(count == countQ.dequeue())
        }
      }

  }
}
