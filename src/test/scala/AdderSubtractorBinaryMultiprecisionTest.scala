import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class AdderSubtractorBinaryMultiprecisionTest extends AnyFlatSpec {
  List.tabulate(10, 4)((w, s) => s"W: ${s + w + 2}, S: ${w + 2}" should "work correctly " in simNow(s + w + 2, w + 2))

  def simNow(W: Int = 11, S: Int = 4) =
    SimConfig.withWave
      .withConfig(
        SpinalConfig(
          defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
          defaultClockDomainFrequency  = FixedFrequency(100 MHz)
        )
      )
      .compile {
        val dut = new AdderSubtractorBinaryMultiprecision(W, S)
        dut
      }
      .doSim { dut =>
        import dut._
        import lib.simSupport._
        var A      = BigInt(0)
        var B      = BigInt(0)
        var addSub = false
        val max    = (BigInt(1) << W)
        dut.clockDomain.forkStimulus(10)
        io.en           #= false
        io.addSub       #= false
        io.input.valid  #= false
        io.input.A      #= 0
        io.input.B      #= 0
        io.output.ready #= false
        clockDomain.waitSampling()
        for (s <- 0 until 1000) {
          io.en #= true
          io.addSub.randomize()
          io.input.valid.randomize()
          io.input.A.randomize()
          io.input.B.randomize()
          io.output.ready.randomize()
          clockDomain.waitSampling()
          //----------------------------load-------------------------------
          if (io.input.valid.toBoolean && io.input.ready.toBoolean) {
            addSub = io.addSub.toBoolean
            if (!addSub) { // add
              A = io.input.A.toBigInt
              B = io.input.B.toBigInt
            } else { //sub
              A = io.input.A.toSignBigInt
              B = io.input.B.toSignBigInt
            }
          }
          //---------------------------done----------------------------------
          if (io.output.valid.toBoolean && io.output.ready.toBoolean) {
            if (!addSub) { // add
              val gold = A + B
              val carryPart = if(io.output.carryOut.toBoolean) max else BigInt(0)
              val get = io.output.sum.toBigInt + carryPart
              assert(get == gold, "add sum")
            } else { //sub
              val gold = A - B
              val get = io.output.sum.toSignBigInt
              val overflowPart = if(io.output.overflow.toBoolean) max else BigInt(0)
//              assert(get == gold - overflowPart, " sub sum")

              if(B < 0){
                assert(get == gold - overflowPart, " B < 0")
              }else if(A < 0){
                assert((get == gold + overflowPart), "A < 0 , B >= 0")
              }else{
                assert(get == gold, " A >= 0 , B >= 0")
              }

            }
          }
        }

      }

}
