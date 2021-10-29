import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.Console.in

class BitmaskNextwithConstantPopcountVerilogTest extends AnyFlatSpec {
    for (w <- 2 to 8) {
        s"${w} bits input BitmaskNextwithConstantPopcount" should "work correctly" in simNow(w)
    }
    def simNow(W: Int) = {
        SimConfig.withWave
            .compile {
                val dut = new BitmaskNextwithConstantPopcountVerilog(W)

                dut
            }
            .doSim { dut =>
                import dut._
                import io._
                import lib.simSupport._
                sleep(1)
                for (s <- 0 until 1000) {
                    wordIn.randomize()
                    sleep(1)
                    val dataIn   = wordIn.toBigInt
                    val dataOut  = wordOut.toBigInt
                    val countOne = dataIn.popCount(W)
                    var gold     = dataIn
                    var got      = false
                    for (n <- dataIn + 1 until BigInt(2).pow(W)) { // find from dataIn to MAX
                        if (n.popCount(W) == countOne && !got) {
                            gold = n
                            got  = true
                        }
                    }
                    if (!got) { //not find above, find from 0 to dataIn
                        for (n <- BigInt(0) until dataIn) {
                            if (n.popCount(W) == countOne && !got) {
                                gold = n
                                got  = true
                            }
                        }
                    }
                    if (!got) { // void
                        assert(dataOut === BigInt(2).pow(W) - 1, "void")
                    } else {
                        assert((dataOut == gold || io.wordOut == ()), "not void")
                    }
                }
            }
    }
}
