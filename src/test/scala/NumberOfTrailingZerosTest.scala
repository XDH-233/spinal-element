import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class NumberOfTrailingZerosTest extends AnyFlatSpec {
    it should "work right" in {
        simNow(8)
//        for(l <- 2 until 33){
//            simNow(l)
//        }
    }

    def simNow(W: Int) = {
        SimConfig.withWave
            .compile {
                val dut = new NumberOfTrailingZeros(W)
                dut.all_log.simPublic()
                dut.lsb1.simPublic()
                dut
            }
            .doSim { dut =>
                import dut._
                import lib.simSupport._
                import io._

                wordIn #= 0
                sleep(1)
                for (s <- 0 until 1000) {
                    wordIn.randomize()
                    sleep(1)
                    val dataIn = wordIn.toBigInt
                    val allLog = all_log.toBigInt.divide(log2Up(W), W)
                    val printBlock = {
//                    println("-------------------------------")
//                    println("wordIn: " + dataIn.toString(2))
//                    println("lsb1: " + lsb1.toBigInt.toString(2))
//                    println(allLog.mkString(" "))
//                    println("wordOut: " + wordOut.toBigInt)
                    }
                    var count  = 0
                    var addNum = 1
                    for (b <- 0 until width) {
                        if (dataIn.testBit(b)) {
                            addNum = 0
                        }
                        count += addNum
                    }
                    assert(wordOut.toInt == count)
                }
            }
    }
}
