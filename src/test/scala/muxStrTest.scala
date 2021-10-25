import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class muxStrTest extends AnyFlatSpec {
    for(w <- 1 to   32){
        for(c <- 2 until 16){
            s"width: ${w}, count: ${c} input" should "work right" in simNow(w, c)
        }
    }

    def simNow(W: Int, C:Int)={
        SimConfig.withWave.compile{
            val dut = new muxStr(W,C)

            dut
        }.doSim{dut=>
            import dut._
            import lib.simSupport._
            for(s <- 0 until 100){
                wordsIn.randomize()
                selector.randomize()
                sleep(1)
                val dataInArr = wordsIn.toBigInt.divide(W,C)
                val sel = selector.toInt
                println(dataInArr.mkString(" "))
                println(sel)
                println(wordOut.toBigInt)
                var gold = BigInt(0)
                if(sel < C) {
                    gold = dataInArr(sel)
                }
                assert(wordOut.toBigInt == gold)
            }
        }
    }
}



