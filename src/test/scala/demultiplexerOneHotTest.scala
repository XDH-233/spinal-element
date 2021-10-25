import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class demultiplexerOneHotTest extends AnyFlatSpec {
//    s"width: 8, count: 5 and not broadcast" should "work right" in simNow(8, 5, false)
    for(w <- 1 to 16){
        for(c <- 2 to 8){
            s"width: ${w}, count: ${c} and broadcast" should "work right" in simNow(w, c, true)
            s"width: ${w}, count: ${c} and not broadcast" should "work right" in simNow(w, c, false)
        }
    }

    def simNow(W: Int, C: Int, BroadCast: Boolean)={
        SimConfig.withWave.compile{
            val dut = new demultiplexerOneHot(W,C, BroadCast)

            dut
        }.doSim{dut=>
            import dut._
            import lib.simSupport._
            for(s <- 0 until 1000){
                wordIn.randomize()
                selectors #= oneHot(outputCount)
                sleep(1)
                val dataOutArr = wordsOut.toBigInt.divide(W, C)
                val sel = selectors.toBigInt
                assert(validsOut.toBigInt == sel)
                if(BroadCast){
                    dataOutArr.foreach(w=>assert(w ==wordIn.toBigInt))
                }else{
                    for(i <- 0 until outputCount){
                        if(sel.testBit(i))
                            assert(dataOutArr(i)== wordIn.toBigInt)
                        else
                            assert(dataOutArr(i) == BigInt(0))
                    }
                }
            }
        }
    }
}
