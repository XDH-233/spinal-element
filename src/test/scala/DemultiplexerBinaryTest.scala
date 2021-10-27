import org.scalatest.flatspec.AnyFlatSpec
import spinal.core
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class demultiplexerBinaryTest extends AnyFlatSpec {
    simNow(8, 5, false)
    for(w <- 1 to 16){
        for(c <- 2 to 9){
            s"width: ${w}, count: ${c} and broadcast input" should "work correctly" in simNow(w,c, true)
            s"width: ${w}, count: ${c} and not broadcast input" should "work correctly" in simNow(w,c, false)
        }
    }
    def simNow(W: Int, C:Int, B: Boolean)={
        SimConfig.withWave.compile(new DemultiplexerBinary(W, C, B)).doSim{ dut=>
            import dut._
            import lib.simSupport._

            for(s <- 0 until 1000){
                selector.randomize()
                wordIn.randomize()
                sleep(1)
                val sel = selector.toInt
                val dataIn = wordIn.toBigInt
                val dataOutArr = wordsOut.toBigInt.divide(W, C)
                if(sel>= C)
                    assert(validsOut.toBigInt == BigInt(0))
                else
                    assert(validsOut.toBigInt == BigInt(2).pow(sel))

                if(B){
                    dataOutArr.foreach(d=> assert(d == dataIn))
                }else{
                    if(sel >= C)
                        assert(wordsOut.toBigInt == BigInt(0))
                    else{
                        dataOutArr.zipWithIndex.foreach{case(d,i)=> if(i == sel) assert(d == dataIn) else assert(d == BigInt(0))}
                    }
                }

            }
        }
    }
}
