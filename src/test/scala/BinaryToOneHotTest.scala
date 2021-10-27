import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class BinaryToOneHotTest extends AnyFlatSpec {
    for(i <- 1 to 16){
        for(o <- 1 to 16){
            s"input width: ${i}, output width: ${o}" should " work right" in simNow(i,o)
        }
    }
    def simNow(inWidth: Int, outWidth: Int)={
        SimConfig.withWave.compile{
            val dut = new BinaryToOneHot(inWidth, outWidth)

            dut
        }.doSim{dut=>
            import dut._
            import lib.simSupport.oneHot
            import dut.io._
            for(s <- 0 until 1000){
                binaryIn #= oneHot(inWidth)
                sleep(1)
                val dataIn = binaryIn.toInt
                var gold = BigInt(0)
                if(dataIn < outWidth)
                    gold = BigInt(2).pow(dataIn)
                assert(oneHotOut.toBigInt == gold)
            }
        }
    }
}
