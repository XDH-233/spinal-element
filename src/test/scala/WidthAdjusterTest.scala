import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class WidthAdjusterTest extends AnyFlatSpec {
    for(i <- 1 to 8){
        for(o <- 1 to 8){
            s"input width: ${i}, output width: ${o} and signed" should "work correctly" in simNow(i, o, true)
            s"input width: ${i}, output width: ${o} and unsigned" should "work correctly" in simNow(i, o, false)
        }
    }
    def simNow(I:Int, O: Int, S: Boolean)={
        SimConfig.compile(new WidthAdjuster(I, O, S).setDefinitionName(s"widthAdjuster${I}to${O}" + S.toString)).doSim{ dut=>
            import dut._
            import io._
            import lib.simSupport._
            for(s <- 0 until 100){
                io.originalInput.randomize()
                sleep(1)
                if(O >= I){
                    val dataIn = if(S) originalInput.toSignBigInt(I) else originalInput.toBigInt
                    val dataOut = if(S) adjustedOutput.toSignBigInt(O) else adjustedOutput.toBigInt
                    assert(dataIn == dataIn, "O >= I")
                }else{
                    var dataInUnsigned = originalInput.toBigInt
                    for(b <- O until I){
                        dataInUnsigned = dataInUnsigned.clearBit(b)
                    }
                    assert(adjustedOutput.toBigInt == dataInUnsigned, "O < I")
                }
            }
        }
    }
}
