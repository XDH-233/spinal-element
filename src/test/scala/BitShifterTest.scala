import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
class bitShifterTest extends AnyFlatSpec {

    SimConfig.withWave.compile{
        val dut = new BitShifter(8)

        dut
    }.doSim{dut=>
        import dut._
        for(s <- 0 until 100){
            shiftAmount #= scala.util.Random.nextInt(width + 1)
            wordInLeft #= 0
            wordIn.randomize()
            wordInRight #= 0
            shiftDirection.randomize()
            sleep(1)
        }
    }
}
