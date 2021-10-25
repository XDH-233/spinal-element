import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class bitVotingTest extends AnyFlatSpec {
    for(t <- 2 until 33){
        s"${t}" should "work right " in simNow(t)
    }

    def simNow(W: Int)={
        SimConfig.withWave.compile{
            val dut = new bitVoting(W)

            dut
        }.doSim{dut=>
            import dut._
            import lib.simSupport._

            for(s <- 0 until 1000){
                wordIn.randomize()
                sleep(1)
                val dataIn = wordIn.toBigInt
                val count = dataIn.popCount(W)
                assert(unanimityOnes.toBoolean == (count == W))
                assert(unanimityZeros.toBoolean == (count == 0))
                assert(majority.toBoolean == (count >= W / 2 + 1))
                assert(minority.toBoolean == (count <= W - W / 2 - 1))
                if(W % 2 == 0)
                    assert(tie.toBoolean == (count == W / 2))
            }
        }
    }
}
