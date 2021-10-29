import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class reducerNotTopTest extends AnyFlatSpec {
    SimConfig.withWave.compile(new reducerNotTop()).doSim { dut =>
        import dut._
        for (s <- 0 until 1000) {
            data_in.randomize()
            sleep(1)
            assert(NOR.toBoolean  == N_OR.toBoolean)
            assert(NXOR.toBoolean == N_XOR.toBoolean)
            assert(NAND.toBoolean == N_AND.toBoolean)
        }
    }
}
