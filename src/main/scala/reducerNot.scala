import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

// this is the test about the bug about reduction operator in verilog 1364-2001

case class reducerNot() extends BlackBox{

    val data_in = in Bits(8 bits)
    val NOR = out Bool()
    val NAND = out Bool()
    val NXOR = out Bool()
    val N_OR = out Bool()
    val N_XOR= out Bool()
    val N_AND = out Bool()
    addRTLPath("/home/xdh/IdeaProjects/spinal-element/reducerNot.v")
}

case class reducerNotTop() extends Component{
    val data_in = in Bits(8 bits)
    val NOR = out Bool()
    val NAND = out Bool()
    val NXOR = out Bool()
    val N_OR = out Bool()
    val N_XOR = out Bool()
    val N_AND = out Bool()
    val u_reducerNot = reducerNot()
    u_reducerNot.data_in <> data_in
    NOR <> u_reducerNot.NOR
    NAND <> u_reducerNot.NAND
    NXOR <> u_reducerNot.NXOR
    N_XOR <> u_reducerNot.N_XOR
    N_AND <> u_reducerNot.N_AND
    N_OR <> u_reducerNot.N_OR
}

object reducerNotTopSim extends App{
    SimConfig.withWave.compile(new reducerNotTop()).doSim{dut=>
        import dut._
        for(s <- 0 until 1000){
            data_in.randomize()
            sleep(1)
            assert(NOR.toBoolean == N_OR.toBoolean)
            assert(NXOR.toBoolean == N_XOR.toBoolean)
            assert(NAND.toBoolean == N_AND.toBoolean)
        }
    }
}