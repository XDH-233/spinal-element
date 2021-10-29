import org.scalatest.flatspec._
import reducerOp.XOR
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class BitReducerTest extends AnyFlatSpec {
    import reducerOp._

    "bits reducer " should "work as AND" in {
        for (i <- 2 until 20) {
            simNow(i, AND)
        }
    }
    "bits reducer " should "work as NAND" in {
        for (i <- 2 until 20) {
            simNow(i, NAND)
        }
    }
    "bits reducer " should "work as OR" in {
        for (i <- 2 until 20) {
            simNow(i, AND)
        }
    }
    "bits reducer " should "work as NOR" in {
        for (i <- 2 until 20) {
            simNow(i, NOR)
        }
    }
    "bits reducer " should "work as XOR" in {
        for (i <- 2 until 20) {
            simNow(i, XOR)
        }
    }
    "bits reducer " should "work as XNOR" in {
        for (i <- 2 until 20) {
            simNow(i, XNOR)
        }
    }
    def simNow(width: Int, OP: reducerOp.Value) = {
        SimConfig.withWave.compile((new BitReducer(width, OP)).setDefinitionName("reducer" + OP.toString)).doSim { dut =>
            import dut._
            for (s <- 0 until 1000) {
                io.bitsIn.randomize()
                sleep(1)
                val dataIn = io.bitsIn.toBigInt
                val res    = io.bitOut.toBigInt
                var bitOut = BigInt(0)
                if (dataIn.testBit(0))
                    bitOut = BigInt(1)

                for (b <- 1 until dut.width) {
                    if (dataIn.testBit(b))
                        bitOut = operate(bitOut, BigInt(1), OP)
                    else
                        bitOut = operate(bitOut, BigInt(0), OP)
                }
                assert(bitOut == res)
            }

            def operate(a: BigInt, b: BigInt, opt: reducerOp.Value): BigInt = {
                opt match {
                    case AND  => a & b
                    case NAND => if ((a & b) == BigInt(0)) BigInt(1) else BigInt(0)
                    case OR   => a | b
                    case NOR  => if ((a | b) == BigInt(0)) BigInt(1) else BigInt(0)
                    case XOR  => a ^ b
                    case XNOR => if ((a ^ b) == BigInt(0)) BigInt(1) else BigInt(0)
                }
            }
        }

    }
}
