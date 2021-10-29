import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class AddSubVerilogTest extends AnyFlatSpec {
    simNow(8)

    def simNow(Width: Int) = {
        SimConfig.withWave.compile(new AddSubVerilog(Width)).doSim { dut =>
            import dut._
            import lib.simSupport._

            addSub  #= true
            A       #= 186
            B       #= 53
            carryIn #= true
            sleep(1)
            for (s <- 0 until 100) {
                A.randomize()
                B.randomize()
                carryIn.randomize()
                addSub.randomize()
                sleep(1)
                val dataAU = A.toBigInt
                val dataBU = B.toBigInt
                val dataAS = A.toSignBigInt(Width)
                val dataBS = B.toSignBigInt(Width)
                val sumU   = sum.toBigInt
                val sumS   = sum.toSignBigInt(Width)
                val MAX    = BigInt(2).pow(Width)
                println("------------------------------------")
                println(addSub.toBoolean)
                println("carryIn: " + carryIn.toBigInt)
                println("dataAU: " + dataAU)
                println("dataBU: " + dataBU)
                println("sumU: " + sumU)
                println("dataAS: " + dataAS)
                println("dataBS: " + dataBS)
                println("sumS: " + sumS)

                if (addSub.toBoolean) { // sub
                    if (overflow.toBoolean) {
                        if (dataBS >= 0) {
                            assert(sumS == MAX + dataAS - dataBS - carryIn.toBigInt)
                        } else {
                            assert(sumS == dataAS - dataBS - carryIn.toBigInt - MAX)
                        }
                    } else {
                        assert(sumS == dataAS - dataBS - carryIn.toBigInt) // pass
                    }
                } else { // add
                    if (carryOut.toBoolean) {
                        assert(sumU == dataAU + dataBU + carryIn.toBigInt - MAX) //pass
                    } else {
                        assert(sumU == dataAU + dataBU + carryIn.toBigInt) //pass
                    }
                    if (overflow.toBoolean) {
                        if (dataAS > 0 || dataBS > 0) {
                            assert(sumS == carryIn.toBigInt + dataAS + dataBS - MAX) //pass
                        } else {
                            assert(sumS == carryIn.toBigInt + dataAU + dataBU - MAX) //pass
                        }
                    } else {
                        assert(sumS == dataAS + dataBS + carryIn.toBigInt) //pass
                    }
                }
            }
        }
    }
}
