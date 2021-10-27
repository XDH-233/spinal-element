import org.scalatest.flatspec.AnyFlatSpec
import spinal.core
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class AdderSubtractorBinTest extends AnyFlatSpec {
    for(i <- 2 to 18){
        s"${i} bits adder or subtractor" should "work correctly" in simNow(i)
    }

    def simNow(W: Int)={
        SimConfig.compile(new AdderSubtractorBin(W)).doSim{ dut=>
            import dut._
            import dut.io._
            import lib.simSupport._
            addOrSub #= true
            for(s <- 0 until 10000){
                addOrSub.randomize()
                carryIn.randomize()
                A.randomize()
                B.randomize()
                sleep(1)
                val dataAU = A.toBigInt
                val dataBU = B.toBigInt
                val dataAS = A.toSignBigInt(W)
                val dataBS = B.toSignBigInt(W)
                val sumU = sum.toBigInt
                val sumS = sum.toSignBigInt(W)
                val MAX = BigInt(2).pow(W)
                if(addOrSub.toBoolean){ // sub
                    if(overflow.toBoolean){
                        if(dataBS >= 0){
                            assert(sumS == MAX + dataAS - dataBS - carryIn.toBigInt)
                        }else{
                            assert(sumS == dataAS - dataBS - carryIn.toBigInt - MAX)
                        }
                    }else{
                        assert(sumS == dataAS - dataBS - carryIn.toBigInt)
                    }
                }else{// add
                    if(carryOut.toBoolean){
                        assert(sumU == dataAU + dataBU - MAX + carryIn.toBigInt)
                    }else{
                        assert(sumU == dataAU + dataBU + carryIn.toBigInt)
                    }
                    if(overflow.toBoolean){
                        if(dataAS > 0 || dataBS > 0){ // + plus + -> -
                            assert(sumS == carryIn.toBigInt + dataAS + dataBS - MAX)
                        }else{ // - plus - -> +
                            assert(sumS == carryIn.toBigInt + dataAU + dataBU - MAX)
                        }
                    }else{
                        assert(sumS == dataAS + dataBS + carryIn.toBigInt)
                    }
                }
            }
        }
    }
}
