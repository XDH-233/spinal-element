import org.scalatest.flatspec._

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class wordReverserTest extends AnyFlatSpec{
    it should "work normally" in {
        SimConfig.withWave.compile(new WordReverser(wordWidth = 8, wordCount = 4)).doSim{ dut=>
            import dut._
            for(s <- 0 until 100){
                wordsIn.randomize()
                sleep(1)
                val dataIn = wordsIn.toBigInt
                var dataInStr = dataIn.toString(2)
                if(dataInStr.length < wordWidth * wordCount){
                    dataInStr = ("0" * (wordWidth * wordCount - dataInStr.length)) + dataInStr
                }
//                println(dataInStr)
//                println(dataInStr.grouped(wordWidth).map(BigInt(_,2)).toArray.mkString(" "))
                val c = dataInStr.grouped(wordWidth).map(BigInt(_, 2)).toArray.zip(wordsOut.map(_.toBigInt)).foreach{case(gold, res)=> assert(res == gold)}
            }
        }
    }
}