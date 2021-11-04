import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class WordReducerTest extends AnyFlatSpec {
  import reducerOp._
  import lib.simSupport._

  it should "work right in word XOR reducer" in simNow(4, 8, XOR)
  it should "work right in word NXOR reducer" in simNow(4, 8, XNOR)
  it should "work right in word AND reducer" in simNow(4, 8, AND)
  it should "work right in word NAND reducer" in simNow(4, 8, NAND)
  it should "work right in word OR reducer" in simNow(4, 8, OR)
  it should "work right in word NOR reducer" in simNow(4, 8, NOR)

  def simNow(count: Int, width: Int, OP: Value): Unit = {
    SimConfig.withWave
      .compile {
        val dut = new WordReducer(wordCount = count, wordWidth = width, op = OP).setDefinitionName("wordReducer" + OP.toString)
        dut
      }
      .doSim { dut =>
        import dut._
        import io._

        for (s <- 0 until 100) {
          wordsIn.randomize()
          sleep(1)
          val dataIn = wordsIn.toBigInt.divide(width, count)
          val gold   = dataIn.reduce((l, r) => operate(l, r, OP))
          assert(wordOut.toBigInt == gold)
        }
      }
    def operate(a: BigInt, b: BigInt, opt: Value): BigInt = {
      opt match {
        case AND  => a & b
        case NAND => (a & b).negate(width)
        case OR   => a | b
        case NOR  => (a | b).negate(width)
        case XOR  => a ^ b
        case XNOR => (a ^ b).negate(width)
      }
    }

  }

}
