import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class TriadicBooleanOperatorTest extends AnyFlatSpec {
  for (w <- 1 to 9) {
    s"TriadicBooleanOperator ${w} bits" should "work correctly" in simNow(w)
  }
  def simNow(W: Int) = {
    SimConfig.withWave
      .compile {
        val dut = new TriadicBooleanOperator(W)
        dut
      }
      .doSim { dut =>
        import dut._
        import io._
        import lib.simSupport._

        for (s <- 0 until 1000) {
          truthTable0.randomize()
          truthTable1.randomize()
          wordA.randomize()
          wordB.randomize()
          wordC.randomize()
          dual.randomize()
          sleep(1)
          val table0 = truthTable0.toBigInt.divide(1, dyadicBooleanOperations.dyadicTruthTableWidth)
          val table1 = truthTable1.toBigInt.divide(1, dyadicBooleanOperations.dyadicTruthTableWidth)
          val A      = wordA.toBigInt
          val B      = wordB.toBigInt
          val C      = wordC.toBigInt
          val d      = dual.toBoolean
          Range(0, W).foreach { w =>
            var sel = 0
            if (A.testBit(w))
              sel += 2
            if (B.testBit(w))
              sel += 1
            val gold0 = if (C.testBit(w)) table1(sel) == BigInt(1) else table0(sel) == BigInt(1)
            var gold1 = gold0
            if (d)
              gold1 = if (!C.testBit(w)) table1(sel) == BigInt(1) else table0(sel) == BigInt(1)
            assert(result0.toBigInt.testBit(w) == gold0, "0")
            assert(result1.toBigInt.testBit(w) == gold1, "1")
          }
        }
      }
  }
}
