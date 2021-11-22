// http://fpgacpu.ca/fpga/Adder_Subtractor_Binary_Saturating.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.util.Random

case class AdderSubtractorBinarySaturating(wordWidth: Int) extends Component {
  val io = new Bundle {
    val limitMax      = in Bits (wordWidth bits)
    val limitMin      = in Bits (wordWidth bits)
    val addSub        = in Bool ()
    val carryIn       = in Bool ()
    val A             = in Bits (wordWidth bits)
    val B             = in Bits (wordWidth bits)
    val sum           = out Bits (wordWidth bits)
    val carryOut      = out Bool ()
    val overLimitMax  = out Bool ()
    val underLimitMin = out Bool ()
  }

  io.overLimitMax.clear()
  io.underLimitMin.clear()

  val unsignedExtendedA = io.A.asUInt.resize(wordWidth + 1)
  val unsignedExtendedB = io.B.asUInt.resize(wordWidth + 1)
  val signedExtendedA   = io.A.asSInt.resize(wordWidth + 1)
  val signedExtendedB   = io.B.asSInt.resize(wordWidth + 1)

  val addSum = unsignedExtendedA + unsignedExtendedB + io.carryIn.asUInt
  val subSum = signedExtendedA - signedExtendedB + (io.carryIn ## io.carryIn).asSInt
  io.carryOut := addSum.msb
  when(~io.addSub) {
    when(addSum > io.limitMax.asUInt) {
      io.sum := io.limitMax
      io.overLimitMax.set()
    } elsewhen (addSum < io.limitMin.asUInt) {
      io.sum := io.limitMin
      io.underLimitMin.set()
    } otherwise {
      io.sum := addSum(wordWidth - 1 downto 0).asBits
    }
  } otherwise {
    when(subSum > io.limitMax.asSInt) {
      io.sum := io.limitMax
      io.overLimitMax.set()
    } elsewhen (subSum < io.limitMin.asSInt) {
      io.sum := io.limitMin
      io.underLimitMin.set()
    } otherwise {
      io.sum := subSum.msb ## subSum(wordWidth - 2 downto 0)
    }
  }
}

case class bitsTest() extends Component {
  val dataIn  = in Bits (8 bits)
  val dataOut = out Bits (8 bits)
  dataOut := dataIn
}

object bitsTestSim extends App {
  import lib.simSupport._

  SimConfig.withWave
    .compile {
      val dut = new bitsTest()
      dut
    }
    .doSim { dut =>
      import dut._
      import io._
      dataIn.getSignedValue(-20)
      sleep(1)
      println(dataOut.toSignBigInt.toInt)
      println(dataOut.toSignBigInt)
      for (s <- 0 until 1000) {}
    }
}
