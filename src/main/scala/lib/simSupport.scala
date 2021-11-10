package lib
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

object simSupport {
  implicit class BigIntSim(b: BigInt) {
    def negate(width: Int): BigInt = {
      var res = b
      for (i <- 0 until width) {
        if (b.testBit(i))
          res = res.clearBit(i)
        else
          res = res.setBit(i)
      }
      res
    }

    // divide a BigInt num by bit width
    def divide(width: Int, count: Int): Array[BigInt] = {
      var bStr = b.toString(2)
      if (bStr.length < width * count)
        bStr = "0" * (width * count - bStr.length) + bStr
      bStr.grouped(width).map(BigInt(_, 2)).toArray.reverse
    }

    // BigInt's toString(2) will truncate heading zeros, so I improve this.
    def toBinaryString(width: Int): String = {
      var ret = b.toString(2)
      if (ret.length < width)
        ret = "0" * (width - ret.length) + ret
      ret
    }
    def popCount(width: Int): Int = {
      var ret = 0
      for (w <- 0 until width) {
        if (b.testBit(w))
          ret += 1
      }
      ret
    }
  }

  implicit class BitsSim(b: Bits) {
    def toSignBigInt: BigInt = {
      val width = b.getWidth
      var ret   = b.toBigInt
      if (ret > BigInt(2).pow(width - 1) - 1)
        ret = ret - BigInt(2).pow(width)
      return ret
    }
    def randomOneHot = { b #= 1 << scala.util.Random.nextInt(b.getWidth) }
  }

}
