package lib

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

object bitsConvert {
  implicit class BitsConvert(b: Bits) {
    def toGray: Bits = {
      val width = b.getWidth
      val ret   = Bits(width bits)
      ret.msb := b.msb
      (0 until width - 1).foreach(i => ret(i) := b(i) ^ b(i + 1))
      ret
    }

    def grayToBin: Bits = {
      val width  = b.getWidth
      val binVec = Vec(Bool, width)
      binVec.head := b.msb
      (1 until width).foreach(i => binVec(i) := binVec(i - 1) ^ b(width - 1 - i))
      binVec.reverse.asBits
    }
  }
}
