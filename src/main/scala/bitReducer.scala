import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

object reducerOp extends Enumeration {
    type operation = Value
    val AND, NAND, OR, NOR, XOR, XNOR = Value
}

case class bitReducer(width: Int, op: reducerOp.Value) extends Component {
    val io = new Bundle {
        val bitsIn = in Bits (width bits)
        val bitOut = out Bool()
    }
    noIoPrefix()
    // asBool make a great difference compare with using Bits.subD...
    op match {
        case reducerOp.AND => io.bitOut := io.bitsIn.asBools.reduce(_ & _)
        case reducerOp.NAND => io.bitOut := io.bitsIn.asBools.reduce((l, r) => ~(l & r))
        case reducerOp.OR => io.bitOut := io.bitsIn.asBools.reduce(_ | _)
        case reducerOp.NOR => io.bitOut := io.bitsIn.asBools.reduce((l, r) => ~(l | r))
        case reducerOp.XOR => io.bitOut := io.bitsIn.asBools.reduce(_ ^ _)
        case reducerOp.XNOR => io.bitOut := io.bitsIn.asBools.reduce((l, r) => ~(l ^ r))
    }
    def toBool(bits: Bits): Bool={
        bits(0)
    }
}