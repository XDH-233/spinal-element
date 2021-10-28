//http://fpgacpu.ca/fpga/Bit_Reducer.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

object reducerOp extends Enumeration {
    type operation = Value
    val AND, NAND, OR, NOR, XOR, XNOR = Value
}

case class BitReducer(width: Int, op: reducerOp.Value) extends Component {
    val io = new Bundle {
        val bitsIn = in Bits (width bits)
        val bitOut = out Bool()
    }
    // asBool make a great difference compare with using Bits.subD...
    op match {
        case reducerOp.AND => io.bitOut := io.bitsIn.asBools.reduce(_ & _)
        case reducerOp.NAND => io.bitOut := io.bitsIn.asBools.reduce((l, r) => ~(l & r))
        case reducerOp.OR => io.bitOut := io.bitsIn.asBools.reduce(_ | _)
        case reducerOp.NOR => io.bitOut := io.bitsIn.asBools.reduce((l, r) => ~(l | r))
        case reducerOp.XOR => io.bitOut := io.bitsIn.asBools.reduce(_ ^ _)
        case reducerOp.XNOR => io.bitOut := io.bitsIn.asBools.reduce((l, r) => ~(l ^ r))
    }
}


// this is the test about the bug about reduction operator in verilog 1364-2001
case class reducerNot() extends BlackBox{

    val data_in = in Bits(8 bits)
    val NOR = out Bool()
    val NAND = out Bool()
    val NXOR = out Bool()
    val N_OR = out Bool()
    val N_XOR= out Bool()
    val N_AND = out Bool()
    addRTLPath("src/verilog/reducerNot.v")
}

case class reducerNotTop() extends Component{
    val data_in = in Bits(8 bits)
    val NOR = out Bool()
    val NAND = out Bool()
    val NXOR = out Bool()
    val N_OR = out Bool()
    val N_XOR = out Bool()
    val N_AND = out Bool()
    val u_reducerNot = reducerNot()
    u_reducerNot.data_in <> data_in
    NOR <> u_reducerNot.NOR
    NAND <> u_reducerNot.NAND
    NXOR <> u_reducerNot.NXOR
    N_XOR <> u_reducerNot.N_XOR
    N_AND <> u_reducerNot.N_AND
    N_OR <> u_reducerNot.N_OR
}
