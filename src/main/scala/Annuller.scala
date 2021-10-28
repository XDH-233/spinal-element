// http://fpgacpu.ca/fpga/Annuller.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

object implementation extends  Enumeration{
    type way = Value
    val MUX, AND = Value

}

case class Annuller(width: Int, impWay: implementation.Value) extends Component{
    val io = new Bundle{
        val annul = in Bool()
        val dataIn = in Bits(width bits)
        val dataOut = out Bits(width bits)
    }

    impWay match {
        case implementation.MUX => io.dataOut := Mux(io.annul, B(0), io.dataIn)
        case implementation.AND => {
            val tmp = Bits(width bits)
            tmp.setAllTo(io.annul === False)
            io.dataOut := io.dataIn & (tmp)
        }
    }
}


