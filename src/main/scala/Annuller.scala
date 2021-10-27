import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

object implementation extends  Enumeration{
    type way = Value
    val MUX, AND = Value

}

case class annuller(width: Int, impWay: implementation.Value) extends Component{
    val annul = in Bool()
    val dataIn = in Bits(width bits)
    val dataOut = out Bits(width bits)

    impWay match {
        case implementation.MUX => dataOut := Mux(annul, B(0), dataIn)
        case implementation.AND => {
            val tmp = Bits(width bits)
            tmp.setAllTo(annul === False)
            dataOut := dataIn & (tmp)
        }
    }
}


object annullerRTL extends App{
    SpinalVerilog((new annuller(8, implementation.MUX)).setDefinitionName("annullerMUX"))
    SpinalVerilog((new annuller(8, implementation.AND)).setDefinitionName("annullerAND"))
}
