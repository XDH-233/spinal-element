// http://fpgacpu.ca/fpga/Pipeline_Skid_Buffer.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.collection.mutable.Queue

//----------------stateMachine-------------
//
//                 /--\ +- flow
//                 |  |
//          load   |  v   fill
// -------   +    ------   +    ------
//|       | ---> |      | ---> |      |
//| Empty |      | Busy |      | Full |
//|       | <--- |      | <--- |      |
// -------    -   ------    -   ------
//         unload         flush
//------------------------------------------

case class PipeSkidBuffer(width: Int) extends Component {
    val io = new Bundle{
        val Input  = slave Stream (Bits(width bits))
        val Output = master Stream (Bits(width bits))
    }
    noIoPrefix()
    import io._

    // Regs
    val InReadyReg  = Reg(Bool()) init (True)
    val OutValidReg = Reg(Bool()) init (False)
    val dataOut     = Reg(Bits(width bits)) init (0)
    val dataBuf     = Reg(Bits(width bits)) init (0)

    Input.ready := InReadyReg
    Output.payload := dataOut
    Output.valid := OutValidReg
    val FSM = new StateMachine{
        val Empty = new State with EntryPoint
        val Busy = new State
        val Full = new State
        Empty.whenIsActive{
            when(Input.fire && ~Output.fire){ // load
                dataOut := Input.payload
                InReadyReg := True
                OutValidReg := True
                goto(Busy)
            }
        }
        Busy.whenIsActive{
            when(Input.fire && (~Output.fire)){ // fill
                dataBuf := Input.payload
                InReadyReg := False
                OutValidReg := True
                goto(Full)
            }elsewhen((~Input.fire) && Output.fire){ // unload
                InReadyReg := True
                OutValidReg := False
                goto(Empty)
            }elsewhen(Input.fire && Output.fire){ // flow
                dataOut := Input.payload
            }
        }
        Full.whenIsActive{
            when((~Input.fire) && Output.fire){ // flush
                dataOut := dataBuf
                InReadyReg := True
                OutValidReg := True
                goto(Busy)
            }
        }
    }

}


case class validReadyPipe(width: Int) extends Component {
    val Input  = slave Stream (Bits(width bits))
    val Output = master Stream (Bits(width bits))
    Output <-/< Input
}


