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
  val io = new Bundle {
    val Input  = slave Stream (Bits(width bits))
    val Output = master Stream (Bits(width bits))
  }

  // Regs
  val InReadyReg  = Reg(Bool()) init (True)
  val OutValidReg = Reg(Bool()) init (False)
  val dataOut     = Reg(Bits(width bits)) init (0)
  val dataBuf     = Reg(Bits(width bits)) init (0)

  io.Input.ready    := InReadyReg
  io.Output.payload := dataOut
  io.Output.valid   := OutValidReg
  val FSM = new StateMachine {
    val Empty = new State with EntryPoint
    val Busy  = new State
    val Full  = new State
    Empty.whenIsActive {
      when(io.Input.fire && ~io.Output.fire) { // load
        dataOut     := io.Input.payload
        InReadyReg  := True
        OutValidReg := True
        goto(Busy)
      }
    }
    Busy.whenIsActive {
      when(io.Input.fire && (~io.Output.fire)) { // fill
        dataBuf     := io.Input.payload
        InReadyReg  := False
        OutValidReg := True
        goto(Full)
      } elsewhen ((~io.Input.fire) && io.Output.fire) { // unload
        InReadyReg  := True
        OutValidReg := False
        goto(Empty)
      } elsewhen (io.Input.fire && io.Output.fire) { // flow
        dataOut := io.Input.payload
      }
    }
    Full.whenIsActive {
      when((~io.Input.fire) && io.Output.fire) { // flush
        dataOut     := dataBuf
        InReadyReg  := True
        OutValidReg := True
        goto(Busy)
      }
    }
  }

}

case class ValidReadyPipe(width: Int) extends Component {
  val Input  = slave Stream (Bits(width bits))
  val Output = master Stream (Bits(width bits))
  Output <-/< Input
}
