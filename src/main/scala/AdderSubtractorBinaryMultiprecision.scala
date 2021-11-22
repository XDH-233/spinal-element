// http://fpgacpu.ca/fpga/Adder_Subtractor_Binary_Multiprecision.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class inputPorts(wordWidth: Int) extends Bundle with IMasterSlave {
  val A, B  = Bits(wordWidth bits)
  val valid = Bool()
  val ready = Bool()

  override def asMaster(): Unit = {
    out(A, B, valid)
    in(ready)
  }

  def fire: Bool = valid && ready

}

case class outputPorts(wordWidth: Int) extends Bundle with IMasterSlave {
  val sum, carries = Bits(wordWidth bits)
  val ready        = Bool()
  val valid        = Bool()
  val carryOut     = Bool() // sign
  val overflow     = Bool() // signed

  override def asMaster(): Unit = {
    out(sum, carries, valid, carryOut, overflow)
    in(ready)
  }

  def fire: Bool = valid && ready
}

case class AdderSubtractorBinaryMultiprecision(wordWidth: Int, stepWordWidth: Int) extends Component {
  val io = new Bundle {
    val en     = in Bool ()
    val addSub = in Bool () // 0/1 add/sub
    val input  = slave(inputPorts(wordWidth))
    val output = master(outputPorts(wordWidth))
  }

  val stepWordCount      = scala.math.ceil(wordWidth.toDouble / stepWordWidth.toDouble).toInt
  val stepWordWidthTotal = stepWordCount * stepWordWidth
  val padWidth           = stepWordWidthTotal - wordWidth

  // default value of the combinatorial output ports
  io.input.ready  := False
  io.output.valid := False

  val inputLoad   = Bool()
  val calculating = Bool()
  inputLoad   := False
  calculating := False

  // deal with B
  val BSelected = Mux(io.addSub, ~io.input.B, io.input.B)
  val BExtended = BSelected.asSInt.resize(stepWordWidthTotal)
  val BReversed = Bits(stepWordWidthTotal bits)
  val BReverser = WordReverser(
    stepWordWidth,
    stepWordCount,
    wordsIn  = BExtended.asBits,
    wordsOut = BReversed
  )
  val stepB = Bits(stepWordWidth bits)
  val BStorage = RegisterPipeline(
    width        = stepWordWidth,
    pipeDepth    = stepWordCount,
    parallelLoad = inputLoad,
    parallelIn   = BReversed.asBits,
    pipeIn       = B(0, stepWordWidth bits),
    en           = io.en
  )
  stepB := BStorage.io.pipeOut
  val step     = Counter(0, stepWordCount - 1)
  val stepDone = step.willOverflow
  when(calculating) {
    when(inputLoad) {
      step.clear()
    } otherwise {
      step.increment()
    }
  }

  // deal with A
  val AExtended = io.input.A.asSInt.resize(stepWordWidthTotal)
  val AReversed = Bits(stepWordWidthTotal bits)
  val AReverser = WordReverser(
    wordWidth = stepWordWidth,
    wordCount = stepWordCount,
    wordsIn   = AExtended.asBits,
    wordsOut  = AReversed
  )
  val stepA = Bits(stepWordWidth bits)
  val AStorage = RegisterPipeline(
    width        = stepWordWidth,
    pipeDepth    = stepWordCount,
    parallelLoad = inputLoad,
    parallelIn   = AReversed,
    pipeIn       = B(0, stepWordWidth bits),
    en           = io.en
  )
  stepA := AStorage.io.pipeOut

  // add and carry in
  val stepCarryIn = Reg(Bool()) init (False)
  val stepAdder = AdderSubtractorBin(
    width    = stepWordWidth,
    addOrSub = False,
    carryIn  = stepCarryIn,
    A        = stepA,
    B        = stepB
  )
  val stepCarryOut = Bool()
  stepCarryOut := stepAdder.io.carryOut
  val stepCarrySelected = Mux(inputLoad, io.addSub, stepCarryOut)
  when(inputLoad || calculating) {
    stepCarryIn := stepCarrySelected
  }

  // deal with sum
  val outputSum = RegisterPipeline(
    width        = stepWordWidth,
    pipeDepth    = stepWordCount,
    parallelLoad = False,
    parallelIn   = B(0, stepWordWidthTotal bits),
    pipeIn       = stepAdder.io.sum,
    en           = calculating
  )
  val sumReversed = Bits(stepWordWidthTotal bits)
  val sumReverser = WordReverser(
    wordWidth = stepWordWidth,
    wordCount = stepWordCount,
    wordsIn   = outputSum.io.parallelOut,
    wordsOut  = sumReversed
  )
  io.output.sum := sumReversed.asSInt.resize(wordWidth).asBits

  // deal with carries
  val outputCarries = RegisterPipeline(
    width        = stepWordWidth,
    pipeDepth    = stepWordCount,
    parallelLoad = False,
    parallelIn   = B(0, stepWordWidthTotal bits),
    pipeIn       = stepAdder.io.carries,
    en           = calculating
  )
  val carriesReversed = Bits(stepWordWidthTotal bits)
  val carriesReverser = WordReverser(
    wordWidth = stepWordWidth,
    wordCount = stepWordCount,
    wordsIn   = outputCarries.io.parallelOut,
    wordsOut  = carriesReversed
  )
  io.output.carries := carriesReversed.resize(wordWidth)

  // deal with carryOut
  val allCarries  = stepCarryIn ## carriesReversed(stepWordWidthTotal - 1 downto stepWordWidth * (stepWordCount - 1))
  val lastCarryIn = allCarries(stepWordWidth - padWidth - 1)
  io.output.carryOut := allCarries(stepWordWidth - padWidth)
  io.output.overflow := lastCarryIn ^ io.output.carryOut

  val FSM = new StateMachine {
    val LOAD  = new State with EntryPoint
    val CALC  = new State
    val DONE  = new State
    val ERROR = new State

    LOAD.whenIsActive {
      io.input.ready.set()
      when(io.input.fire) {
        inputLoad.set()
        goto(CALC)
      }
    }

    CALC.whenIsActive {
      calculating.set()
      when(stepDone) {
        goto(DONE)
      }
    }

    DONE.whenIsActive {
      io.output.valid.set()
      when(io.output.fire) {
        goto(LOAD)
      }
    }
  }
}
