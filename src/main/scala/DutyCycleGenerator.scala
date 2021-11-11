// http://fpgacpu.ca/fpga/Duty_Cycle_Generator.html

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class DutyCycleGenerator(countWidth: Int) extends Component {
  val io = new Bundle {
    val start        = in Bool ()
    val finish       = out Bool ()
    val firstPhase   = in Bool () // 0/1 -> low/high
    val highCycles   = in UInt (countWidth bits)
    val lowCycles    = in UInt (countWidth bits)
    val dutyCycleOut = out Bool ()
  }

  io.finish := False
  val phaseCycles = UInt(countWidth bits)
  phaseCycles := 0
  val phaseDuration = PulseDivider(
    wordWidth      = countWidth,
    initialDivisor = 0,
    restart        = False,
    divisor        = phaseCycles,
    pulsesIn       = True
  )

  val endOfPhase      = phaseDuration.io.pulseOut
  val noPhase         = phaseDuration.io.divByZero
  val phaseDone       = endOfPhase || noPhase
  val startLowPhase   = phaseDone && io.start && ~io.firstPhase
  val startNoPhase    = phaseDone && ~io.start
  val startHighPhase  = phaseDone && io.start && io.firstPhase
  val loadOutputValue = Bool()
  val outputValue     = Bool()
  loadOutputValue := True
  outputValue     := False
  io.dutyCycleOut := RegNextWhen(outputValue, loadOutputValue)

  val FSM = new StateMachine {
    val idle       = new State with EntryPoint // 1
    val lowFirst   = new State // 2
    val highSecond = new State // 3
    val highFirst  = new State // 4
    val lowSecond  = new State // 5

    idle.whenIsActive {
      outputValue.clear()
      when(startLowPhase) {
        phaseCycles := io.lowCycles
        outputValue.clear()
        goto(lowFirst)
      }
      when(startHighPhase) {
        outputValue.set()
        phaseCycles := io.highCycles
        goto(highFirst)
      }
    }

    lowFirst.whenIsActive {
      outputValue.clear()
      when(phaseDone) {
        phaseCycles := io.highCycles
        outputValue.set()
        goto(highSecond)
      }
    }

    highSecond.whenIsActive {
      outputValue.set()
      doneToWhere()
      io.finish := phaseDone
    }

    highFirst.whenIsActive {
      outputValue.set()
      when(phaseDone) {
        outputValue.clear()
        phaseCycles := io.lowCycles
        goto(lowSecond)
      }
    }

    lowSecond.whenIsActive {
      outputValue.clear()
      doneToWhere()
      io.finish := phaseDone
    }

    def doneToWhere() = {
      when(startNoPhase) {
        loadOutputValue := False
        goto(idle)
      }
      when(startLowPhase) {
        phaseCycles := io.lowCycles
        outputValue.clear()
        goto(lowFirst)
      }
      when(startHighPhase) {
        phaseCycles := io.highCycles
        outputValue.set()
        goto(highFirst)
      }
    }
  }
}
