package lib

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class CounterEnhanced(val start: BigInt, val end: BigInt, incVal: BigInt = 1) extends ImplicitArea[UInt] {
  require(start <= end)
  val willLoad      = False.allowOverride
  val willIncrement = False.allowOverride // 0 -> dec, 1 -> inc
  val willDecrement = ~willIncrement
  val willClear     = False.allowOverride
  val run           = False.allowOverride
  val loadValue     = U(0, log2Up(end + 1) bits)

  def clear(): Unit = willClear := True

  def increment(): Unit = {
    willIncrement := True
    run           := True
  }

  def decrement(): Unit = {
    willIncrement := False
    run           := True
  }

  def load(Val: BigInt): Unit = {
    willLoad  := True
    run       := True
    loadValue := U(Val).resized
  }

  def load(Val: UInt) = {
    willLoad  := True
    run       := True
    loadValue := Val
  }

  val valueNext, valueCountNext = UInt(log2Up(end + 1) bit)
  val value                     = RegNext(valueNext) init (start)
  val willOverflowIfInc         = value +^ incVal > end
  val willOverflow              = willOverflowIfInc && willIncrement && (~willLoad) && run
  val willUnderflowIfDec        = value < start + incVal
  val willUnderflow             = willUnderflowIfDec && willDecrement && (~willLoad) && run
  valueCountNext               := value
  valueNext                    := Mux(run, valueCountNext, value)
  if (isPow2(end + 1) && start == 0) { //Check if using overflow follow the spec
    when(willLoad) {
      valueCountNext := loadValue
    } elsewhen (willIncrement) {
      valueCountNext := value + incVal
    } otherwise {
      valueCountNext := value - incVal
    }
  } else {
    when(willLoad) {
      valueCountNext := loadValue
    } elsewhen (willOverflow) {
      valueCountNext := U(start)
    } elsewhen (willUnderflow) {
      valueCountNext := U(end)
    } otherwise {
      when(willIncrement) {
        valueCountNext := value + incVal
      } otherwise {
        valueCountNext := value - incVal
      }
    }
  }
  when(willClear) {
    valueNext := start
  }

  willOverflowIfInc.allowPruning
  willOverflow.allowPruning
  willUnderflow.allowPruning
  willUnderflowIfDec.allowPruning
  willLoad.allowPruning
  override def implicitValue: UInt = this.value
}

object CounterEnhanced {
  def apply(start: BigInt, end: BigInt, incVal: BigInt): CounterEnhanced = new CounterEnhanced(start, end, incVal)
}
