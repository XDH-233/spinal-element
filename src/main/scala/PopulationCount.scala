import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class PopulationCount(width: Int) extends Component {
  val io = new Bundle {
    val wordIn   = in Bits (width bits)
    val countOut = out Bits (log2Up(width) + 1 bits)
  }

  io.countOut := io.wordIn.subdivideIn(1 bits).map(_.asUInt).map(_.resize(log2Up(width) + 1)).reduceBalancedTree(_ + _).asBits
}

object PopulationCount {
  def apply(width: Int, wordIn: Bits, countOut: Bits): PopulationCount = {
    val ret = new PopulationCount(width)
    ret.io.wordIn := wordIn
    countOut      := ret.io.countOut
    ret
  }
}
