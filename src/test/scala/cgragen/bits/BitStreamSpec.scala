package cgragen.bits

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import scala.util.Random

class BitStreamSpec extends AnyFlatSpec {
  behavior of "Bit-stream generation"

  val rng = new Random(42)

  it should "correctly convert between integers and bit values" in {
    val widths = Seq.fill(5) { rng.between(3, 25) }

    widths.foreach { width =>
      val nums = Seq.fill(100) { BigInt(width, rng).toInt }

      nums.foreach { num =>
        bitValues2Int(int2BitValues(num, width)) should equal (num) }
    }
  }
}
