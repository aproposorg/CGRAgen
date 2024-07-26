package cgragen

import scala.collection.mutable
import scala.language.implicitConversions

package object bits {
  /** Define a new exception class */
  private[bits] class BitStreamGenerationException(msg: String) extends Exception(msg)
  
  // Individual bit value enumeration
  private[bits] object BitValue extends Enumeration {
    type BitValue = Value
    val BitLow    = Value("0")
    val BitHigh   = Value("1")
    def apply(b: Boolean) = if (b) BitHigh else BitLow
  }
  import BitValue._

  /** Return a list of BitValues from an integer
   * @param v the integer to select bits from
   * @param numBits the number of bits to convert
   * @return a list of BitValues
   */
  private[bits] def int2BitValues(v: Int, numBits: Int): Seq[BitValue] = {
    require(numBits >= 0, "number of bits to convert must be non-negative")
    (0 until numBits).map(i => BitValue((v >> i) & 1))
  }

  /** Return an integer from a list of BitValues
   * @param arr the list to select bits from
   * @return an integer
   */
  private[bits] def bitValues2Int(arr: Seq[BitValue]): Int = {
    require(0 <= arr.length && arr.length <= 32,
      "number of bits to convert must fit an integer")
    arr.zipWithIndex
      .map { case (bit, pos) => (if (bit == BitHigh) 1 else 0) << pos }
      .sum
  }
}
