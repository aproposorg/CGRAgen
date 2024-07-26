package cgragen.cgra

import cgragen.dfgparse.Opcode._

import scala.collection.mutable

/** Parameterizable operation
 * @param dataSize the size of the operation
 * @param op the opcode of the operation
 * @param ii the initiation interval of the operation
 * @param lat the latency of the operation
 * 
 * @note Approximable operations are marked by their  `isApproximable` field 
 *       being true. Their error characteristics in their supported modes 
 *       are computed on a per-need basis before mapping, not in their 
 *       intialization here. The characteristics map is, thus, empty till 
 *       this characterization is performed.
 */
private[cgragen] final class AbstractOperation(val dataSize: Int, val op: Opcode,
  val ii: Int, val lat: Int)(implicit conf: cgragen.Parameters) {
  /** Fields start *********************************************************/
  // The error characteristics in each approximation mode
  private val _chars = mutable.HashMap.empty[Int, (Double, Double)]
  /** Fields end ***********************************************************/

  /** Accessors start ******************************************************/
  /** Return the error characteristic of this operation */
  def chars = _chars.toMap
  /** Accessors end ********************************************************/

  /** Modifiers start ******************************************************/
  /** Add a pair of error characteristics for a mode to this operation
   * @param mInd the mode to set
   * @param mred the mean relative error distance
   * @param sded the error distance standard deviation
   */
  def addChar(mInd: Int, mred: Double, sdred: Double): Unit = {
    require(0 <= mInd && mInd <= conf.CGRAApproximationModes,
      "cannot set mode index higher than the maximum")
    _chars(mInd) = (mred, sdred)
  }

  /** Remove a pair of error characteristics for a mode from this operation
   * @param mInd the mode to remove
   */
  def removeChar(mInd: Int): Unit = _chars.remove(mInd)
  /** Modifiers end ********************************************************/
}

private[cgragen] object AbstractOperation {
  /** Check whether an operation is approximable
   * @param op the operation to check
   * @return true iff `op` is approximable given the current configuration
   */
  def isApproximable(op: Opcode)
    (implicit conf: cgragen.Parameters): Boolean = {
    val apprxmblOps = tokenizeOpList(conf.CGRAApproximableOps)
    apprxmblOps.contains(op)
  }
}
