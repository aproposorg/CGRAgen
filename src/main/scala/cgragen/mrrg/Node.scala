package cgragen.mrrg

import scala.collection.mutable

import cgragen.cgra.AbstractModule

import cgragen.dfgparse.Opcode.Opcode

import cgragen.mrrg.MRRGNodeType.{MRRGNodeType, NodeRouting}

/** MRRG node
 * @param name the name of the node
 * @param cycle the clock cycle to which the node belongs
 * @param parent the module which the node models part of
 * @param essential whether the node is essential or may be trimmed away
 * @param nodeType the type of the node
 */
final class MRRGNode(var name: String, val cycle: Int, val parent: AbstractModule,
  val nodeType: MRRGNodeType, val essential: Boolean) {
  /** Fields start ***********************************************************/
  // Supported operations by this node
  private val _modes = mutable.ArrayBuffer.empty[Opcode]

  // Fanout and fanin nodes of this node
  private val _fanin  = mutable.ArrayBuffer.empty[MRRGNode]
  private val _fanout = mutable.ArrayBuffer.empty[MRRGNode]
  /** Fields end *************************************************************/

  /** Accessors start ********************************************************/
  /** Return the fanin nodes of this node */
  def fanin = _fanin.toArray

  /** Return the fanout nodes of this node */
  def fanout = _fanout.toArray
  /** Accessors end **********************************************************/

  /** Modifiers start ********************************************************/
  /** Add a supported mode to this node
   * @param mode the mode to add
   */
  def addMode(mode: Opcode): Unit = _modes += mode

  /** Add a fanin node to this node
   * @param node the node to add
   */
  def addFanin(node: MRRGNode): Unit = _fanin += node

  /** Remove a fanin node from this node
   * @param node the node to remove
   */
  def removeFanin(node: MRRGNode): Unit = _fanin -= node

  /** Add a fanout node to this node
   * @param node the node to add
   */
  def addFanout(node: MRRGNode): Unit = _fanout += node

  /** Remove a fanout node from this node
   * @param node the node to remove
   */
  def removeFanout(node: MRRGNode): Unit = _fanout -= node
  /** Modifiers end **********************************************************/

  /** Various other methods start ********************************************/
  /** Check whether this node can map the given operation
   * @param opcode the operation to check for
   * @return true if this node can map the operation; false otherwise
   */
  def canMapOp(opcode: Opcode) = _modes.contains(opcode)

  /** Return this node in a string */
  override def toString() = s"${cycle}:${name}"

  /** Return the hash of this node */
  override def hashCode(): Int = toString().hashCode()
  /** Various other methods end **********************************************/
}

object MRRGNode {
  /** Create a new MRRGNode with the specified arguments
   * @param name the name of the node
   * @param cycle the cycle to which the node belongs
   * @param parent the module to which the node belongs
   * @param nodeType the type of the node (defaults to NodeRouting)
   * @param essential whether the node is essential (defaults to false)
   * @return a new instance of [[MRRGNode]]
   */
  def apply(name: String, cycle: Int, parent: AbstractModule,
    nodeType: MRRGNodeType = NodeRouting, essential: Boolean = false)
    (implicit conf: cgragen.Parameters) = {
    new MRRGNode(name, cycle, parent, nodeType, essential)
  }
}
