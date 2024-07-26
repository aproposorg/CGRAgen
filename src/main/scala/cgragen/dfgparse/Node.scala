package cgragen.dfgparse

import cgragen.dfgparse.Opcode.Opcode

import scala.collection.mutable

/** DFG node
 * @param name the name of the node
 * @param opcode the operation of the node
 */
private[cgragen] final class DFGNode(val name: String, val opcode: Opcode) {
  /** Fields start *********************************************************/
  // Map of named attributes of this node
  private val _attrs = mutable.HashMap.empty[String, String]
  /** Fields end ***********************************************************/

  /** Accessors start ******************************************************/
  /** Return the attributes of this node */
  def attrs: Map[String, String] = _attrs.toMap
  /** Accessors end ********************************************************/

  /** Modifiers start ******************************************************/
  /** Add an attribute to this node
   * @param key the name of the attribute
   * @param value the value of the attribute
   */
  def addAttr(key: String, value: String): Unit = _attrs(key) = value

  /** Remove an attribute from this node
   * @param key the name of the attribute
   */
  def removeAttr(key: String): Unit = _attrs.remove(key)
  /** Modifiers end ********************************************************/

  /** Various other methods start ******************************************/
  /** Compare this node to another node */
  override def equals(obj: Any): Boolean = obj match {
    case that: DFGNode =>
    this.name == that.name && this.opcode == that.opcode
    case _ => false
  }

  /** Return this node in a string */
  override def toString(): String = {
    val attrStr = _attrs
      .map { case (k, v) =>
        val vStr = v.toString()
        if (vStr.isEmpty()) s"$k=\"\"" else s"$k=$vStr" }
      .mkString("[", " ", "]")
    s"${name}${attrStr}"
  }

  /** Return the hash of this node */
  override def hashCode(): Int = name.hashCode()
  /** Various other methods end ********************************************/
}
