package cgragen.dfgparse

/** DFG edge
 * @param src the source node
 * @param snk the sink node
 * @param operand the operand index in the sink node's operation
 */
private[cgragen] final class DFGEdge(val src: DFGNode, val snk: DFGNode, val operand: Int) {
  /** Various other methods start ******************************************/
  /** Compare this edge to another edge */
  override def equals(obj: Any): Boolean = obj match {
    case that: DFGEdge =>
      this.src == that.src && this.snk == that.snk && this.operand == that.operand
    case _ => false
  }

  /** Return this node in a string */
  override def toString(): String = s"${src.name}->${snk.name}[operand=$operand]"

  /** Return the hash of this node */
  override def hashCode(): Int = toString().hashCode()
  /** Various other methods end ********************************************/
}
