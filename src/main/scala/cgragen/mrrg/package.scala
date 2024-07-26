package cgragen

import scala.collection.mutable

package object mrrg {
  /** A new exception class */
  private[mrrg] class MRRGGenerationException(msg: String) extends Exception(msg)

  // MRRG node type enumeration
  object MRRGNodeType extends Enumeration {
    type MRRGNodeType = Value
    val NodeRouting   = Value("routing")
    val NodeRegister  = Value("register")
    val NodeFunction  = Value("function")
  }

  /** Link two MRRG nodes
   * @param a the first node (fanout node)
   * @param b the second node (fanin node)
   */
  private[mrrg] def link(a: MRRGNode, b: MRRGNode): Unit = {
    a.addFanout(b)
    b.addFanin(a)
  }

  /** Unlink two MRRG nodes
   * @param a the first node (fanout node)
   * @param b the second node (fanin node)
   */
  private[mrrg] def unlink(a: MRRGNode, b: MRRGNode): Unit = {
    a.removeFanout(b)
    b.removeFanin(a)
  }
}
