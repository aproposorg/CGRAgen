package cgragen.cgra

import scala.collection.mutable

/** Abstract connection between ports
 * @param source the port port of the connection
 * @param sext whether to sign-extend this connection on narrower sink ports
 */
private[cgra] final class AbstractConnection(val source: AbstractPort, val sext: Boolean) {
  /** Fields start ***********************************************************/
  // Keep track of sink ports of this connection
  private val _sinks = mutable.ArrayBuffer.empty[AbstractPort]
  /** Fields end *************************************************************/

  /** Accessors start ********************************************************/
  /** Return the sink ports of this connection */
  def sinks = _sinks.toArray
  /** Accessors end **********************************************************/

  /** Modifiers start ********************************************************/
  /** Add a sink port to this connection
   * @param port the port to add
   */
  def addSink(port: AbstractPort): Unit = _sinks += port
  /** Modifiers end **********************************************************/

  /** Various other methods start ********************************************/
  /** Return this connection in a string */
  override def toString(): String = _sinks.map { sink => 
    s"${source.parent.name}.${source.name} -> ${sink.parent.name}.${sink.name}" 
  }.mkString("\n")
  /** Various other methods end **********************************************/
}
