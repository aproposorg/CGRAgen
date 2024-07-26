package cgragen.cgra

import cgragen.archparse.PortType.{PortType, PortUnspecified}

/** Abstract port
 * @param name the name of the port
 * @param pt the type of the port
 * @param dataSize the size of the port
 * @param parent the module to which the port belongs
 */
private[cgragen] final class AbstractPort(val name: String, val pt: PortType = PortUnspecified,
  val dataSize: Int = 0, val parent: AbstractModule = null) {
  /** Various other methods start ********************************************/
  /** Return this port in a string */
  override def toString(): String = name
  /** Various other methods end **********************************************/
}
