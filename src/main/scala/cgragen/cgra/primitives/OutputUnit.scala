package cgragen.cgra.primitives

import cgragen.archparse.PortType.PortInput

import cgragen.cgra.AbstractModule
import cgragen.cgra.ModuleType.ModPrimIO

/** Abstract output port module
 * @param name the name of the module
 * 
 * @note Inherits from [[AbstractModule]].
 */
private[cgragen] final class AbstractOutputUnit(name: String, dataSize: Int)
  (implicit conf: cgragen.Parameters) extends AbstractModule(name, dataSize, ModPrimIO)

private[cgragen] object AbstractOutputUnit {
  /** Create a new AbstractOutputUnit with the specified arguments
   * @param name the name of the module
   * @param size the size of the module's internals
   * @return a new instance of [[AbstractOutputUnit]]
   */
  def apply(name: String, size: Int = -1)(implicit conf: cgragen.Parameters): AbstractOutputUnit = {
    // Get default parameters if none are passed
    val outSize = if (size < 0) conf.DataSize else size

    val out = new AbstractOutputUnit(name, outSize)

    // Add a port
    out.addPort("in", PortInput, outSize)

    out
  }
}
