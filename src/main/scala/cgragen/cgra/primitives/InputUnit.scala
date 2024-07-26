package cgragen.cgra.primitives

import cgragen.archparse.PortType.PortOutput

import cgragen.cgra.AbstractModule
import cgragen.cgra.ModuleType.ModPrimIO

/** Abstract input port module
 * @param name the name of the module
 * 
 * @note Inherits from [[AbstractModule]].
 */
private[cgragen] final class AbstractInputUnit(name: String, dataSize: Int)
  (implicit conf: cgragen.Parameters) extends AbstractModule(name, dataSize, ModPrimIO)

private[cgragen] object AbstractInputUnit {
  /** Create a new AbstractInputUnit with the specified arguments
   * @param name the name of the module
   * @param size the size of the module's internals
   * @return a new instance of [[AbstractInputUnit]]
   */
  def apply(name: String, size: Int = -1)(implicit conf: cgragen.Parameters): AbstractInputUnit = {
    // Get default parameters if none are passed
    val inSize = if (size < 0) conf.DataSize else size

    val in = new AbstractInputUnit(name, inSize)

    // Add a port
    in.addPort("out", PortOutput, inSize)

    in
  }
}
