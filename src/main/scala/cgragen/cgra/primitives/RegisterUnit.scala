package cgragen.cgra.primitives

import cgragen.archparse.PortType.{PortInput, PortOutput}

import cgragen.cgra.AbstractModule
import cgragen.cgra.ModuleType.ModPrimReg

/** Abstract register unit module
 * @param name the name of the module
 * 
 * @note Inherits from [[AbstractModule]].
 */
private[cgragen] final class AbstractRegisterUnit(name: String, dataSize: Int)
  (implicit conf: cgragen.Parameters) extends AbstractModule(name, dataSize, ModPrimReg)

private[cgragen] object AbstractRegisterUnit {
  /** Create a new AbstractRegisterUnit with the specified arguments
   * @param name the name of the module
   * @param size the size of the module's internals (defaults to cgra.DataSize)
   * @return a new instance of [[AbstractRegisterUnit]]
   */
  def apply(name: String, size: Int = -1)(implicit conf: cgragen.Parameters): AbstractRegisterUnit = {
    // Get default parameters if none are passed
    val regSize = if (size < 0) conf.DataSize else size

    val reg = new AbstractRegisterUnit(name, regSize)
    
    // Add some ports
    reg.addPort("in", PortInput, regSize)
    reg.addPort("out", PortOutput, regSize)

    reg
  }
}
