package cgragen.cgra.primitives

import cgragen.archparse.PortType.{PortInput, PortOutput}

import cgragen.cgra.{AbstractModule, ConfigCell}
import cgragen.cgra.ModuleType.ModPrimFunc

/** Abstract constant unit module
 * @param name the name of the module
 * 
 * @note Inherits from [[AbstractModule]].
 */
private[cgragen] final class AbstractConstantUnit(name: String, dataSize: Int)
  (implicit conf: cgragen.Parameters) extends AbstractModule(name, dataSize, ModPrimFunc)

private[cgragen] object AbstractConstantUnit {
  /** Create a new AbstractConstantUnit with the specified arguments
   * @param name the name of the module
   * @param size the size of the module's internals (defaults to cgra.DataSize)
   * @return a new instance of [[AbstractConstantUnit]]
   */
  def apply(name: String, size: Int = -1)(implicit conf: cgragen.Parameters): AbstractConstantUnit = {
    // Get default parameters if none are passed
    val cuSize = if (size < 0) conf.DataSize else size

    val cu = new AbstractConstantUnit(name, cuSize)

    // Add two ports
    cu.addPort("const", PortInput, cuSize)
    cu.addPort("out", PortOutput, cuSize)

    // Add a config cell to control constant
    cu.addConfig(new ConfigCell("const"), "this.const")

    cu
  }
}
