package cgragen.cgra.primitives

import cgragen.archparse.PortType.{PortInput, PortOutput}

import cgragen.cgra.{log2Ceil, AbstractModule, ConfigCell}
import cgragen.cgra.ModuleType.ModPrimMux

import scala.collection.mutable

/** Abstract multiplexer unit module
 * @param name the name of the module
 * @param muxSize the number of elements the multiplexer should select between
 * 
 * @note Inherits from [[AbstractModule]]. The multiplexer per default has a registered output.
 */
private[cgragen] final class AbstractMultiplexerUnit(name: String, dataSize: Int, val muxSize: Int)
  (implicit conf: cgragen.Parameters) extends AbstractModule(name, dataSize, ModPrimMux) {
  require(muxSize >= 2, "Multiplexer must have at least two inputs")
}

private[cgragen] object AbstractMultiplexerUnit {
  /** Create a new AbstractMultiplexerUnit with the specified arguments
   * @param name the name of the module
   * @param muxSize the number of elements to select between
   * @param size the size of the module's internals (defaults to cgra.DataSize)
   * @return a new instance of [[AbstractMultiplexerUnit]]
   */
  def apply(name: String, muxSize: Int, size: Int = -1)
    (implicit conf: cgragen.Parameters): AbstractMultiplexerUnit = {
    // Get default parameters if none are passed
    val dataSize = if (size < 0) conf.DataSize else size

    val mux = new AbstractMultiplexerUnit(name, dataSize, muxSize)

    // Add some ports
    (0 until muxSize).foreach { i =>
      mux.addPort(s"in$i", PortInput, dataSize)
    }
    mux.addPort("select", PortInput, log2Ceil(muxSize))
    mux.addPort("out", PortOutput, dataSize)

    // Add a config cell to control the input selection
    mux.addConfig(new ConfigCell(s"${name}_config"), "this.select")

    mux
  }
}
