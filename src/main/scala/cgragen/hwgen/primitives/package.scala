package cgragen.hwgen

import cgragen.cgra.AbstractModule
import cgragen.archparse.PortType._

package object primitives {
  /** Hardware primitive without configuration logic
   * @param mod the module to generate hardware from
   * 
   * @note Inheriting classes must define a `generate` method that generates 
   *       the underlying hardware of it. The method is automatically called 
   *       upon construction.
   */
  private[hwgen] abstract class Primitive(mod: AbstractModule)
    (implicit conf: cgragen.Parameters) extends SubModule(mod) {
    /** Dot not allow addition of sub-modules */
    override def addSubModule(name: String, subMod: Buildable): Unit = {
      println(s"[ERROR] Cannot add sub-module ($name) to primitive")
      throw new Exception(s"invalid addition of sub-module to primitive")
    }

    /** Build up the frame of the primitive */
    override def build(): Unit = {
      // Add all the necessary ports to the to module
      val configCellPorts = mod.configCells.map(_._2.port)
      mod.ports
        .filter(_._2.pt == PortInput)
        .foreach { case (name, port) =>
          if (configCellPorts.exists(_ == port)) addCtrlInput(name, port.dataSize)
          else addInput(name, port.dataSize) }
      mod.ports
        .filter(_._2.pt == PortOutput)
        .foreach { case (name, port) => addOutput(name, port.dataSize) }
    }
  }
}
