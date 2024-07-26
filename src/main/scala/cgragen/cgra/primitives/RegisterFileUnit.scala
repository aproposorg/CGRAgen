package cgragen.cgra.primitives

import cgragen.archparse.PortType.{PortInput, PortOutput}

import cgragen.cgra.{AbstractModule, ConfigCell}
import cgragen.cgra.ModuleType.ModPrimRf

import scala.collection.mutable

/** Abstract register file unit module
 * @param name the name of the module
 * @param numInputs the number of input ports to the register file
 * @param numOutputs the number of output ports from the register file
 * @param numRegsLg2 log2 of the number of registers
 * 
 * @note Inherits from [[AbstractModule]].
 */
private[cgragen] final class AbstractRegisterFileUnit(name: String, dataSize: Int,
  val numInputs : Int, val numOutputs: Int, val numRegsLg2: Int)
  (implicit conf: cgragen.Parameters) extends AbstractModule(name, dataSize, ModPrimRf) {
  require(numInputs  >= 0, "Register file must have a non-negative number of inputs")
  require(numOutputs >= 0, "Register file must have a non-negative number of outputs")
  require(numRegsLg2 >  0, "Register file must have a positive number of registers")
}

private[cgragen] object AbstractRegisterFileUnit {
  /** Create a new AbstractRegisterFileUnit with the specified arguments 
   * @param name the name of the module
   * @param numInputs the number of input ports
   * @param numOutputs the number of output ports
   * @param numRegsLg2 log2 of the number of registers (defaults to 
   *                   cgra.RegisterFile.Log2NRegister)
   * @param size the size of the module's internals (defaults to cgra.DataSize)
   * @return a new instance of [[AbstractRegisterFileUnit]]
   */
  def apply(name: String, numInputs: Int, numOutputs: Int, numRegsLg2: Int = -1, size: Int = -1)
    (implicit conf: cgragen.Parameters): AbstractRegisterFileUnit = {
    // Get default parameters if none are passed
    val rfSize       = if (size < 0)       conf.DataSize                      else size
    val rfNumRegsLg2 = if (numRegsLg2 < 0) conf.RFArgs("Log2NRegister").toInt else numRegsLg2

    val rf = new AbstractRegisterFileUnit(name, rfSize, numInputs, numOutputs, rfNumRegsLg2)

    // Add some ports
    (0 until numInputs).foreach { in =>
      rf.addPort(s"in$in", PortInput, rfSize)
      rf.addPort(s"addr_in$in", PortInput, rfNumRegsLg2)
      rf.addPort(s"WE$in", PortInput, 1)
    }
    (0 until numOutputs).foreach { out =>
      rf.addPort(s"out$out", PortOutput, rfSize)
      rf.addPort(s"addr_out$out", PortInput, rfNumRegsLg2)
    }

    // Add some config cells to control address and enable ports
    (0 until numInputs).foreach { in =>
      rf.addConfig(new ConfigCell(s"${name}_addr_in$in"), s"this.addr_in$in")
      rf.addConfig(new ConfigCell(s"${name}_WE$in"), s"this.WE$in")
    }
    (0 until numOutputs).foreach { out =>
      rf.addConfig(new ConfigCell(s"${name}_addr_out$out"), s"this.addr_out$out")
    }

    rf
  }
}
