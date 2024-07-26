package cgragen

import cgragen.cgra.primitives._

import cgragen.dfgparse.Opcode, Opcode._

import scala.math.{ceil, log10}

package object cgra {
  /** Some new exception classes */
  private[cgra] class MalformedDataException(msg: String) extends Exception(msg)
  private[cgra] class MissingParameterException(msg: String) extends Exception(msg)
  private[cgra] class MissingPortException(msg: String) extends Exception(msg)
  private[cgra] class MissingModuleException(msg: String) extends Exception(msg)
  private[cgra] class DuplicateConnectionException(msg: String) extends Exception(msg)
  private[cgra] class DuplicateDefinitionException(msg: String) extends Exception(msg)
  private[cgra] class TopLevelModuleException(msg: String) extends Exception(msg)
  private[cgra] class TopLevelIOInferenceException(msg: String) extends Exception(msg)

  /** Return a ceiled log2
   * @param i the value to apply log2 to
   * @return ceil(log2(i))
   */
  private[cgragen] def log2Ceil(i: Int): Int = log2Ceil(i.toDouble).toInt.max(1)

  /** Return a ceiled log2
   * @param i the value to apply log2 to
   * @return ceil(log2(i))
   */
  private[cgragen] def log2Ceil(d: Double): Double = ceil(log10(d)/log10(2.0)).max(1.0)

  // Component type enumeration
  private[cgragen] object ModuleType extends Enumeration {
    type ModuleType = Value
    val ModComposite, ModPrimFunc, ModPrimReg, ModPrimRf, ModPrimMux, ModPrimIO = Value
  }

  /** Check whether an operation is hardware approximable
   * @param op the operation to check
   * @return true iff `op` is approximable in hardware
   */
  private[cgragen] def isHWApproximable(op: Opcode)
    (implicit conf: cgragen.Parameters): Boolean = {
    val apprxmblOps = tokenizeOpList(conf.CGRAApproximableOps)
    if (apprxmblOps.toSet.size != apprxmblOps.size) {
      println(s"[ERROR] Non-unique set of hardware approximable operations")
      throw new MalformedDataException("non-unique hardware approximable operations")
    }
    apprxmblOps.contains(op)
  }

  /** Return the total configuration bit width of a module template
   * @param mod the module template to extract bit-width from
   * @return the number of bits needed to configure `mod`
   */
  private[cgragen] def confWidth(mod: AbstractModule): Int = mod match {
    case _: AbstractInputUnit | _: AbstractOutputUnit | _: AbstractRegisterUnit => 0
    case _: AbstractConstantUnit    | _: AbstractFunctionUnit |
         _: AbstractMultiplexerUnit | _: AbstractRegisterFileUnit =>
      mod.configCells.values.map(_.port.dataSize).sum
    case _: AbstractModule =>
      mod.subModules.values.foldLeft(0) { case (acc, subMod) => acc + confWidth(subMod) }
  }

  /** Get the module and port names from a string
   * @param name the name string to look for
   * @return tuple of (Boolean, String, String) with information about the 
   *         module and port names if found
   */
  private[cgra] def getModulePort(name: String): (Boolean, String, String) = {
    val dotInd = name.indexOf('.')
    val module = if (dotInd != -1) name.take(dotInd) else ""
    val port   = name.drop(dotInd + 1)
    (port.indexOf('.') == -1, module, port)
  }

  /** Split a string of operation names into a sequence of Opcodes
   * @param str the string to split
   * @return a sequence of opcodes named by elements of `str`
   */
  private[cgra] def tokenizeOpList(str: String): Seq[Opcode] = {
    val ops = str.split(' ').map { opName => 
      try {
        Opcode.withName(opName)
      } catch {
        case e: NoSuchElementException => 
          println(s"[ERROR] Function unit has invalid opcode. Failed with error: ${e.getMessage()}")
          throw new MalformedDataException("invalid opcode")
      }
    }
    ops.toSeq
  }
}
