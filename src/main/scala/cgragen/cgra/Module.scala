package cgragen.cgra

import cgragen.archparse.{Architecture, Connection, Template}
import cgragen.archparse.PortType._

import cgragen.cgra.ModuleType.{ModComposite, ModuleType}
import cgragen.cgra.primitives._

import scala.collection.mutable

/** Abstract module base class
 * @param name the name of the module
 * @param dataSize the data size of the module
 * @param modType the type of the module (defaults to ModComposite)
 * @param templateName the name of the template on which the module is based
 */
private[cgragen] abstract class AbstractModule(val name: String, val dataSize: Int,
  val modType: ModuleType = ModComposite, val templateName: String = "")
  (implicit conf: cgragen.Parameters) {
  /** Fields start ***********************************************************/
  // Pointer to the module's parent, if any
  private var parent: AbstractModule = null

  // Abstract representation of ports in this module formatted as
  // (name, port pointer)
  private val _ports = mutable.HashMap.empty[String, AbstractPort]

  // Abstract representation of connections in this module formatted as
  // (port pointer, connection pointer)
  private val _connections = mutable.HashMap.empty[AbstractPort, AbstractConnection]

  // Abstract representation of sub-modules in this module formatted as
  // (name, module pointer)
  private val _subModules = mutable.HashMap.empty[String, AbstractModule]

  // Abstract representation of config cells in this module formatted as
  // (name, cell pointer)
  private val _configCells = mutable.HashMap.empty[String, ConfigCell]
  /** Fields end *************************************************************/

  /** Accessors start ********************************************************/
  /** Return the ports of this module */
  def ports = _ports.toMap

  /** Return the connections of this module */
  def connections = _connections.toMap

  /** Return the sub-module instances of this module */
  def subModules = _subModules.toMap

  /** Return the config cells of this module */
  def configCells = _configCells.toMap
  /** Accessors end **********************************************************/

  /** Modifiers start ********************************************************/
  /** Add a configuration cell to the module
   * @param cell the configuration cell to add
   * @param snk the ports to connect it to
   */
  def addConfig(cell: ConfigCell, snk: String): Unit = {
    // Check if the config cell already exists
    if (_configCells.contains(cell.name)) {
      throw new DuplicateDefinitionException("config cell already exists")
    }

    // Otherwise, connect the cell to the specified port. First, get its 
    // module and port names
    val (exists, modName, portName) = getModulePort(snk)
      
    // If there was an error finding the module or port name, throw an error
    if (!exists) {
      println(s"[ERROR] Connection failed in ($name) when attempting split on port ($snk)")
      throw new MalformedDataException("module.port error")
    }

    // Otherwise, locate the module and port to connect
    val module = modName match {
      case "this" => this
      case _ =>
        // If the module does not exist, throw an error
        _subModules.getOrElse(modName, {
          println(s"[ERROR] Connection failed in ($name): sub-module ($modName) not found")
          throw new MissingModuleException(s"missing sub-module $modName")
        })
    }

    // Now it is safe to get the module and its related port. If it does not 
    // exist, throw an error
    val port = module.ports.getOrElse(portName, {
      println(s"[ERROR] Connection failed in ($name): port ($portName) not found")
      throw new MissingPortException(s"missing port $portName")
    })

    // Set the config cell's port
    assert(cell.port == null, "cannot assign multiple ports to a config cell")
    cell.port = port

    // Update the config cell size to fit the connected port
    if (port.dataSize > cell.dataSize) cell.dataSize = port.dataSize

    // Add the config cell to this module
    _configCells(cell.name) = cell
  }

  /** Add a connection to the module
   * @param src the source port of the connection
   * @param snk the sink port of the connection
   * @param sext whether to sign-extend this connection on narrower sink ports
   */
  def addConnection(src: String, snk: String, sext: Boolean = conf.Sext): Unit = {
    // Various error messages
    val connErrMsg = s"[ERROR] Connection could not be made between ($src) and ($snk) within ($name)"

    // Get the ports' module and port names
    val (srcExists, srcModName, srcPortName) = getModulePort(src)
    val (snkExists, snkModName, snkPortName) = getModulePort(snk)

    // Check if there was an error finding either module or port names
    if (!srcExists || !snkExists) {
      println(connErrMsg)
      println(s"[ERROR] Connection failed in ($name) when attempting split on port ($src) or ($snk)")
      throw new MalformedDataException("module.port error")
    }

    // Find the corresponding modules if any
    val srcModule = srcModName match {
      case "this" => this
      case _ =>
        // If the source module does not exist, throw an error
        _subModules.getOrElse(srcModName, {
          println(connErrMsg)
          println(s"[ERROR] Connection failed in ($name): source module ($srcModName) not found")
          throw new MissingModuleException(s"missing sub-module $srcModName")
        })
    }
    val snkModule = snkModName match {
      case "this" => this
      case _ =>
        // If the sink module does not exist, throw an error
        _subModules.getOrElse(snkModName, {
          println(connErrMsg)
          println(s"[ERROR] Connection failed in ($name): sink module ($snkModName) not found")
          throw new MissingModuleException(s"missing sub-module $snkModName")
        })
    }

    // Find the corresponding ports if any
    val srcPort = srcModule.ports.getOrElse(srcPortName, {
      // If the source port does not exist, throw an error
      println(s"[ERROR] Connection failed in ($name): source port ($srcPortName) not found")
      throw new MissingPortException(s"missing port $srcPortName")
    })
    val snkPort = snkModule.ports.getOrElse(snkPortName, {
      // If the sink port does not exist, throw an error
      println(s"[ERROR] Connection failed in ($name): sink port ($snkPortName) not found")
      throw new MissingPortException(s"missing port $snkPortName")
    })

    // Check for uniqueness: sink port cannot have existing connections
    _connections.foreach { case (_, conn) => 
      conn.sinks.foreach { sink =>
        if (snkPort == sink) {
          // If a matching port is found, throw an error
          println(connErrMsg)
          print(s"[ERROR] Connection failed in ($name): sink ($snk) is already connected to ")
          println(s"(${conn.source.parent.name}.${conn.source.name})")
          throw new DuplicateConnectionException(s"failed on sink port $snk")
        }
      }
    }

    // Write debug messages on mismatched data sizes
    if (conf.CGRADebug) {
      val infoMsg = s"[DEBUG] Connection from ($src) to ($snk) in module ($name) has mismatched data sizes"
      if (srcPort.dataSize < snkPort.dataSize) {
        println(s"$infoMsg and will be ${if (sext) "sign" else "zero"}-extended")
      } else if (srcPort.dataSize > snkPort.dataSize) {
        println(s"$infoMsg and will be truncated")
      }
    }

    // Finally, add this connection if it does not already exist in this module
    if (!_connections.contains(srcPort)) {
      // Add an entry in the connections map
      _connections(srcPort) = new AbstractConnection(srcPort, sext)
    } else {
      // Otherwise, check that the destination is unique
      _connections(srcPort).sinks.foreach { sink =>
        if (sink == snkPort) {
          throw new DuplicateConnectionException(s"failed on destination $sink")
        }
      }
    }

    // Add the connection's sink port
    if (conf.CGRADebug) println(s"[DEBUG] Added connection from ($src) to ($snk) in module ($name)")
    _connections(srcPort).addSink(snkPort)
  }

  /** Add a port to the module
   * @param portName the name of the port
   * @param pt the port type
   * @param size the data size of the port (defaults to cgra.DataSize)
   */
  def addPort(portName: String, pt: PortType, size: Int = conf.DataSize): Unit = {
    // If the port already exists, throw an error
    if (_ports.contains(portName)) {
      println(s"[ERROR] Port Insertion failed in ($name): port ($portName) already exists")
      throw new DuplicateDefinitionException(s"duplicate port $portName")
    }

    // Otherwise, create a new port and add it
    if (conf.CGRADebug) println(s"[DEBUG] Added port ($portName) of size ($size) to module ($name)")
    _ports(portName) = new AbstractPort(portName, pt, size, this)
  }

  /** Add a sub-module to the module
   * @param m the module to add
   */
  def addSubModule[T <: AbstractModule](m: T): Unit = {
    // If the sub-module already exists, throw an error
    if (_subModules.contains(m.name)) {
      println(s"[ERROR] Sub-module (${m.name}) already exists within ($name)")
      throw new DuplicateDefinitionException(s"duplicate sub-module ${m.name}")
    }

    // Otherwise, add it to this module
    if (conf.CGRADebug) println(s"[DEBUG] Added sub-module (${m.name}) to module ($name)")
    m.parent = this
    _subModules(m.name) = m
  }
  /** Modifiers end **********************************************************/

  /** Various other methods start ********************************************/
  /** Return the full path to this module */
  def fullPath: String = if (parent == null || parent.name == "CGRA") name else s"${parent.fullPath}.$name"

  /** Return whether this module has config cells */
  def hasConfigCells: Boolean = _configCells.nonEmpty

  /** Recursively fetch all of this module's and its submodules' config cells */
  def configOrder: Seq[ConfigCell] = {
    _configCells.values.toSeq ++ _subModules.values.flatMap(_.configOrder)
  }

  /** Return this module in a string */
  override def toString(): String = {
    /** Return a module in an indented string
     * @param mod the module to return
     * @param ind number of double spaces to indent content (defaults to 0)
     * @return a string representation of this module
     */
    def _toIndentedString(mod: AbstractModule, ind: Int = 0): String = {
      require(ind >= 0, "number of double spaces to indent by must be non-negative")

      // Create indentation and string builder
      val indent = "  " * ind
      val bs     = new StringBuilder(s"${mod.name}:")

      // Add all ports to the string
      if (!mod.ports.isEmpty) {
        bs ++= s"\n${indent}  Ports:\n"
        bs ++= mod.ports.keys.map { portName => 
          s"${indent}    $portName"
        }.toSeq.sorted.mkString("\n")
      }

      // Add all sub-modules to the string
      if (!mod.subModules.isEmpty) {
        bs ++= s"\n${indent}  Sub-modules:\n"
        bs ++= mod.subModules.values.map { m => 
          s"${indent}    ${_toIndentedString(m, ind+2)}"
        }.toSeq.sorted.mkString("\n")
      }

      // Add all connections to the string
      if (!mod.connections.isEmpty) {
        bs ++= s"\n${indent}  Connections:\n"
        bs ++= mod.connections.values.map { c => 
          s"${indent}    ${c.toString.replace(s"$name.", "").split('\n').mkString(s"\n${indent}    ")}"
        }.toSeq.sorted.mkString("\n")
      }

      bs.mkString
    }

    _toIndentedString(this)
  }

  /** Return the hash of this module */
  override def hashCode(): Int = name.hashCode()
  /** Various other methods end **********************************************/
}

/** Abstract basic module without any predetermined functionality
 * @param name the name of the module
 * @param dataSize the size of the module's internals
 * 
 * @note Inherits from [[AbstractModule]]
 */
private[cgragen] sealed class AbstractBaseModule(name: String, dataSize: Int, templateName: String)
  (implicit conf: cgragen.Parameters) extends AbstractModule(name, dataSize, templateName=templateName)

private[cgragen] object AbstractModule {
  /** Create a new AbstractModule with the specified arguments
   * @param name the name of this module
   * @param temp the module template to base this module on
   * @param arch the Architecture instance from which to recursively draw module templates
   * @return a new instance of [[AbstractModule]]
   * 
   * @note This function's recursive nature means it needs the Architecture argument.
   */
  def apply(name: String, temp: Template, arch: Architecture)
    (implicit conf: cgragen.Parameters): AbstractModule = {
    if (conf.CGRADebug) println(s"[DEBUG] Creating module ($name) from template (${temp.name})")

    // Create an empty abstract module with the specified name
    val mod = new AbstractBaseModule(name, temp.dataSize, temp.name)

    // Add the ports to the module
    if (!temp.ports.isEmpty) {
      if (conf.CGRADebug) println(s"[DEBUG] Adding ports to module ($name)")
      temp.ports.foreach { case (portName, (portType, portSize)) =>
        mod.addPort(portName, portType, portSize)
      }
    }

    // Add the primitives to the module
    if (!temp.primitives.isEmpty) {
      if (conf.CGRADebug) println(s"[DEBUG] Adding primitives to module ($name)")
      temp.primitives.foreach { case (primType, primArgs) =>
        val primName = primArgs("name")
        val primSize = primArgs.getOrElse("size", temp.dataSize.toString).toInt
        if (conf.CGRADebug) println(s"[DEBUG] Adding primitive ($primName) of type ($primType)")
        primType match {
          case "ConstUnit" =>
            mod.addSubModule(AbstractConstantUnit(primName, primSize))

          case "FuncUnit" =>
            // Function units may have lists of operations and their IIs and latencies
            val ops = tokenizeOpList(primArgs.getOrElse("ops", conf.FUArgs("Ops")))
            val iis = if (primArgs.contains("IIs")) {
              val iisOpt = primArgs("IIs").split(' ').map(_.toIntOption)
              if (iisOpt.exists(_ == None) || iisOpt.length != ops.length) {
                println(s"[ERROR] Function unit ($name) has malformed IIs")
                throw new MalformedDataException("function unit IIs")
              }
              iisOpt.map(_.get).toSeq
            } else {
              Seq.fill(ops.length){ conf.FUArgs("II").toInt }
            }
            val lats = if (primArgs.contains("latencies")) {
              val latenciesOpt = primArgs("latencies").split(' ').map(_.toIntOption)
              if (latenciesOpt.exists(_ == None) || latenciesOpt.length != ops.length) {
                println(s"[ERROR] Function unit ($name) has malformed latencies")
                throw new MalformedDataException("function unit latencies")
              }
              latenciesOpt.map(_.get).toSeq
            } else {
              Seq.fill(ops.length){ conf.FUArgs("Latency").toInt }
            }
            val approx = {
              val locApprox = if (!primArgs.contains("approx")) false else {
                primArgs("approx").toIntOption match {
                  case Some(x) => x != 0
                  case _ =>
                    println(s"[ERROR] Function unit ($name) has malformed approximation flag")
                    throw new MalformedDataException("function unit approximation flag")
                }
              }
              conf.ApproximateArithmetic || locApprox
            }
            mod.addSubModule(AbstractFunctionUnit(primName, ops, iis, lats, approx, primSize))

          case "Multiplexer" =>
            // Multiplexers must have a specified number of inputs
            if (!primArgs.contains("ninput") || primArgs("ninput").toIntOption == None) {
              println(s"[ERROR] Multiplexer ($name) is missing its number of inputs")
              throw new MissingParameterException("number of multiplexer inputs")
            }
            val muxSize = primArgs("ninput").toInt
            mod.addSubModule(AbstractMultiplexerUnit(primName, muxSize, primSize))

          case "Register" =>
            mod.addSubModule(AbstractRegisterUnit(primName, primSize))

          case "RegisterFile" =>
            // Register files must have specified numbers of inputs and outputs
            if (!primArgs.contains("ninput") || primArgs("ninput").toIntOption == None) {
              println(s"[ERROR] Register file ($name) is missing its number of inputs")
              throw new MissingParameterException("number of register file inputs")
            }
            if (!primArgs.contains("noutput") || primArgs("noutput").toIntOption == None) {
              println(s"[ERROR] Register file ($name) is missing its number of outputs")
              throw new MissingParameterException("number of register file outputs")
            }
            val numInputs  = primArgs("ninput").toInt
            val numOutputs = primArgs("noutput").toInt
            val numRegsLg2 = primArgs.getOrElse("log2-nregister", conf.RFArgs("Log2NRegister")).toInt
            mod.addSubModule(AbstractRegisterFileUnit(primName, numInputs, numOutputs, numRegsLg2, primSize))

          case "InputUnit" =>
            mod.addSubModule(AbstractInputUnit(primName, primSize))

          case "OutputUnit" =>
            mod.addSubModule(AbstractOutputUnit(primName, primSize))

          case _ => 
            println(s"[ERROR] Got unsupported primitive type ($primType)")
            throw new MalformedDataException("unsupported primitive")
        }
      }
    }

    // Recursively add any sub-modules to the module
    if (temp.subModules.nonEmpty) {
      if (conf.CGRADebug) println(s"[DEBUG] Adding sub-modules to module ($name)")
      temp.subModules.foreach { case (modName, modType) =>
        // Beware that modType is a string that should be matched against available 
        // template names to identify the sub-module to instantiate
        if (!arch.modTemplates.contains(modType)) {
          println(s"[ERROR] Cannot instantiate sub-module ($modName) of unknown type ($modType)")
          throw new MissingModuleException("invalid sub-module instance")
        }
        mod.addSubModule(AbstractModule(modName, arch.modTemplates(modType), arch))
      }
    }

    // Add connections to the module
    if (temp.connections.nonEmpty) {
      if (conf.CGRADebug) println(s"[DEBUG] Adding connections to module ($name)")
      temp.connections.foreach { case Connection(toType, toArgs, fromType, fromArgs, sext) =>
        // Check that the connection is parsed to its simplest form
        if (fromType != "from" || toType != "to") {
          print("[ERROR] Internal module representation only supports connections ")
          println("with from-type = \"from\" and to-type = \"to\"")
          throw new MalformedDataException("unsupported connection types")
        }
        mod.addConnection(fromArgs, toArgs, sext)
      }
    }

    // Recursively ensure that all input and output units are uniquely named
    if (mod.subModules.nonEmpty) {
      // Fetch all input units
      def getInputUnits(module: AbstractModule): Seq[AbstractInputUnit] = {
        val curr = module.subModules.values
          .collect { case in: AbstractInputUnit => in }
          .toSeq
        val rec = module.subModules.values
          .collect { case subMod if subMod.modType == ModComposite => subMod }
          .flatMap(getInputUnits)
        curr ++ rec
      }
      val inputUnitNames = getInputUnits(mod).map(_.name)
      if (inputUnitNames.size != inputUnitNames.toSet.size) {
        print("[ERROR] Got input units with non-unique names that will fail ")
        println("fail hardware generation")
        throw new DuplicateDefinitionException("non-unique input unit name")
      }

      // Fetch all output units
      def getOutputUnits(module: AbstractModule): Seq[AbstractOutputUnit] = {
        val curr = module.subModules.values
          .collect { case in: AbstractOutputUnit => in }
          .toSeq
        val rec = module.subModules.values
          .collect { case subMod if subMod.modType == ModComposite => subMod }
          .flatMap(getOutputUnits)
        curr ++ rec
      }
      val outputUnitNames = getOutputUnits(mod).map(_.name)
      if (outputUnitNames.size != outputUnitNames.toSet.size) {
        print("[ERROR] Got output units with non-unique names that will fail ")
        println("hardware generation")
        throw new DuplicateDefinitionException("non-unique output unit name")
      }

      // Fetch all port names recursively
      def getPorts(module: AbstractModule): Seq[AbstractPort] = module match {
        case _: AbstractInputUnit | _: AbstractOutputUnit => Seq.empty[AbstractPort]
        case _ =>
          val curr = module.ports.values.toSeq
          val rec  = module.subModules.values
            .flatMap(getPorts)
          curr ++ rec
      }
      val portNames = getPorts(mod).map(_.name).toSet
      Seq(("input", inputUnitNames), ("output", outputUnitNames)).foreach {
        case (dir, names) =>
        names.filter(name => portNames.contains(name)).foreach { name =>
          print(s"[ERROR] Got $dir unit ($name) whose name overlaps with a ")
          println("sub-module port and may fail hardware generation")
          throw new DuplicateDefinitionException(s"overlapping $dir unit name")
        }
      }
    }

    // Finalize and return the constructed module
    if (conf.CGRADebug) println(s"[DEBUG] Finished creating module ($name)")
    mod
  }
}
