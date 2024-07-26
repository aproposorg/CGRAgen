package cgragen.archparse

import cgragen.archparse.PortType._

import scala.collection.mutable

import scala.xml.Node

/** Abstract module template
 * @param name the name of the template
 * @param dataSize the default data size of the template
 * 
 * @note Used internally for parsing.
 */
private[cgragen] final class Template(val name: String, val dataSize: Int) {
  /** Fields start ***********************************************************/
  // Abstract representation of connections in this template 
  // with to-connection types being one of "to" or "distribute-to" 
  // and from-connection types being one of "from" or "select-from".
  private val _connections = mutable.ArrayBuffer.empty[Connection]

  // Abstract representation of ports in this template
  private val _ports = mutable.HashMap.empty[String, (PortType, Int)]

  // Abstract representation of primitives in this template
  private val _primitives = mutable.ArrayBuffer.empty[(String, Map[String, String])]

  // Abstract representation of sub-modules in this template
  private val _subModules = mutable.HashMap.empty[String, String]

  // Abstract representation of wires in this template
  private val _wires = mutable.HashMap.empty[String, String]
  /** Fields end *************************************************************/

  /** Accessors start ********************************************************/
  /** Return the connections of this template */
  def connections = _connections.toArray

  /** Return the ports of this template */
  def ports = _ports.toMap

  /** Return the primitive instances of this template */
  def primitives = _primitives.toArray

  /** Return the sub-mdoule instances of this template */
  def subModules = _subModules.toMap

  /** Return the wires of this template */
  def wires = _wires.toMap
  /** Accessors end **********************************************************/

  /** Modifiers start ********************************************************/
  /** Add a port to the template
   * @param portName the name of the port
   * @param pt the type of the port
   * @param size the size of the port
   */
  def addPort(portName: String, pt: PortType, size: Int): Unit = {
    require(portName != "", "ports must have a non-empty name")
    require(size >= 0, "ports must have a non-negative size")
    if (_ports.contains(portName)) {
      // If a port with the specified name already exists, throw an error
      println(s"[ERROR] Duplicate port ($portName) found in template ($name)")
      throw new DuplicateDefinitionException("port already exists")
    }
    // Otherwise, simply add the port
    _ports(portName) = (pt, size)
  }

  /** Add a wire to the template
   * @param wireName the name of the wire
   * @param wireArgs the associated ports (defaults to "")
   */
  def addWire(wireName: String, wireArgs: String = ""): Unit = {
    require(wireName != "", "wires must have a non-empty name")
    if (_wires.contains(wireName)) {
      // If a wire with the specified name already exists, throw an error
      println(s"[ERROR] Duplicate wire ($wireName) found in template ($name)")
      throw new DuplicateDefinitionException("wire already exists")
    }
    // Otherwise, simply add the wire
    _wires(wireName) = wireArgs
  }

  /** Remove a wire from the template
   * @param wireName the name of the wire
   */
  def removeWire(wireName: String): Unit = {
    require(wireName != "", "wires must have a non-empty name")
    require(_wires.contains(wireName), "wire must exist to be deleted")
    // Simply remove the wire
    _wires.remove(wireName)
  }

  /** Add associated ports to an existing wire
   * @param wireName the name of the wire
   * @param wireArgs the associated ports to add
   * @note Should only be called knowing a wire with the specified name 
   *       already exists.
   */
  def updateWireArgs(wireName: String, wireArgs: String): Unit = {
    require(wireName != "", "wires must have a non-empty name")
    require(_wires.contains(wireName), s"cannot update non-existent wire ($wireName)")
    if (_wires(wireName) != "") {
      println(s"[ERROR] Cannot override existing wire arguments in wire ($wireName)")
      throw new DuplicateDataException("multiple wire connections")
    }

    // Simply overwrite the map entry for this wire
    _wires(wireName) = wireArgs
  }

  /** Add a primitive instance to the template
   * @param primType the type of primitive to add
   * @param attrs map of attributes assigned to the primitive
   * @note All insertions are assumed to be successfull.
   */
  def addPrimitive(primType: String, attrs: Map[String, String]): Unit = {
    _primitives += ((primType, attrs))
  }

  /** Add a sub-module to the template
   * @param name the name of the sub-module
   * @param modType the type of the sub-module
   * @note All insertions are assumed to be successfull.
   */
  def addSubModule(name: String, modType: String): Unit = {
    require(name != "", "sub-modules must have a non-empty name")
    _subModules += (name -> modType)
  }

  /** Add a connection to the template
   * @param toType the type of the to-connection
   * @param toArgs the ports of the to-connection
   * @param fromType the type of the from-connection
   * @param fromArgs the ports of the from-connection
   * @param sext whether to sign-extend this connection on narrower sink ports
   */
  def addConnection(toType: String, toArgs: String, fromType: String, fromArgs: String, sext: Boolean): Unit = {
    require(toType != "", "to-connection type must be non-empty")
    require(toArgs != "", "to-connection port arguments must be non-empty")
    require(fromType != "", "from-connection type must be non-empty")
    require(fromArgs != "", "from-connection port arguments must be non-empty")
    _connections += Connection(toType, toArgs, fromType, fromArgs, sext)
  }

  /** Update the connection at a specific index of the array
   * @param ind the index of the connection to update
   * @param conn the new connection
   * @note Should only be called knowing a connection already exists.
   */
  def updateConnection(ind: Int, conn: Connection): Unit = {
    require(0 <= ind && ind < _connections.length, "index must be within bounds")
    _connections(ind) = conn
  }
  /** Modifiers end **********************************************************/

  /** Various other methods start ********************************************/
  /** Return this template in an indented string 
   * @param ind number of double spaces to indent content
   * @return a string representation of this template
   */
  def toString(ind: Int): String = {
    require(ind >= 0, "number of double spaces to indent by must be non-negative")
    
    // Create indentation and string builder
    val indent = "  " * ind
    val bs     = new StringBuilder(s"${indent}Template ($name) has the following contents:\n")

    // Add all ports to the string
    if (!_ports.isEmpty) {
      bs ++= s"${indent}  Ports:\n"
      bs ++= _ports.keys.toSeq.sorted.map { portName =>
        s"${indent}    Port ($portName) of type (${_ports(portName)._1}) with size (${_ports(portName)._2})"
      }.mkString("\n")
    }
    
    // Add all primitives to the string
    if (!_primitives.isEmpty) {
      bs ++= s"\n${indent}  Primitives:\n"
      bs ++= _primitives.map { case (primType, attrs) => 
        s"${indent}    Primitive of type ($primType) with attributes:\n${
          attrs.map { case (name, value) => s"${indent}      $name -> $value" }.mkString("\n")
        }"
      }.mkString("\n")
    }

    // Add all connections to the string
    if (!_connections.isEmpty) {
      bs ++= s"\n${indent}  Connections:\n"
      bs ++= _connections.map { case Connection(toType, toArgs, fromType, fromArgs, sext) => 
        s"${indent}    Connection from ($fromArgs) to ($toArgs) with ${if (sext) "sign" else "zero"}-extension"
      }.mkString("\n")
    }

    // Add all wires to the string
    if (!_wires.isEmpty) {
      bs ++= s"\n${indent}  Wires:\n"
      bs ++= _wires.map { case (name, port) => 
        s"${indent}    Wire ($name) related to port ($port)"
      }.mkString("\n")
    }

    bs.mkString
  }

  /** Return this template in a string */
  override def toString(): String = toString(0)

  /** Return the hash of this template */
  override def hashCode(): Int = name.hashCode()
  /** Various other methods end **********************************************/
}

private[cgragen] object Template {
  /** Parse an XML description of a module template
   * @param mod the XML description of the module template
   * @param defs a map of definitions provided in the architecture
   * @param parsed a map of named previously-parsed module templates
   * @return a new instance of [[cgragen.archparse.Template]] resulting from 
   *         the parsing
   */
  def apply(
    mod   : Node, 
    defs  : Map[String, Int],
    parsed: Map[String, Template] = Map.empty[String, Template]
  )(
    implicit conf: cgragen.Parameters
  ): Template = {
    // First get the name of the module and check its validity in Verilog
    val modName = mod \@ "name"
    if (!isValidName(modName)) {
      println(s"[ERROR] Module template ($modName) has invalid name")
      throw new InvalidIdentifierException("invalid module template name")
    }

    // Next, check if the module has a passed data size
    val modSize = if ((mod \ "@size").isEmpty) conf.DataSize else {
      val errMsg = s"[ERROR] Module template ($modName) has invalid size"
      val arg = (mod \@ "size")
      arg.toIntOption match {
        case Some(size) if size > 0 => size
        case Some(size) =>
          println(errMsg)
          throw new NegativeDataSizeException("invalid template size")
        case _ =>
          if (!defs.contains(arg)) {
            println(errMsg)
            throw new MissingDataException("invalid template size")
          } else if (defs(arg) <= 0) {
            println(errMsg)
            throw new NegativeDataSizeException("invalid template size")
          } else defs(arg)
      }
    }

    // Parse the template's contents
    if (conf.CGRADebug) println(s"[DEBUG] Found module template ($modName) - parsing its contents")
    val tmp = new Template(modName, modSize)

    // Scan the module for ports and add them to the template
    if (conf.CGRADebug) println("[DEBUG] Scanning for ports in module template")
    _addPorts(modName, mod, defs, tmp)

    // Next, scan the module for wires and add them to the template
    if (conf.CGRADebug) println("[DEBUG] Scanning for wires in module template")
    _addWires(modName, mod, tmp)

    // Next, scan the module for primitives and add them to the template
    if (conf.CGRADebug) println("[DEBUG] Scanning for primitive instances in module template")
    _addPrimitives(modName, mod, defs, tmp)

    // Next, scan the module for sub-modules and add them to the template
    if (conf.CGRADebug) println("[DEBUG] Scanning for sub-module instances in module template")
    _addSubModules(modName, mod, tmp)

    // Next, the difficult part of adding connections
    if (conf.CGRADebug) println("[DEBUG] Scanning for connections in module template")
    _addConnections(modName, mod, tmp, parsed)

    // Next, replace all wires with corresponding port names
    if (conf.CGRADebug) println("[DEBUG] Replacing wired connections with equivalent port connections")
    _connectWires(modName, tmp)

    // Next, verify that no sub-module or primitive has unconnected input ports
    val allToConnPorts   = tmp.connections.map(_.toPorts).toSet
    val allFromConnPorts = tmp.connections.map(_.fromPorts).toSet
    tmp.subModules.foreach { case (subModName, template) =>
      // Check input ports
      parsed(template).ports
        .filter { case (portName, (portType, _)) => portType == PortInput }.keys
        .filter ( portName => !allToConnPorts.contains(s"$subModName.$portName") )
        .foreach { portName =>
        print(s"[ERROR] Module template ($modName) has sub-module ($subModName) ")
        println(s"with unconnected input port ($portName)")
        throw new MissingDataException("unconnected input port")
      }
      // Check output ports
      if (conf.CGRADebug) {
        parsed(template).ports
          .filter { case (portName, (portType, _)) => portType == PortOutput }.keys
          .filter ( portName => !allFromConnPorts.contains(s"$subModName.$portName") )
          .foreach { portName =>
          print(s"[DEBUG] Module template ($modName) has sub-module ($subModName) ")
          println(s"with unconnected output port ($portName)")
        }
      }
    }
    tmp.primitives.foreach { case (primType, args) =>
      val (inPorts, outPorts) = primType match {
        case "ConstUnit" => (Seq.empty[String], Seq("out"))
        case "FuncUnit"  => (Seq("in_a", "in_b"), Seq("out"))
        case "Multiplexer" =>
          ((0 until args("ninput").toInt).map(i => s"in$i").toSeq, Seq("out"))
        case "Register" => (Seq("in"), Seq("out"))
        case "RegisterFile" =>
          ((0 until args("ninput").toInt).map(i => s"in$i").toSeq, 
           (0 until args("noutput").toInt).map(i => s"out$i").toSeq)
        case "InputUnit"  => (Seq.empty[String], Seq("out"))
        case "OutputUnit" => (Seq("in"), Seq.empty[String])
        case _ =>
          // Should never occur
          println(s"[ERROR] Got unsupported primitive type ($primType)")
          throw new MissingDataException("unsupported primitive")
      }
      // Check input ports
      inPorts.filter(portName => !allToConnPorts.contains(s"${args("name")}.$portName"))
        .foreach { portName =>
        print(s"[ERROR] Module template ($modName) has primitive (${args("name")}) ")
        println(s"with unconnected input port ($portName)")
        throw new MissingDataException("unconnected input port")
      }
      // Check output ports
      if (conf.CGRADebug) {
        outPorts.filter(portName => !allFromConnPorts.contains(s"${args("name")}.$portName"))
          .foreach { portName =>
          print(s"[DEBUG] Module template ($modName) has primitive (${args("name")}) ")
          println(s"with unconnected output port ($portName)")
        }
      }
    }

    // Last, verify that no input port is driven by multiple ports
    def multiplyDriven(ports: List[String], visited: Set[String] = Set.empty[String]): Option[String] = {
      ports match {
        case port :: tail =>
          if (visited(port)) Some(port) else multiplyDriven(tail, visited + port)
        case Nil =>
          None
      }
    }
    multiplyDriven(tmp.connections.map(_.toPorts).toList).foreach { portName =>
      print(s"[ERROR] Module template ($modName) has multiply-driven sub-module ")
      println(s"or primitive port ($portName)")
      throw new DuplicateDataException("multiply-connected input port")
    }

    // Finalize and return the template
    if (conf.CGRADebug) println(s"[DEBUG] Finished parsing module template ($modName)")
    tmp
  }

  /** Return a tuple of (portName, portSize) from XML 
   * @param modName the name of the parent module
   * @param port the XML-formatted port to scan
   * @param defs a map of definitions provided in the architecture
   * @param tmp the module template containing the port
   * @return a tuple of (portName, portSize)
   */
  private def _getPortNameAndSize(modName: String, port: Node, defs: Map[String, Int], tmp: Template) = {
    // Ports must be named and may have a size
    if ((port \ "@name").isEmpty) {
      println(s"[ERROR] Module template ($modName) has unnamed port")
      throw new MissingDataException("unnamed port")
    }
    val portName = port \@ "name"

    // Check if the port has a size
    if (!(port \ "@size").isEmpty) {
      val errMsg = s"[ERROR] Module template ($modName) has port ($portName) with invalid size"
      val arg = (port \@ "size")
      arg.toIntOption match {
        case Some(portSize) if portSize > 0 => (portName, portSize)
        case Some(portSize) =>
          println(errMsg)
          throw new NegativeDataSizeException("invalid port size")
        case _ =>
          if (!defs.contains(arg)) {
            println(errMsg)
            throw new MissingDataException("invalid port size")
          } else if (defs(arg) <= 0) {
            println(errMsg)
            throw new NegativeDataSizeException("invalid port size")
          } else (portName, defs(arg))
      }
    } else {
      (portName, tmp.dataSize)
    }
  }

  /** Parse and add ports to a module template
   * @param modName the name of the module template to operate on
   * @param mod the XML description of the module template
   * @param defs a map of definitions provided in the architecture
   * @param tmp the module template to update
   */
  private def _addPorts(modName: String, mod: Node, defs: Map[String, Int], tmp: Template)
    (implicit conf: cgragen.Parameters): Unit = {
    // ... add input ports
    (mod \\ "input").foreach { input =>
      // Get the port's name and size, and add it to the template
      val (portName, portSize) = _getPortNameAndSize(modName, input, defs, tmp)
      // Verify that the name is valid in Verilog
      if (!isValidName(portName)) {
        println(s"[ERROR] Module template ($modName) has input port ($portName) with invalid name")
        throw new InvalidIdentifierException("invalid input port name")
      }
      tmp.addPort(portName, PortInput, portSize)
      if (conf.CGRADebug) {
        print(s"[DEBUG] Added input port ($portName) of size ($portSize) ")
        println(s"to module template ($modName)")
      }
    }

    // ... add output ports
    (mod \\ "output").foreach { output =>
      // Get the port's name and size, and add it to the template
      val (portName, portSize) = _getPortNameAndSize(modName, output, defs, tmp)
      // Verify that the name is valid in Verilog
      if (!isValidName(portName)) {
        println(s"[ERROR] Module template ($modName) has output port ($portName) with invalid name")
        throw new InvalidIdentifierException("invalid output port name")
      }
      tmp.addPort(portName, PortOutput, portSize)
      if (conf.CGRADebug) {
        print(s"[DEBUG] Added output port ($portName) of size ($portSize) ")
        println(s"to module template ($modName)")
      }
    }

    // Check if the module has any ports and warn if not
    if (conf.CGRADebug && tmp.ports.isEmpty) {
      println(s"[DEBUG] Module template ($modName) has no ports")
    }
  }

  /** Parse and add wires to a module template
   * @param modName the name of the module template to operate on
   * @param mod the XML description of the module template
   * @param tmp the module template to update
   */
  private def _addWires(modName: String, mod: Node, tmp: Template)
    (implicit conf: cgragen.Parameters): Unit = {
    (mod \\ "wire").foreach { wire =>
      // Wires must be named
      if ((wire \ "@name").isEmpty) {
        println(s"[ERROR] Module template ($modName) has unnamed wire")
        throw new MissingDataException("unnamed wire")
      }
      val wireName = wire \@ "name"
      // Verify that the name is valid in Verilog
      if (!isValidName(wireName)) {
        println(s"[ERROR] Module template ($modName) has wire ($wireName) with invalid name")
        throw new InvalidIdentifierException("invalid wire name")
      }
      tmp.addWire(wireName)
      if (conf.CGRADebug) println(s"[DEBUG] Added wire ($wireName) to module template ($modName)")
    }
  }

  /** Parse and add primitives to a module template
   * @param modName the name of the module template to operate on
   * @param mod the XML description of the module template
   * @param defs a map of definitions provided in the architecture
   * @param tmp the module template to update
   */
  private def _addPrimitives(modName: String, mod: Node, defs: Map[String, Int], tmp: Template)
    (implicit conf: cgragen.Parameters): Unit = {
    (mod \\ "inst").foreach { prim =>
      // Primitives must be named and have a type
      if ((prim \ "@name").isEmpty) {
        println(s"[ERROR] Module template ($modName) has unnamed primitive instance")
        throw new MissingDataException("unnamed primitive instance")
      }
      val primName = prim \@ "name"
      if ((prim \ "@module").isEmpty) {
        println(s"[ERROR] Module template ($modName) has untyped primitive instance ($primName)")
        throw new MissingDataException("untyped primitive instance")
      }
      val primType = prim \@ "module"

      // Extract the primitive arguments
      val primArgs = {
        val passed = prim.attributes.asAttrMap - "module"
        if (passed.contains("size")) {
          val errMsg = s"[ERROR] Module template ($modName) has primitive instance ($primName) with invalid size"
          val arg = passed("size")
          arg.toIntOption match {
            case Some(size) if size > 0 => passed
            case Some(size) =>
              println(errMsg)
              throw new NegativeDataSizeException("invalid primitive size")
            case _ =>
              if (!defs.contains(arg)) {
                println(errMsg)
                throw new MissingDataException("invalid port size")
              } else if (defs(arg) <= 0) {
                println(errMsg)
                throw new NegativeDataSizeException("invalid port size")
              } else passed + ("size" -> defs(arg).toString())
          }
        } else {
          passed + ("size" -> tmp.dataSize.toString())
        }
      }

      // Verify that the name is valid in Verilog
      if (!isValidName(primName)) {
        println(s"[ERROR] Module template ($modName) has primitive instance ($primName) with invalid name")
        throw new InvalidIdentifierException("invalid primitive instance name")
      }
      tmp.addPrimitive(primType, primArgs)
      if (conf.CGRADebug) {
        print(s"[DEBUG] Added primitive instance ($primName) of type ($primType) ")
        println(s"to module template ($modName)")
      }
    }
  }

  /** Parse and add sub-modules to a module template
   * @param modName the name of the module template to operate on
   * @param mod the XML description of the module template
   * @param tmp the module template to update
   */
  private def _addSubModules(modName: String, mod: Node, tmp: Template)
    (implicit conf: cgragen.Parameters): Unit = {
    (mod \\ "submodule").foreach { subMod =>
      // Sub-modules must be named and have a type
      if ((subMod \ "@name").isEmpty) {
        println(s"[ERROR] Module template ($modName) has unnamed sub-module instance ($subMod)")
        throw new MissingDataException("unnamed sub-module instance")
      }
      if ((subMod \ "@module").isEmpty) {
        println(s"[ERROR] Module template ($modName) has untyped sub-module instance ($subMod)")
      }
      val (subModName, subModType) = (subMod \@ "name", subMod \@ "module")
      // Verify that the name is valid in Verilog
      if (!isValidName(subModName)) {
        println(s"[ERROR] Module template ($modName) has sub-module instance ($subModName) with invalid name")
        throw new InvalidIdentifierException("invalid sub-module instance name")
      }
      tmp.addSubModule(subModName, subModType)
      if (conf.CGRADebug) {
        print(s"[DEBUG] Added sub-module ($subModName) of type ($subModType) ")
        println(s"to module template ($modName)")
      }
    }
  }

  /** Parse and add connections to a module template
   * @param modName the name of the module template to operate on
   * @param mod the XML description of the module template
   * @param tmp the module template to update
   * @param parsed a map of named previously-parsed module templates
   */
  private def _addConnections(modName: String, mod: Node, tmp: Template, parsed: Map[String, Template])
    (implicit conf: cgragen.Parameters): Unit = {
    (mod \\ "connection").foreach { conn =>
      // Connections must have from- and to-types, i.e., define only one of 
      // (from, select-from) and only one of (to, distribute-to), with 
      // corresponding port name arguments
      if ((conn \ "@from").isEmpty && (conn \ "@select-from").isEmpty) {
        println(s"[ERROR] Module template ($modName) has connection with no from-type")
        throw new MissingDataException("missing from-type")
      } else if ((conn \ "@from").nonEmpty && (conn \ "@select-from").nonEmpty) {
        println(s"[ERROR] Module template ($modName) has connection with multiple from-types")
        throw new DuplicateDataException("multiple from-types")
      }
      if ((conn \ "@to").isEmpty && (conn \ "@distribute-to").isEmpty) {
        println(s"[ERROR] Module template ($modName) has connection with no to-type")
        throw new MissingDataException("missing to-type")
      } else if ((conn \ "@to").nonEmpty && (conn \ "@distribute-to").nonEmpty) {
        println(s"[ERROR] Module template ($modName) has connection with multiple to-types")
        throw new DuplicateDataException("multiple to-types")
      }
      val fromType = if ((conn \ "@from").isEmpty) "select-from" else "from"
      val fromArgs = conn \@ fromType
      val toType   = if ((conn \ "@to").isEmpty) "distribute-to" else "to"
      val toArgs   = conn \@ toType

      // Verify that none of the arguments are empty
      if (fromArgs == "") {
        println(s"[ERROR] Module template ($modName) has connection with no from-arguments")
        throw new MissingDataException("missing from-arguments")
      }
      if (toArgs == "") {
        println(s"[ERROR] Module template ($modName) has connection with no to-arguments")
        throw new MissingDataException("missing to-arguments")
      }

      // Verify that all targeted ports and wires exist
      /** Check existence of ports and wires referred to by connections
       * @param connArgs the from- or to-arguments of a connection
       */
      def checkConnArgs(connArgs: Iterable[String]) = connArgs.foreach { arg =>
        arg.split('.').toList match {
          case wireName :: Nil =>
            // The argument must refer to a wire
            if (!tmp.wires.contains(wireName)) {
              print(s"[ERROR] Module template ($modName) has connection to ")
              println(s"non-existent wire ($wireName)")
              throw new MissingDataException("invalid connection argument")
            }
          case subModName :: portName :: Nil =>
            // The argument must refer to a port in this template or in one of 
            // its sub-modules (or primitives, whose ports are not yet generated)
            subModName match {
              case "this" =>
                if (!tmp.ports.contains(portName)) {
                  print(s"[ERROR] Module template ($modName) has connection to ")
                  println(s"non-existent port ($portName)")
                  throw new MissingDataException("invalid connection argument")
                }
              case _ if tmp.subModules.contains(subModName) =>
                val subModTmp = parsed(tmp.subModules(subModName))
                if (!subModTmp.ports.contains(portName)) {
                  print(s"[ERROR] Module template ($modName) has connection to ")
                  println(s"non-existent sub-module port ($arg)")
                  throw new MissingDataException("invalid connection argument")
                }
              case _ =>
                // The argument refers to a primitive; skip it
            }
          case _ =>
            // The argument has too many hierarchical levels
            println(s"[ERROR] Module template ($modName) has connection with invalid argument ($arg)")
            throw new MissingDataException("invalid connection argument")
        }
      }
      checkConnArgs(fromArgs.split(' '))
      checkConnArgs(toArgs.split(' '))

      // Connections may have an optional sign-extension parameter passed to them
      val sext = {
        val sextOpt = if ((conn \ "@sext").isEmpty) None else Some(conn \@ "sext")
        sextOpt match {
          case Some(arg) =>
            arg.toIntOption match {
              case Some(num) => num != 0
              case _ =>
                println(s"[ERROR] Pattern has connection with invalid sign-extension argument")
                throw new MissingDataException("invalid sign-extension argument")
            }
          case _ => conf.Sext
        }
      }

      // Valid combinations of from- and to-types are (from, to), 
      // (from, distribute-to), and (select-from, to)
      (fromType, toType) match {
        case ("from", "to") =>
          // Verify that there is only one argument to from- and to-connections
          if (fromArgs.split(' ').length != 1 || toArgs.split(' ').length != 1) {
            print(s"[ERROR] Module template ($modName) has (from, to) connection ")
            println("with multiple from- or to-connections")
            throw new DuplicateDataException("multiple from- or to-arguments")
          }
          // If this connection points to a wire, update its arguments
          if (tmp.wires.contains(toArgs)) {
            tmp.updateWireArgs(toArgs, fromArgs)
            if (conf.CGRADebug) println(s"[DEBUG] Updated wire ($toArgs) with value ($fromArgs)")
          } else {
            // Otherwise, simply add it as a new connection
            tmp.addConnection(toType, toArgs, fromType, fromArgs, sext)
            if (conf.CGRADebug) println(s"[DEBUG] Added connection from ($fromArgs) to ($toArgs)")
          }

        case ("from", "distribute-to") =>
          // Verify that there is only one argument to from-connections
          if (fromArgs.split(' ').length != 1) {
            print(s"[ERROR] Module template ($modName) has (from, distribute-to) ")
            println("connection with multiple from-connections")
            throw new DuplicateDataException("multiple from-arguments")
          }
          // Run through each of the to-arguments and add individual 
          // connections to them (and remove duplicate connections silently)
          toArgs.split(' ').foreach { to =>
            // If this connection points to a wire, update its arguments
            if (tmp.wires.contains(to)) {
              tmp.updateWireArgs(to, fromArgs)
              if (conf.CGRADebug) println(s"[DEBUG] Updated wire ($to) with value ($fromArgs)")
            } else {
              // Otherwise, simply add it as a new connection
              tmp.addConnection("to", to, fromType, fromArgs, sext)
              if (conf.CGRADebug) println(s"[DEBUG] Added connection from ($fromArgs) to ($to)")
            }
          }

        case ("select-from", "to") =>
          // Verify that there is only one argument to to-connections
          if (toArgs.split(' ').length != 1) {
            print(s"[ERROR] Module template ($modName) has (select-from, to) ")
            println("connection with multiple to-connections")
            throw new DuplicateDataException("multiple to-arguments")
          }
          // If there is only one from-argument, turn this connection into a 
          // (from, to)-type equivalent
          if (fromArgs.split(' ').length == 1) {
            // If this connection points to a wire, update its arguments
            if (tmp.wires.contains(toArgs)) {
              tmp.updateWireArgs(toArgs, fromArgs)
              if (conf.CGRADebug) {
                print(s"[DEBUG] Updated wire ($toArgs) with value ($fromArgs) ")
                println(s"adapted from a (select-from, to) connection")
              }
            } else {
              // Otherwise, simply add it as a new connection
              tmp.addConnection(toType, toArgs, "from", fromArgs, sext)
              if (conf.CGRADebug) {
                print(s"[DEBUG] Added connection from ($fromArgs) to ($toArgs) ")
                println("adapted from a (select-from, to) connection")
              }
            }
          } else {
            // Otherwise, instantiate a mux and connect it accordingly
            val numInputs   = fromArgs.split(' ').length
            val namePostfix = {
              val split = toArgs.split('.')
              if (split.head == "this") split.tail.mkString("_") else split.mkString("_")
            }
            val muxName     = s"mux_${namePostfix}"
            val muxArgs     = Map(
              "name" -> muxName, 
              "module" -> "Multiplexer", 
              "size" -> tmp.dataSize.toString(), 
              "ninput" -> numInputs.toString()
            )
            tmp.addPrimitive("Multiplexer", muxArgs)
            if (conf.CGRADebug) println(s"[DEBUG] Added multiplexer with arguments ($muxArgs)")

            // Connect the from-arguments to the mux
            fromArgs.split(' ').zipWithIndex.foreach { case (from, ind) =>
              tmp.addConnection("to", s"$muxName.in$ind", "from", from, sext)
              if (conf.CGRADebug) println(s"[DEBUG] Added connection from ($from) to ($muxName.in$ind)")
            }

            // Connect the mux to the to-argument
            if (tmp.wires.contains(toArgs)) {
              // If this connection points to a wire, update its arguments
              tmp.updateWireArgs(toArgs, s"$muxName.out")
              if (conf.CGRADebug) println(s"[DEBUG] Updated wire ($toArgs) with value ($muxName.out)")
            } else {
              // Otherwise, simply add it as a new connection
              tmp.addConnection("to", toArgs, "from", s"$muxName.out", sext)
              if (conf.CGRADebug) println(s"[DEBUG] Added connection from ($muxName.out) to ($toArgs)")
            }
          }

        case _ =>
          // If no previous case matched, the port type combination is invalid
          print(s"[ERROR] Module template ($modName) has connection ($conn) ")
          println(s"with invalid (from, to) type pair")
          throw new MissingDataException("invalid (from, to) type pair")
      }
    }
  }

  /** Update module template to connect ports to other ports rather than wires
   * @param modName the name of the module template to operate on
   * @param tmp the module template to update
   */
  private def _connectWires(modName: String, tmp: Template)
    (implicit conf: cgragen.Parameters): Unit = {
    tmp.connections.zipWithIndex.foreach { case (conn, ind) =>
      // If this connection's from-argument is a wire, change it to the 
      // wire's argument
      if (tmp.wires.contains(conn.fromPorts)) {
        val upc = conn.copy(fromPorts = tmp.wires(conn.fromPorts))
        tmp.updateConnection(ind, upc)
        if (conf.CGRADebug) println(s"[DEBUG] Updated connection between (${conn.toPorts}) and (${conn.fromPorts})")
      }
      // If this connection's to-argument is a wire, change it to the 
      // wire's argument
      if (tmp.wires.contains(conn.toPorts)) {
        val upc = conn.copy(toPorts = tmp.wires(conn.toPorts))
        tmp.updateConnection(ind, upc)
        if (conf.CGRADebug) println(s"[DEBUG] Updated connection between (${conn.toPorts}) and (${conn.fromPorts})")
      }
    }

    // Verify that all wires have been disconnected, and then clear the map
    val allConnPorts = tmp.connections.flatMap(conn => Seq(conn.toPorts, conn.fromPorts)).toSet
    tmp.wires.keys.filter(wire => allConnPorts.contains(wire)).foreach { wire =>
      println(s"[ERROR] Module template ($modName) has wire with remaining connections")
      throw new MissingDataException("wire with remaining connections")
    }
    tmp.wires.keys.foreach(tmp.removeWire(_))
  }
}
