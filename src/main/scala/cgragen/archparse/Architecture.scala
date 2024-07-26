package cgragen.archparse

import cgragen.archparse.PortType._

import scala.collection.mutable

import scala.xml.{Elem, Node}
import scala.xml.XML.loadFile

/** Abstract architecture 
 * @param rows the number of rows in the CGRA architecture
 * @param cols the number of columns in the CGRA architecture
 * 
 * @note Used internally for parsing and conversion to internal representation.
 */
private[cgragen] final class Architecture(_rows: Int, _cols: Int)(implicit conf: cgragen.Parameters) {
  require(_rows > 0, "number of rows must be positive")
  require(_cols > 0, "number of columns must be positive")

  /** Fields start ***********************************************************/
  // Abstract representation of module templates in this architecture
  private val _modTemplates = mutable.HashMap.empty[String, Template]

  // Abstract representation of sub-modules in this architecture
  private val _subModules = mutable.HashMap.empty[String, Template]

  // Abstract representation of connections in this architecture
  private val _connections = mutable.ArrayBuffer.empty[Connection]

  // Constant definitions in the architecture
  private val _definitions = mutable.HashMap.empty[String, Int]
  /** Fields end *************************************************************/

  /** Accessors start ********************************************************/
  /** Return the module templates of this architecture */
  def modTemplates = _modTemplates.toMap

  /** Return the sub-module instances of this architecture */
  def subModules = _subModules.toMap

  /** Return the connections of this architecture */
  def connections = _connections.toArray

  /** Return the definitions of this architecture */
  def definitions = _definitions.toMap
  /** Accessors end **********************************************************/

  /** Modifiers start ********************************************************/
  /** Add a constant definition from XML to this architecture
   * @param defn the XML-formatted definition to add
   */
  def addDefinition(defn: Node): Unit = {
    // First identify the name of the constant
    if ((defn \ "@name").isEmpty) {
      println("[ERROR] Constant definition is missing a name")
      throw new MissingDataException("missing name in definition")
    }
    val defnName = defn \@ "name"
    if (conf.CGRADebug) println(s"[DEBUG] Found constant definition ($defnName) - parsing its value")

    // Then identify its value and check that it's integral
    if ((defn \ "@value").isEmpty) {
      println(s"[ERROR] Constant definition ($defnName) is missing a value")
      throw new MissingDataException("missing value in definition")
    } else if ((defn \@ "value").toIntOption == None) {
      println(s"[ERROR] Constant definition ($defnName) has a non-integral value")
      throw new MissingDataException("ill-typed value in definition")
    }
    val defnValue = (defn \@ "value").toInt

    // Finalize and add the constant to the architecture
    if (conf.CGRADebug) println(s"[DEBUG] Finished parsing definition ($defnName) with value ($defnValue)")
    _definitions(defnName) = defnValue
  }

  /** Add a module template to this architecture
   * @param name the name of the template
   * @param temp the parsed template to add
   */
  def addTemplate(name: String, temp: Template): Unit = {
    if (conf.CGRADebug) println(s"[DEBUG] Added module template ($name) to architecture")
    _modTemplates += ((name, temp))
  }

  /** Add a sub-module to this architecture
   * @param blockName the name of the sub-module
   * @param blockType the name of the parsed template
   */
  def addSubModule(blockName: String, blockType: String): Unit = {
    // Verify that the named template exists
    if (!_modTemplates.contains(blockType)) {
      println(s"[ERROR] Cannot add sub-module ($blockName) with unknown template ($blockType)")
      throw new MissingFieldException("missing module template")
    }

    // Now add the sub-module to the architecture
    _subModules(blockName) = _modTemplates(blockType)
  }

  /** Add the sub-modules of a pattern to this architecture
   * @param pat the parsed pattern to extract sub-modules from
   */
  def addPatternSubModules(pat: Pattern): Unit = {
    // Verify that all sub-modules of the pattern have matching module templates
    pat.subModules
      .filter { case (_, blockType) => !_modTemplates.contains(blockType) }
      .foreach { case (blockName, blockType) =>
      println(s"[ERROR] Pattern has sub-module ($blockName) of undeclared type ($blockType)")
      throw new MissingFieldException("missing module template")
    }

    // Add all the pattern sub-module
    pat.subModules.foreach { case (blockName, blockType) =>
      addSubModule(blockName, blockType)
    }
    if (conf.CGRADebug) println(s"[DEBUG] Added pattern sub-modules to architecture")
  }

  /** Add the _connections of a sub-module to this architecture
   * @param pat the parsed pattern to extract connections from
   */
  def addPatternConnections(pat: Pattern): Unit = {
    // Verify that all connections of the pattern point to existing ports
    // ... check input ports
    pat.connections.map(_.toPorts).foreach { toArg =>
      toArg.split('.').toList match {
        case modName :: portName :: Nil =>
          // Get the names of the module template's input ports
          val tempInputPorts = _subModules(modName).ports
            .collect { case (tempPortName, (tempPortType, _)) if tempPortType == PortInput => tempPortName }
            .toSeq
          if (!tempInputPorts.contains(portName)) {
            println(s"[ERROR] Pattern has connection to non-existent port ($toArg)")
            throw new MissingDataException("missing input port")
          }
        case _ =>
          // Should never occur
          println(s"[ERROR] Pattern has connection with invalid to-argument ($toArg)")
          throw new MissingDataException("invalid connection argument")
      }
    }

    // ... check output ports
    pat.connections.map(_.fromPorts).foreach { fromArg =>
      fromArg.split('.').toList match {
        case modName :: portName :: Nil =>
          // Get the names of the module template's output ports
          val tempOutputPorts = _subModules(modName).ports
            .collect { case (tempPortName, (tempPortType, _)) if tempPortType == PortOutput => tempPortName }
            .toSeq
          if (!tempOutputPorts.contains(portName)) {
            println(s"[ERROR] Pattern has connection from non-existent port ($fromArg)")
            throw new MissingDataException("missing output port")
          }
        case _ =>
          // Should never occur
          println(s"[ERROR] Pattern has connection with invalid from-argument ($fromArg)")
          throw new MissingDataException("invalid connection argument")
      }
    }

    // Finally, add all the _connections to the architecture
    _connections ++= pat.connections
    if (conf.CGRADebug) println(s"[DEBUG] Added pattern _connections to architecture")
  }
  /** Modifiers end **********************************************************/

  /** Various other methods start ********************************************/
  /** Return this architecture in a string */
  override def toString(): String = {
    val bs = new StringBuilder(s"Architecture with size ${_rows}x${_cols} has the following contents:\n")

    // Add all module templates to the string
    if (!_modTemplates.isEmpty) {
      bs ++= "Module templates:\n"
      bs ++= _modTemplates.map { _._2.toString(ind = 1) }.mkString("\n")
    }

    // Add all sub-modules to the string
    if (!_subModules.isEmpty) {
      bs ++= "\nSub-modules:\n"
      bs ++= _subModules.map { case (modName, temp) => 
        s"  Module ($modName) of template type (${temp.name})"
      }.mkString("\n")
    }

    // Add all the _connections to the string
    if (!_connections.isEmpty) {
      bs ++= "\n_connections:\n"
      bs ++= _connections.map { case Connection(toType, toName, fromType, fromName, sext) => 
        s"  Connection from ($fromName) to ($toName) with ${if (sext) "sign" else "zero"}-extension"
      }.mkString("\n")
    }

    bs.mkString
  }

  /** Get the top-level module templates in this architecture */
  def getTopModTemplates(): Map[String, Template] = {
    val temps = modTemplates
    temps.filterNot { case (name, _) => (temps - name).exists(_._2.subModules.contains(name)) }
  }
  /** Various other methods end **********************************************/
}

object Architecture {
  /** Parse an architecture file
   * @param file a string containing an architecture file path
   * @return a new instance of [[cgragen.archparse.Architecture]] resulting 
   *         from the parsing
   */
  def apply(file: String)(implicit conf: cgragen.Parameters): Architecture = {
    if (conf.CGRADebug) println(s"[DEBUG] Parsing architecture file ($file)")

    // Open the file and parse it as XML
    val arch = Architecture(loadFile(file))

    // Finalize and return the architecture
    if (conf.CGRADebug) println(s"[DEBUG] Finished parsing architecture file ($file)")
    arch
  }

  /** Parse an XML description of an architecture
   * @param xml the XML description of the architecture
   * @return a new instance of [[cgragen.archparse.Architecture]] resulting 
   *         from the parsing
   */
  def apply(xml: Elem)(implicit conf: cgragen.Parameters): Architecture = {
    // Check that the XML has the CGRA tag
    (xml \\ "CGRA").length match {
      case 0 =>
        println("[ERROR] Architecture file should specify exactly one CGRA")
        throw new MissingFieldException("no <CGRA> field")
      case x if x > 1 =>
        println("[ERROR] Architecture file should specify exactly one CGRA")
        throw new DuplicateFieldException("multiple <CGRA> fields")
      case _ =>
    }
    if (!((xml \\ "CGRA").head == xml)) {
      println("[ERROR] Architecture file has misplaced CGRA declaration")
      throw new MisplacedFieldException("<CGRA>")
    }

    // Generate an architecture data instance from the specification
    (xml \\ "architecture").length match {
      case 0 =>
        println("[ERROR] Architecture file should specify exactly one architecture")
        throw new MissingFieldException("no <architecture> field")
      case x if x > 1 =>
        println("[ERROR] Architecture file should specify exactly one architecture")
        throw new DuplicateFieldException("multiple <architecture> fields")
      case _ =>
    }
    if ((xml \ "architecture").length != (xml \\ "architecture").length) {
      println("[ERROR] Architecture file has misplaced architecture declaration")
      throw new MisplacedFieldException("architecture")
    }
    val xmlArch = (xml \ "architecture").head

    // Find the architecture's number of rows and columns
    if ((xmlArch \ "@row").isEmpty) {
      println("[ERROR] Architecture should specify a number of rows")
      throw new MissingDataException("row count not present")
    }
    if ((xmlArch \ "@col").isEmpty) {
      println("[ERROR] Architecture should specify a number of columns")
      throw new MissingDataException("column count not present")
    }
    val rows = (xmlArch \@ "row").toIntOption.getOrElse {
      println("[ERROR] Number of rows must be integral")
      throw new MissingDataException("row count invalid")
    }
    val cols = (xmlArch \@ "col").toIntOption.getOrElse {
      println("[ERROR] Number of columns must be integral")
      throw new MissingDataException("column count invalid")
    }

    // Create and initialize an architecture
    val arch = new Architecture(rows, cols)

    // Parse all the definitions in the XML
    if ((xml \ "definition").length != (xml \\ "definition").length) {
      println("[ERROR] Architecture has misplaced definitions")
      throw new MisplacedFieldException("definitions")
    }
    (xml \ "definition").foreach { d => arch.addDefinition(d) }

    // Parse all the module templates in the XML (and check that no primitives 
    // are instantiated outside module templates)
    if ((xml \ "template").length != (xml \\ "template").length) {
      println("[ERROR] Architecture has misplaced module template declarations")
      throw new MisplacedFieldException("module template declarations")
    }
    if ((xml \\ "inst").length != ((xml \ "template") \ "inst").length) {
      println("[ERROR] Architecture has misplaced primitive instantiations")
      throw new MisplacedFieldException("primitive instantiations")
    }
    // Check that no two module templates are missing a name or have the same name
    (xml \ "template").filter(mod => (mod \ "@name").isEmpty).foreach { mod =>
      println(s"[ERROR] Architecture has unnamed module template")
      throw new MissingDataException("unnamed module template")
    }
    (xml \ "template").foldLeft(Set.empty[String]) { case (acc, mod) =>
      val modName = mod \@ "name"
      if (acc.contains(modName)) {
        println(s"[ERROR] Architecture has module template with duplicate name ($modName)")
        throw new DuplicateDefinitionException("duplicate template name")
      } else {
        acc + modName
      }
    }

    // Get the module template dependencies of each module template individually
    val individualDependencies = Map.from((xml \ "template").map { mod =>
      val name = mod \@ "name"
      // Check that each sub-module is named and typed
      (mod \ "submodule").filter(subMod => (subMod \ "@name").isEmpty).foreach { subMod =>
        println(s"[ERROR] Module template ($name) has unnamed sub-module instance")
        throw new MissingDataException("unnamed sub-module instance")
      }
      (mod \ "submodule").filter(subMod => (subMod \ "@module").isEmpty).foreach { subMod =>
        val subModName = subMod \@ "name"
        println(s"[ERROR] Module template ($name) has untyped sub-module instance ($subModName)")
        throw new MissingDataException("untyped sub-module instance")
      }
      // Now get their types
      val deps = (mod \ "submodule").map(_ \@ "module").toSet
      (name -> deps)
    })
    // Check that all identified dependencies refer to defined templates
    individualDependencies.values.flatten.filter(!individualDependencies.contains(_)).foreach { dep =>
      println(s"[ERROR] Module template has undefined sub-module dependency ($dep)")
      throw new MissingDataException("undefined sub-module")
    }

    // Combine dependencies recursively and check that no template is 
    // dependent on itself
    val combinedDependencies = individualDependencies.map { case (name, deps) =>
      (name -> (deps ++ deps.flatMap(_getRecDeps(name, _, individualDependencies))))
    }

    // Run through hierarchy of module templates while keeping track of 
    // already parsed templates, and add the templates to the architecture
    _parseHierarchy(xml, combinedDependencies, arch.definitions).foreach { case (name, temp) => 
      arch.addTemplate(name, temp)
    }

    // Knowing the basics of the architecture and its templates, parse the 
    // CGRA's architectural patterns
    if ((xml \ "architecture" \ "pattern").length != (xml \\ "pattern").length) {
      println("[ERROR] Architecture has misplaced patterns")
      throw new MisplacedFieldException("patterns")
    }
    val patterns = (xml \ "architecture" \ "pattern").map(pat => Pattern(pat, arch.definitions))

    // Add all sub-modules first, then add their _connections. This helps to 
    // ensure independence from parsing order
    patterns.foreach(pat => arch.addPatternSubModules(pat))
    patterns.foreach(pat => arch.addPatternConnections(pat))
    
    arch
  }

  /** Recursively fetch module template dependencies of a template
   * @param start the module template for which the fetch was initiated
   * @param current the module template whose dependencies are currently fetched
   * @param indDeps a map of first-level dependencies of known module templates
   * @return a set of module template names of all dependencies of `start`
   */
  private def _getRecDeps(start: String, current: String, indDeps: Map[String, Set[String]]): Set[String] = {
    // Check that the template is not dependent on itself
    if (indDeps(current).contains(start)) {
      println(s"[ERROR] Module template ($start) is recursively self-dependent")
      throw new RecursiveDefinitionException("self-dependent module template")
    }
    // Recursively get dependencies of the current template's dependencies
    val currDeps = indDeps(current)
    val recDeps  = indDeps(current).flatMap(_getRecDeps(start, _, indDeps))
    currDeps ++ recDeps
  }

  /** Parse a module template hierarchy according to its dependencies
   * @param xml the XML description of the architecture
   * @param pending a map of module template names and their dependencies 
   *                still waiting to be parsed
   * @param defs a map of definitions provided in the architecture
   * @param parsed a map of module template names and parsed template instances
   * @return a map of module template names and parsed template instances
   */
  private def _parseHierarchy(
    xml    : Node, 
    pending: Map[String, Set[String]], 
    defs   : Map[String, Int], 
    parsed : Map[String, Template] = Map.empty[String, Template], 
  )(
    implicit conf: cgragen.Parameters
  ): Map[String, Template] = {
    // If templates is empty, return the map of parsed templates
    if (pending.isEmpty) {
      parsed
    } else {
      // Otherwise, select a template all of whose dependencies have been 
      // parsed, parse it, and add it to the map. Knowing that there are no 
      // self-dependencies, the hierarchy is a tree and there will always be 
      // a valid leaf node to parse
      pending.collect { case (name, deps) if deps.forall(parsed.contains(_)) => name } match {
        case name :: rest =>
          val template = Template((xml \ "template").filter(mod => (mod \@ "name") == name).head, defs, parsed)
          _parseHierarchy(xml, pending - name, defs, parsed + (name -> template))
        case _ => // should never occur
          throw new RecursiveDefinitionException("no parseable module template")
      }
    }
  }
}
