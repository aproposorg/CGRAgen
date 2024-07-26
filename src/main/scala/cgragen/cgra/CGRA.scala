package cgragen.cgra

import cgragen.archparse.{Architecture, Connection}
import cgragen.archparse.PortType._

import cgragen.cgra.AbstractOperation, AbstractOperation.isApproximable
import cgragen.cgra.primitives._
import cgragen.cgra.ModuleType._

import scala.collection.mutable

/** Abstract CGRA representation
 * @param name the name of the CGRA
 */
private[cgragen] final class CGRA(name: String, dataSize: Int)(implicit conf: cgragen.Parameters)
  extends AbstractModule(name, dataSize) {
  /** Modifiers start ********************************************************/
  /** Add a sub-module to the module
   * @param m the module to add
   * 
   * @note Overrides the [[AbstractModule]]'s method to ensure no primitives
   *       are instantiated at this level.
   */
  override def addSubModule[T <: AbstractModule](m: T): Unit = {
    // If the module is not a composite, it must be a primitive
    if (m.modType != ModComposite) {
      print(s"[ERROR] Sub-module (${m.name}) is a primitive and cannot be ")
      println("instantiated in the top-level module")
      throw new TopLevelModuleException("primitive instantiation")
    }
    super.addSubModule(m)
  }
  /** Modifiers end **********************************************************/

  /** Various other methods start ********************************************/
  /** Return the CGRA in a DOT-formatted string
   * @return a string representation of the CGRA
   */
  def toDotString(): String = {
    // Helper to build representations of primitives
    def _primHelper(mod: AbstractModule)(ind: Int): String = {
      val outerWs = " " * ind
      val innerWs = " " * (ind + 2)
      val bs = new StringBuilder(s"${outerWs}subgraph \"${mod.fullPath}\" {\n")
      bs ++= s"${innerWs}label=\"${mod.name}\";\n"

      // Find the shape of the primitive block based on its type
      val shape = mod match {
        case _: AbstractConstantUnit    => "Msquare"
        case _: AbstractFunctionUnit    => "invhouse"
        case _: AbstractMultiplexerUnit => "invtrapezium"
        case _: AbstractRegisterUnit | _: AbstractRegisterFileUnit => "rectangle"
        case _: AbstractInputUnit    | _: AbstractOutputUnit       => "Mcircle"
        case _ =>
          println(s"[ERROR] The given module (${mod.name}) is not a primitive")
          throw new Exception("invalid module for DOT generation")
      }

      // Input all ports here
      bs ++= s"${innerWs}\"${mod.fullPath}\"[label=\"${mod.name}\",shape=\"$shape\"];\n"
      mod.ports.foreach { case (portName, port) =>
        val attrs = s"label=\"${portName}\",shape=\"rectangle\""
        bs ++= s"${innerWs}\"${port.parent.fullPath}.${portName}\"[$attrs];\n"
        bs ++= s"${innerWs}\"${mod.fullPath}\"->\"${port.parent.fullPath}.${portName}\"[arrowhead=\"none\"];\n" }

      bs ++= s"${outerWs}}\n"
      bs.mkString
    }

    // Helper to recursively build representations of sub-modules
    def _helper(mod: AbstractModule)(ind: Int): String = {
      val outerWs = " " * ind
      val innerWs = " " * (ind + 2)
      val bs = new StringBuilder(s"${outerWs}subgraph \"${mod.fullPath}\" {\n")
      bs ++= s"${innerWs}label=\"${mod.name}\";\n"
      bs ++= s"${innerWs}color=blue;\n"

      // Recursively add sub-module nodes to the graph
      mod.subModules.values.foreach { _ match {
        case base: AbstractBaseModule     => bs ++= _helper(base)(ind + 2)
        case const: AbstractConstantUnit  => bs ++= _primHelper(const)(ind + 2)
        case fu: AbstractFunctionUnit     => bs ++= _primHelper(fu)(ind + 2)
        case mux: AbstractMultiplexerUnit => bs ++= _primHelper(mux)(ind + 2)
        case reg: AbstractRegisterUnit    => bs ++= _primHelper(reg)(ind + 2)
        case rf: AbstractRegisterFileUnit => bs ++= _primHelper(rf)(ind + 2)
        case in: AbstractInputUnit        => bs ++= _primHelper(in)(ind + 2)
        case out: AbstractOutputUnit      => bs ++= _primHelper(out)(ind + 2)
        case _ => // no action needed
      }}

      // Add ports and connections to the graph
      mod.ports
        .filter  { case (portName, _) => !mod.subModules.contains(portName) }
        .foreach { case (portName, port) =>
          val attrs = s"label=\"${portName}\",shape=\"rectangle\""
          bs ++= s"${innerWs}\"${port.parent.fullPath}.${portName}\"[$attrs];\n" }

      mod.connections.values.foreach { conn =>
        val srcName = s"\"${conn.source.parent.fullPath}.${conn.source.name}\""
        conn.sinks.foreach { snk =>
          val snkName = s"\"${snk.parent.fullPath}.${snk.name}\""
          bs ++= s"${innerWs}${srcName}->${snkName};\n"
        }
      }

      bs ++= s"${outerWs}}\n"
      bs.mkString
    }

    // Recursively add all sub-modules to the graph
    val bs = new StringBuilder(s"digraph ${name} {\n")
    subModules.values.foreach(subMod => bs ++= _helper(subMod)(2))

    // Add any top-level connections to the graph
    connections.values.foreach { conn =>
      val srcName = s"\"${conn.source.parent.fullPath}.${conn.source.name}\""
      conn.sinks.foreach { snk =>
        val snkName = s"\"${snk.parent.fullPath}.${snk.name}\""
        bs ++= s"  ${srcName}->${snkName};\n"
      }
    }

    bs ++= "}\n"
    bs.mkString
  }

  /** Return this CGRA in a string */
  override def toString(): String = toDotString()

  /** Recursively fetch all of this CGRA's submodules' config cells and order 
   * them according to, first, their parent names and, then, port names
   */
  override def configOrder: Seq[ConfigCell] = {
    subModules.keys
      .toSeq.sorted
      .flatMap { name =>
        subModules(name).configOrder
          .sortBy(cell => s"${cell.port.parent.fullPath}_${cell.port.name}")
    }
  }
  /** Various other methods end **********************************************/
}

private[cgragen] object CGRA {
  /** Create a new CGRA with the specified architecture
   * @param arch the abstract architecture of the CGRA
   * @return a new instance of [[CGRA]]
   */
  def apply(arch: Architecture)(implicit conf: cgragen.Parameters) = {
    if (conf.CGRADebug) println("[DEBUG] Converting architecture to CGRA")

    // Create an empty CGRA with a generic top name
    val cgra = new CGRA("CGRA", conf.DataSize)

    // First, iterate over and instantiate all sub-modules by adding them to the CGRA
    if (arch.subModules.nonEmpty) {
      if (conf.CGRADebug) println("[DEBUG] Adding sub-modules to CGRA")
      arch.subModules.foreach { case (modName, modTemp) =>
        // Generate a new module with this name and template (architecture must be passed
        // as the function might need recursive calls to instantiate sub-modules to the 
        // sub-module)
        cgra.addSubModule(AbstractModule(modName, modTemp, arch))
      }
    }

    // Add all top-level connections to the CGRA
    if (arch.connections.nonEmpty) {
      if (conf.CGRADebug) println("[DEBUG] Adding connections to CGRA")
      arch.connections.foreach { case Connection(toType, toArgs, fromType, fromArgs, sext) =>
        assert(toType == "to", "connection should have been parsed to have only to-type = \"to\"")
        assert(fromType == "from", "connection should have been parsed to have only from-type = \"from\"")
        cgra.addConnection(fromArgs, toArgs, sext)
      }
    }

    // There are no unconnected ports in the CGRA's submodules, but the 
    // top-level sub-modules may have unconnected IO. If requested, collect 
    // these in an additional block
    if (conf.CGRAInferTopLevelIO && arch.subModules.nonEmpty) {
      if (conf.CGRADebug) println("[DEBUG] Inferring top-level ports in CGRA")

      // Ensure that no input or output primitives are specified
      val ioPrims = _findIOPrims(cgra)
      if (ioPrims.nonEmpty) {
        print("[ERROR] Cannot infer partially specified top-level IO - ")
        println(s"found (${ioPrims.map(_.fullPath).mkString("[", ", ", "]")})")
        throw new TopLevelIOInferenceException("partially specified IO")
      }

      // Find all unconnected input ports first
      val inPorts = cgra.subModules.flatMap { case (modName, mod) =>
        mod.ports
          .filter { case (_, port) =>
            port.pt == PortInput &&
            !cgra.subModules.filterNot(_._1 == modName).exists(_._2.connections.values.exists(_.sinks.contains(port))) &&
            !cgra.connections.exists(_._2.sinks.contains(port)) }
          .map { case (portName, port) =>
            val topPortName = s"${modName}_${portName}_inf"
            if (!cgra.subModules.contains(conf.CGRATopLevelIOName)) {
              cgra.addSubModule(new AbstractBaseModule(conf.CGRATopLevelIOName, cgra.dataSize, "inf"))
            }
            cgra.subModules(conf.CGRATopLevelIOName).addPort(topPortName, PortOutput, port.dataSize)
            cgra.subModules(conf.CGRATopLevelIOName).addSubModule(AbstractInputUnit(topPortName, port.dataSize))
            cgra.subModules(conf.CGRATopLevelIOName).addConnection(s"$topPortName.out", s"this.$topPortName", false)
            cgra.addConnection(s"${conf.CGRATopLevelIOName}.$topPortName", s"$modName.$portName", false)
            topPortName }
      }

      // Then find all unconnected output ports
      val outPorts = cgra.subModules.flatMap { case (modName, mod) =>
        mod.ports
          .filter { case (_, port) =>
            port.pt == PortOutput &&
            !cgra.subModules.filterNot(_._1 == modName).exists(_._2.connections.contains(port)) &&
            !cgra.connections.contains(port) }
          .map { case (portName, port) =>
            val topPortName = s"${modName}_${portName}_inf"
            if (!cgra.subModules.contains(conf.CGRATopLevelIOName)) {
              cgra.addSubModule(new AbstractBaseModule(conf.CGRATopLevelIOName, cgra.dataSize, "inf"))
            }
            cgra.subModules(conf.CGRATopLevelIOName).addPort(topPortName, PortInput, port.dataSize)
            cgra.subModules(conf.CGRATopLevelIOName).addSubModule(AbstractOutputUnit(topPortName, port.dataSize))
            cgra.subModules(conf.CGRATopLevelIOName).addConnection(s"this.$topPortName", s"$topPortName.in")
            cgra.addConnection(s"$modName.$portName", s"${conf.CGRATopLevelIOName}.$topPortName", false)
            topPortName }
      }

      if (conf.CGRADebug) {
        print(s"[DEBUG] Inferred a total of (${inPorts.size}) input ports ")
        println(s"and (${outPorts.size}) output nodes from CGRA")
      }
    }

    // Verify that the CGRA has top-level IO
    if (conf.CGRADebug) {
      val ioPrims = _findIOPrims(cgra)
      if (ioPrims.collect { case in: AbstractInputUnit => in }.isEmpty)
        println(s"[DEBUG] CGRA has no top-level input ports")
      if (ioPrims.collect { case out: AbstractOutputUnit => out }.isEmpty)
        println(s"[DEBUG] CGRA has no top-level output ports")
    }

    // Conversion was successfull, finalize and return the CGRA
    if (conf.CGRADebug) println("[DEBUG] Finished converting architecture to CGRA")
    cgra
  }

  /** Recursively find all IO primitives in a module
   * @param mod the module to search through
   * @return a list of all IO primitives in `mod`
   */
  private def _findIOPrims(mod: AbstractModule): Seq[AbstractModule] = mod.modType match {
    case ModComposite => mod.subModules.values.toSeq.flatMap(_findIOPrims(_))
    case ModPrimIO => Seq(mod)
    case _ => Seq.empty[AbstractModule]
  }

  /** Find the set of approximate arithmetic operations supported by a CGRA
   * @param cgra the CGRA to search through
   * @return the set of approximate arithmetic operations supported by `cgra`
   */
  private[cgragen] def supportedApprxOps(cgra: CGRA)
    (implicit conf: cgragen.Parameters): Seq[AbstractOperation] = {
    // Collect the approximate operations recursively on each sub-module
    def _dfs[T <: AbstractModule](mod: T): Seq[AbstractOperation] = mod match {
      case fu: AbstractFunctionUnit =>
        if (!fu.ports.contains("mode")) Seq.empty[AbstractOperation]
        else fu.operations.filter(oprtn => isApproximable(oprtn.op))
      case _ =>
        mod.subModules.values
          .foldLeft(Seq.empty[AbstractOperation]) { case (acc, subMod) => acc ++ _dfs(subMod) }
    }

    cgra.subModules.values
      .foldLeft(Seq.empty[AbstractOperation]) { case (acc, block) => acc ++ _dfs(block) }
  }
}
