package cgragen.hwgen

import cgragen.cgra.CGRA
import cgragen.cgra.ModuleType.ModComposite

import cgragen.hwgen.ConfigurationType._

import chisel3._
import chisel3.experimental.noPrefix
import chisel3.experimental.hierarchy.{Definition, Instance}
import chisel3.util.experimental.forceName

import scala.collection.mutable

/** Hardware CGRA without top-level configuration logic
 * @param mod the module to generate hardware from
 * @param confType the type of configuration to implement
 */
private[hwgen] final class CGRAModule(cgra: CGRA, confType: ConfigurationType)
  (implicit conf: cgragen.Parameters) extends Buildable(cgra, confType) {
  require(cgra.subModules.forall(_._2.modType == ModComposite),
    "cannot generate CGRA module with top-level primitives")
  require(cgra.ports.isEmpty, "cannot generate CGRA module with top-level ports")
  require(cgra.connections.values.flatMap(_.sinks).forall(_.parent != cgra),
    "cannot generate CGRA module with top-level ports")
  require(cgra.connections.values.map(_.source).forall(_.parent != cgra),
    "cannot generate CGRA module with top-level ports")

  /** Generate the hardware within this CGRA
   * 
   * @note By default, this procedure involves the following steps in the 
   *       specified order:
   *       1. instantiation of the sub-modules as instances of `TopModule`
   *       2. creation of all ports
   *       3. connection of ports with sub-modules
   *       This order must be maintained in order to ensure the module's IO is 
   *       not referenced before it has been fully specified, avoiding 
   *       unintentional elaboration.
   */
  override def build(): Unit = {
    if (conf.HWDebug) println(s"[DEBUG] Building data path in CGRA (${cgra.name})")

    // Construct all the sub-modules recursively and add them to the internal 
    // storage for later referencing
    cgra.subModules.values.foreach { subMod =>
      val spcfr = s"${subMod.templateName}_${subMod.dataSize}"
      if (!_defs.contains(spcfr))
        _defs(spcfr) = Definition(new TopModule(subMod, confType) {
          override val desiredName = spcfr
        })
      _insts(subMod.name) = Instance(_defs(spcfr)) }

    // Connect the enable and clear inputs
    _insts.keys.foreach { name => 
      _insts(name).en  := en
      _insts(name).clr := clr
    }

    /** Provide pass-through connections for any unconnected sub-module ports
     * 
     * This task is particularly easy if the top-level IO is inferred as 
     * the CGRA will contain an additional sub-module, "conf.TopLevelIOName", 
     * from which all the relevant ports can be extracted.
     * 
     * Otherwise, the required ports must be manually extracted depending on 
     * the IO of each of the included sub-modules.
     */

    val unConnSubModIns = {
      val allSubModIns = _insts.map { case (name, subMod) =>
        name -> mutable.HashSet(subMod.io.ins.elements.keys.toSeq:_*) }

      // Remove any connected sink ports
      cgra.connections.values
        .flatMap(_.sinks)
        .foreach { sink => allSubModIns(sink.parent.name) -= sink.name }
      allSubModIns
    }
    val unConnSubModOuts = {
      val allSubModOuts = _insts.map { case (name, subMod) =>
        name -> mutable.HashSet(subMod.io.outs.elements.keys.toSeq:_*) }

      // Remove any connected source ports
      cgra.connections.values
        .map(_.source)
        .foreach { source => allSubModOuts(source.parent.name) -= source.name }
      allSubModOuts
    }

    // Add these ports to the top-level
    unConnSubModIns.foreach { case (subModName, portNames) =>
      portNames.foreach { name =>
        val portName = if (subModName == conf.CGRATopLevelIOName) name else s"${subModName}_${name}"
        addInput(portName, _insts(subModName).io.ins(name).getWidth) }
    }
    unConnSubModOuts.foreach { case (subModName, portNames) =>
      portNames.foreach { name =>
        val portName = if (subModName == conf.CGRATopLevelIOName) name else s"${subModName}_${name}"
        addOutput(portName, _insts(subModName).io.outs(name).getWidth) }
    }

    // Connect the created ports accordingly
    unConnSubModIns.foreach { case (subModName, portNames) =>
      portNames.foreach { name =>
        val portName = if (subModName == conf.CGRATopLevelIOName) name else s"${subModName}_${name}"
        _insts(subModName).io.ins(name).suggestName(name) := io.ins(portName).suggestName(portName) }
    }
    unConnSubModOuts.foreach { case (subModName, portNames) =>
      portNames.foreach { name =>
        val portName = if (subModName == conf.CGRATopLevelIOName) name else s"${subModName}_${name}"
        io.outs(portName).suggestName(portName) := _insts(subModName).io.outs(name).suggestName(name) }
    }

    // Connect all ports together as specified in the module
    cgra.connections.values.foreach { conn =>
      val source = conn.source

      // Both the source and sink can only be ports of sub-modules
      val (sourceName, sourcePort) = (source.name, _insts(source.parent.name).io.outs(source.name))
      conn.sinks.foreach { sink =>
        val (sinkName, sinkPort) = (sink.name, _insts(sink.parent.name).io.ins(sink.name))
        sinkPort.suggestName(sinkName) := sourcePort.suggestName(sourceName)
      }
    }

    // Force the names of the top-level ports
    io.ins .elements.foreach { case (name, port) => forceName(port, name) }
    io.outs.elements.foreach { case (name, port) => forceName(port, name) }

    if (conf.HWDebug) println(s"[DEBUG] Finished building data path in CGRA (${cgra.name})")
  }

  /** Generate the configuration hardware within this CGRA
   * 
   * @note By default, this depends on the configuration style of the 
   *       sub-modules. Parallelly configurable designs have their ports 
   *       passed through to the top level. Serially configurable designs are 
   *       organized into a scan chain, exposing only a single entry point to 
   *       the top level. The sub-modules are in this case organized in 
   *       alphabetic order to ensure reproducibility.
   */
  override def buildConfig(): Unit = {
    if (conf.HWDebug) println(s"[DEBUG] Building configuration logic in CGRA (${cgra.name})")

    confType match {
      case ParallelConfiguration =>
        val bindings = _insts.flatMap { case (name, subMod) =>
          // Create matching ports for each configuration port in the sub-module 
          // in this module's IO and return their destination port
          subMod.ctrl.ins.elements.map { case (portName, port) =>
            val resName = s"${name}_${portName}"
            addCtrlInput(resName, port.getWidth)
            (resName, port)
          }
        }

        // Now properly bind all the ports
        noPrefix { bindings.foreach { case (portName, destination) =>
          destination := ctrl.ins(portName)
        }}

      case SerialConfiguration =>
        val chain = _insts
          .filter(_._2.ctrl.ins.getWidth > 0)
          .toIndexedSeq.sortBy(_._1)
          .map(_._2)

        if (chain.size > 0) {
          // Add the required ports to this module
          addCtrlInput("scan_en", 1)
          addCtrlInput("scan_in", 1)

          // Chain the sub-modules that require configuration bits
          chain.head.ctrl.ins("scan_in") := ctrl.ins("scan_in")
          (1 until chain.size).foreach { i =>
            chain(i).ctrl.ins("scan_in") := chain(i-1).ctrl.outs("scan_out") }
          chain.foreach(_.ctrl.ins("scan_en") := ctrl.ins("scan_en"))
        }

      case _ =>
        // should not occur
    }

    // Force the names of the top-level configuration ports
    ctrl.ins .elements.foreach { case (name, port) => forceName(port, name) }
    ctrl.outs.elements.foreach { case (name, port) => forceName(port, name) }

    if (conf.HWDebug)
      println(s"[DEBUG] Finished building configuration logic in CGRA (${cgra.name})")
  }
}
