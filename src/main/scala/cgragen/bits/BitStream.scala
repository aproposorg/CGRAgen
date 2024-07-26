package cgragen.bits

import cgragen.bits.BitValue._

import cgragen.cgra.{AbstractModule, CGRA, ConfigCell, confWidth}
import cgragen.cgra.primitives._

import cgragen.dfgparse.{DFGNode, DFGEdge}

import cgragen.mapping.Mapping, Mapping.latencies

import cgragen.mrrg.MRRGNode

import chisel3.util.isPow2

import scala.collection.mutable

/** Bit-stream class used for configurations
 * @param mapping the mapping to generate a bit-stream for
 */
private[cgragen] final class BitStream(mapping: Mapping)
  (implicit conf: cgragen.Parameters) {
  /** Fields start ***********************************************************/
  private val _settings = Array.fill(mapping.ii) {
    mutable.HashMap(mapping.cgra.configOrder
      .map(cc => (cc -> Seq.empty[BitValue])):_*) }
  /** Fields end *************************************************************/

  /** Modifiers start ********************************************************/
  /** Add a configuration to the bit-stream
   * @param cycle the cycle in to target
   * @param cc the config cell to add
   * @param bits the bits to associated with the config cell
   */
  def addConfig(cycle: Int, cc: ConfigCell, bits: Seq[BitValue]): Unit = {
    require(0 <= cycle && cycle < mapping.ii,
      "configuration must belong to a non-negative cycle less than the initiation interval")
    _settings(cycle)(cc) = bits
  }
  /** Modifiers end **********************************************************/

  /** Various other methods start ********************************************/
  /** Return this bit-stream in a string */
  override def toString(): String = {
    def path(cc: ConfigCell): String = s"${cc.port.parent.fullPath}.${cc.name}"

    // The configuration order of the CGRA specifies cells in alphabetic order
    val order = mapping.cgra.configOrder

    // Get the full name paths to the config cells
    val lngstPathLen = order.map(path(_).size).max

    // Build the bit-stream as a string
    val bs = new StringBuilder(s"Bit-stream for CGRA (${mapping.cgra.name})\n")
    if (mapping.nodeMap.nonEmpty) {
      val lats = latencies(mapping)
      bs ++= s"  Node mappings and latencies:\n"
      mapping.nodeMap.foreach { case (dfgNode, fuNode) =>
        val pddName = s"${dfgNode.name} (${lats(dfgNode)})".padTo(lngstPathLen, ' ')
        bs ++= s"    ${pddName}: ${fuNode}\n"
      }
      bs ++= s"\n"
    }
    _settings.zipWithIndex.foreach {
      case (confs, cycle) if confs.nonEmpty =>
        bs ++= s"  Cycle $cycle:\n"
        order.foreach { cc =>
          val pddPath = path(cc).padTo(lngstPathLen, ' ')
          bs ++= s"    ${pddPath}: 0b${confs(cc).reverse.mkString("")}\n" }
        bs ++= "\n"
      case _ => // no action needed
    }
    bs.mkString
  }

  /** Return this bit-stream in a binary array
   * 
   * @note By default, this conversion involves the following steps:
   *       1. generating the proper order of the sub-modules and their
   *          configuration cells following the hardware flow
   *       2. concatenation of the configurations for the ordered cells in 
   *          each sub-module and from there to all cells in a cycle
   *       3. combining the configurations across all cycles
   * 
   * @note The output bit-stream is in reverse order, i.e., the configuration 
   *       bits of the first config cell are the least significant.
   */
  def toBinary(): Array[Byte] = {
    require(isPow2(conf.HWInterfaceSize) && conf.HWInterfaceSize >= 32,
      "cannot generate bit-stream for interface sizes of fewer than 32 bits")

    // The configuration order of the CGRA specifies cells in alphabetic order
    val order = mapping.cgra.configOrder.reverse
    val width = confWidth(mapping.cgra)
    val padWidth = (width + conf.HWInterfaceSize - 1) / conf.HWInterfaceSize * conf.HWInterfaceSize / 8

    // Run through the cell order and build the configuration in each cycle as 
    // BigInts for simplified conversion
    val cycleConfs = (0 until mapping.ii).map { cycle =>
      val sets = _settings(cycle)
      order.foldLeft(BigInt(0)) { case (acc, cc) =>
        val cellConf = bitValues2Int(sets.getOrElse(cc, Seq.empty[BitValue]))
        (acc << cc.dataSize) | cellConf } }

    // Convert the BigInts to arrays of bytes and concatenate them
    cycleConfs
      .foldLeft(Array.empty[Byte]) { case (acc, bi) =>
        val conf = bi.toByteArray
        acc ++ Array.fill(padWidth - conf.size)(0.toByte) ++ conf }
  }
  /** Various other methods end **********************************************/
}

object BitStream {
  /** Generate a new bit-stream from a completed mapping
   * @param mapping the source mapping
   * @return a new bit-stream configuring `mapping.cgra` with `mapping`
   */
  def apply(mapping: Mapping)(implicit conf: cgragen.Parameters): BitStream = {
    require(mapping.isMapped, "cannot generate bit-stream for incomplete mapping")
    require(mapping.occupancy.forall(_.parent != null),
      "cannot generate bit-stream for MRRG with nodes with null parents")

    // Generate array of config cells and create an empty bit-stream
    val confCells = mapping.cgra.configOrder
    val bitStream = new BitStream(mapping)

    // Run through the config cells to find their associated mapped 
    // operations or routes, if any
    confCells.foreach { cc =>
      // Get the config cell's parent module
      val ccParent = cc.port.parent
      if (ccParent == null) {
        print("[ERROR] Cannot generate bit-stream for config cell ")
        println(s"(${cc.name}) with null parent")
        throw new BitStreamGenerationException("null parent in config cell")
      }

      // Get the mapped nodes associated with the parent module for each cycle
      val ccNodeMap = mapping.nodeMap
        .filter(_._2.parent == ccParent)
      val ccEdgeMap = mapping.edgeMap
        .collect { case (dfgEdge, mrrgNodes) if mrrgNodes.exists(_.parent == ccParent) =>
          (dfgEdge -> mrrgNodes.filter(_.parent == ccParent)) }
      (0 until mapping.ii).foreach { cycle =>
        val clNodeMap = ccNodeMap.filter(_._2.cycle == cycle)
        val clEdgeMap = ccEdgeMap
          .collect { case (dfgEdge, mrrgNodes) if mrrgNodes.exists(_.cycle == cycle) =>
            (dfgEdge -> mrrgNodes.filter(_.cycle == cycle).toSeq) }

        // Generate and store a configuration from the remaining nodes
        val bits = _genConfig(ccParent, cc, clNodeMap, clEdgeMap, mapping.approxModes)
        bitStream.addConfig(cycle, cc, bits)
      }
    }

    bitStream
  }

  /** Generate a bit-configuration from a set of mapped MRRG nodes
   * @param mod the parent module to the config cell
   * @param cc the config cell to configure
   * @param opMap an operation to MRRG node mapping
   * @param edgeMap an edge to MRRG node mapping
   * @param approxMap a DFG node to approximation mode map
   * @return a sequence of bit values configuring `cc` from `opMap` and/or `edgeMap`
   */
  private def _genConfig(mod: AbstractModule, cc: ConfigCell, opMap: Map[DFGNode, MRRGNode],
    edgeMap: Map[DFGEdge, Seq[MRRGNode]], approxMap: Map[DFGNode, Int]): Seq[BitValue] = {
    mod match {
      case cst: AbstractConstantUnit     => _genConstConfig(cst, cc, opMap, edgeMap)
      case fu : AbstractFunctionUnit     => _genFUConfig(fu, cc, opMap, edgeMap, approxMap)
      case mux: AbstractMultiplexerUnit  => _genMuxConfig(mux, cc, opMap, edgeMap)
      case rf : AbstractRegisterFileUnit => _genRFConfig(rf, cc, opMap, edgeMap)
      case _ =>
        println(s"[ERROR] Module (${mod.name}) needs no configuration")
        throw new BitStreamGenerationException("no configuration needed")
    }
  }

  /** Generate a bit-configuration for a constant unit
   * @param mod the parent module to the config cell
   * @param cc the config cell to configure
   * @param opMap an operation to MRRG node mapping
   * @param edgeMap an edge to MRRG node mapping
   * @return a sequence of bit values configuring `cc` from `opMap` and/or `edgeMap`
   */
  private def _genConstConfig(mod: AbstractConstantUnit, cc: ConfigCell,
    opMap: Map[DFGNode, MRRGNode], edgeMap: Map[DFGEdge, Seq[MRRGNode]]): Seq[BitValue] = {
    if (opMap.size > 1) {
      println(s"[ERROR] Multiple constants mapped to constant unit (${mod.fullPath})")
      throw new BitStreamGenerationException("multiple constants")
    }

    // Get the mapped constant, if any
    val cst = opMap.keys.headOption.map(_.attrs("value").toInt).getOrElse(0)
    int2BitValues(cst, cc.dataSize)
  }

  /** Generate a bit-configuration for a function unit
   * @param mod the parent module to the config cell
   * @param cc the config cell to configure
   * @param opMap an operation to MRRG node mapping
   * @param edgeMap an edge to MRRG node mapping
   * @param approxMap a DFG node to approximation mode map
   * @return a sequence of bit values configuring `cc` from `opMap` and/or `edgeMap`
   */
  private def _genFUConfig(mod: AbstractFunctionUnit, cc: ConfigCell, opMap: Map[DFGNode, MRRGNode],
    edgeMap: Map[DFGEdge, Seq[MRRGNode]], approxMap: Map[DFGNode, Int]): Seq[BitValue] = {
    if (opMap.size > 1) {
      println(s"[ERROR] Multiple operations mapped to function unit (${mod.fullPath})")
      throw new BitStreamGenerationException("multiple operations")
    }

    // The config cell determines operation or approximation mode
    if (cc.name.endsWith("mode")) {
      val mode = opMap.keys.headOption
        .map(dfgNode => approxMap.getOrElse(dfgNode, 0))
        .getOrElse(0)
      int2BitValues(mode, cc.dataSize)
    } else {
      val ind = opMap.keys.headOption
        .map(dfgNode => mod.operations.map(_.op).indexOf(dfgNode.opcode))
        .getOrElse(0)
      int2BitValues(ind, cc.dataSize)
    }
  }

  /** Generate a bit-configuration for a multiplexer unit
   * @param mod the parent module to the config cell
   * @param cc the config cell to configure
   * @param opMap an operation to MRRG node mapping
   * @param edgeMap an edge to MRRG node mapping
   * @return a sequence of bit values configuring `cc` from `opMap` and/or `edgeMap`
   */
  private def _genMuxConfig(mod: AbstractMultiplexerUnit, cc: ConfigCell,
    opMap: Map[DFGNode, MRRGNode], edgeMap: Map[DFGEdge, Seq[MRRGNode]]): Seq[BitValue] = {
    if (edgeMap.keys.map(_.src).toSet.size > 1) {
      println(s"[ERROR] Multiple edges mapped to multiplexer (${mod.fullPath})")
      throw new BitStreamGenerationException("multiple edges")
    }

    // Get the mapped route(s') input port in use, if any
    val inputRegex = "[a-zA-Z0-9_.]*.in([0-9]+)$".r
    val inps = edgeMap.values
      .flatMap { route =>
        route.collect(_.name match { case inputRegex(id) => id.toInt }) }
    if (inps.toSet.size > 1) {
      println(s"[ERROR] Multiple inputs active in multiplexer (${mod.fullPath})")
      throw new BitStreamGenerationException("multiple active input")
    }

    int2BitValues(inps.headOption.getOrElse(0), cc.dataSize)
  }

  /** Generate a bit-configuration for a register file unit
   * @param mod the parent module to the config cell
   * @param cc the config cell to configure
   * @param opMap an operation to MRRG node mapping
   * @param edgeMap an edge to MRRG node mapping
   * @return a sequence of bit values configuring `cc` from `opMap` and/or `edgeMap`
   */
  private def _genRFConfig(mod: AbstractRegisterFileUnit, cc: ConfigCell,
    opMap: Map[DFGNode, MRRGNode], edgeMap: Map[DFGEdge, Seq[MRRGNode]]): Seq[BitValue] = {
    // Get the configuration by identifying which inputs, registers, and 
    // outs are in use, if any
    val inputRegex  = "[a-zA-Z0-9_.:]*.in([0-9]+)$".r
    val regRegex    = "[a-zA-Z0-9_.:]*.reg([0-9]+)$".r
    val regSelRegex = "[a-zA-Z0-9_.:]*.out([0-9]+)_sel([0-9]+)$".r
    val outputRegex = "[a-zA-Z0-9_.:]*.out([0-9]+)$".r
    val (inps, inpsToRegInds, outpsToRegInds) = {
      val insUsed       = mutable.HashSet.empty[Int]
      val insToRegInds  = mutable.HashMap.empty[Int, Int]
      val outsToRegInds = mutable.HashMap.empty[Int, Int]

      edgeMap.foreach { case (_, route) =>
        // There will always be a register index found here because either input 
        // or output config cells have MRRG nodes which identify it
        val regId = route
          .collect(_.name match { case regRegex(id) => id.toInt })
          .headOption
          .getOrElse(route
            .collect(_.name match { case regSelRegex(_, id) => id.toInt })
            .head)
        val inputs = route
          .collect(_.name match { case inputRegex(id) => id.toInt })
        val outputs = route
          .collect(_.name match { case outputRegex(id) => id.toInt })

        // Check that no register or output has multiple fanins
        if (inputs.exists(in => insToRegInds.contains(in))) {
          println(s"[ERROR] Register file (${mod.fullPath}) has multiply driven register")
          throw new BitStreamGenerationException("multiple fanin")
        }
        if (outputs.exists(out => outsToRegInds.contains(out))) {
          println(s"[ERROR] Register file (${mod.fullPath}) has multiply driven output")
          throw new BitStreamGenerationException("multiple fanin")
        }

        insUsed ++= inputs
        inputs .foreach(in  => insToRegInds  += (in  -> regId))
        outputs.foreach(out => outsToRegInds += (out -> regId))
      }

      (insUsed.toSet, insToRegInds.toMap, outsToRegInds.toMap)
    }

    // Generate the configuration depending on the config cell's name
    val weRegex      = "WE([0-9]+)$".r
    val addrInRegex  = "addr_in([0-9]+)$".r
    val addrOutRegex = "addr_out([0-9]+)$".r
    cc.port.name match {
      case weRegex(id) =>
        if (inps(id.toInt)) Seq(BitHigh) else Seq(BitLow)
      case addrInRegex(id) =>
        int2BitValues(inpsToRegInds.getOrElse(id.toInt, 0), cc.dataSize)
      case addrOutRegex(id) =>
        int2BitValues(outpsToRegInds.getOrElse(id.toInt, 0), cc.dataSize)
      case _ =>
        println(s"[ERROR] cannot determine configuration for (${cc.name})")
        throw new BitStreamGenerationException("invalid config cell")
    }
  }
}
