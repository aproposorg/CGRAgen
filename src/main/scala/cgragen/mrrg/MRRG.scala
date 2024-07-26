package cgragen.mrrg

import cgragen.cgra.{AbstractBaseModule, AbstractModule, CGRA}
import cgragen.cgra.ModuleType._
import cgragen.cgra.primitives._

import cgragen.mrrg.MRRGNodeType._

import scala.collection.mutable

/** MRRG representation
 * @param ii the initiation interval
 */
final class MRRG(val ii: Int)(implicit conf: cgragen.Parameters) {
  require(ii > 0, "initiation interval must be positive")
  /** Fields start ***********************************************************/
  // Nodes in the graph
  val nodes = Array.fill(ii) { mutable.HashMap.empty[String, MRRGNode] }

  // Collective statistics about this MRRG
  private var _updated = true
  private var _nodeCnt = 0
  private var _edgeCnt = 0
  /** Fields end *************************************************************/

  /** Accessors start ********************************************************/
  /** Return the number of nodes in this MRRG */
  def nodeCnt = {
    if (_updated) _update()
    _nodeCnt
  }

  /** Return the number of edges in this MRRG */
  def edgeCnt = {
    if (_updated) _update()
    _edgeCnt
  }
  /** Accessors end **********************************************************/

  /** Modifiers start ********************************************************/
  /** Add a node to the graph
   * @param node the node to add
   */
  def addNode(node: MRRGNode): Unit = {
    require(0 <= node.cycle && node.cycle < ii,
      "cycle must be non-negative and less than the initiation interval")
    _updated = true
    nodes(node.cycle)(node.name) = node
  }
  /** Modifiers end **********************************************************/

  /** Various other methods start ********************************************/
  /** Update the collective statistics of this MRRG */
  private def _update(): Unit = {
    _updated = false
    _nodeCnt = nodes.map(_.size).sum
    _edgeCnt = nodes.map(_.values.map(_.fanout.size).sum).sum
  }

  /** Return the graph in a clustered DOT-formatted string */
  def toDotString() = {
    // For internal use, clusters are tracked in a hierarchical graph-like 
    // structure of case classes to avoid string comparisons
    case class Cluster(prefix: String, parent: Option[Cluster]) {
      val children = mutable.ArrayBuffer.empty[Cluster]
      val nodes    = mutable.ArrayBuffer.empty[MRRGNode]
      val edges    = mutable.ArrayBuffer.empty[(MRRGNode, MRRGNode)]

      override def hashCode(): Int = prefix.hashCode
    }

    // Gets the prefix of a node's name to the last '.' or ':'
    def prefix(name: String): String = name.dropRight(name.split(Array('.', ':')).last.length + 1)

    // Keep track of clusters in a map (prefix, Cluster). Inter-cycle edges 
    // are kept in the empty-prefix cluster
    val clusters = mutable.HashMap("" -> Cluster("", None))
    (0 until ii).foreach { cycle =>
      val clstr = Cluster(cycle.toString, Some(clusters("")))
      clusters(cycle.toString) = clstr
      clusters("").children   += clstr
    }

    // Find the cluster of a particular node, or extend the map of clusters 
    // if no such cluster exists
    def findOrAddCluster(node: MRRGNode, current: Cluster): Cluster = {
      require(node.toString().startsWith(current.prefix))
      val pfx = prefix(node.toString())
      // If the prefix is found, return the current cluster
      if (current.prefix == pfx) {
        current
      } else {
        // Otherwise, search child clusters or add new clusters as needed
        val subs = current.children.filter(clstr => pfx.startsWith(clstr.prefix))
        assert(subs.length <= 1)
        if (subs.isEmpty) {
          val pfxExt = pfx.drop(current.prefix.length + 1).split('.').head
          val pfxSep = if (current.prefix.contains(':')) "." else ":"
          val clstr  = Cluster(s"${current.prefix}$pfxSep$pfxExt", Some(current))
          current.children += clstr
          findOrAddCluster(node, clstr)
        } else {
          findOrAddCluster(node, subs.head)
        }
      }
    }

    // Find the first common sub-cluster given two clusters, if any
    def findCommonCluster(cl1: Cluster, cl2: Cluster): Option[Cluster] = {
      // If the two match, return one of them
      if (cl1.prefix == cl2.prefix) {
        Some(cl1)
      } else {
        // Otherwise, search in both directions to find first match
        val left  = if (cl1.parent != None) findCommonCluster(cl1.parent.get, cl2) else None
        val right = if (cl2.parent != None) findCommonCluster(cl1, cl2.parent.get) else None
        (left, right) match {
          case (None,    None)    => None
          case (Some(_), None)    => left
          case (None,    Some(_)) => right
          case (Some(x), Some(y)) => if (x.prefix.length > y.prefix.length) left else right
        }
      }
    }

    // Build cluster hierarchy 
    nodes.zipWithIndex.foreach { case (nodeMap, cycle) =>
      // Add all the nodes and their edges to the clusters
      nodeMap.values.foreach { src =>
        val srcClstr = findOrAddCluster(src, clusters(cycle.toString))
        srcClstr.nodes += src

        // Run through the node's fanout
        src.fanout.foreach { snk =>
          val snkClstr = findOrAddCluster(snk, clusters(snk.cycle.toString))
          val commonClstr = findCommonCluster(srcClstr, snkClstr)
          if (commonClstr != None) commonClstr.get.edges += ((src, snk))
        }
      }
    }

    // Recursively build sub-clusters and return them in a string given the name 
    // of the current cluster being operated on
    def subClusterString(current: Cluster, ind: Int): String = {
      require(ind >= 0, "number of double spaces to indent by must be non-negative")
      // Recursively add all sub-clusters, if any
      val indent  = "  " * ind
      val clsName = if (current.prefix == "") "top" else current.prefix
      val label   = if (current.prefix == "") "top" else current.prefix.split(Array('.', ':')).last
      val bs = new StringBuilder(s"${indent}subgraph \"cluster_$clsName\" {\n")
      bs ++= s"${indent}  label=\"$label\";\n"

      if (!current.children.isEmpty) {
        bs ++= current.children
          .map(clstr => subClusterString(clstr, ind + 1))
          .mkString("\n")
        bs ++= "\n"
      }

      if (!current.edges.isEmpty) {
        bs ++= current.edges
          .map { case (src, snk) =>
            val lbl = if (src.cycle < snk.cycle) " [color=\"blue\"]"
              else if (src.cycle > snk.cycle) " [color=\"red\"]"
              else ""
            s"\"$src\"->\"$snk\"${lbl};" }
          .mkString(s"$indent  ", s"\n$indent  ", "")
        bs ++= "\n"
      }

      bs ++= s"$indent}"
      bs.mkString
    }

    // Build the string and return it
    val bs = new StringBuilder("digraph {\n")
    bs ++= subClusterString(clusters(""), 1)
    bs ++= "\n}\n"
    bs.mkString
  }

  /** Return this graph in a string */
  override def toString(): String = toDotString()
  /** Various other methods end **********************************************/
}

private[cgragen] object MRRG {
  /** Generate the MRRG representation for a given module with a given 
   * initiation interval
   * @param mod the module to generate an MRRG for
   * @param ii the targeted initiation interval
   * @return an MRRG representation of this module
   */
  def apply[T <: AbstractModule](mod: T, ii: Int)
    (implicit conf: cgragen.Parameters) = mod match {
    case cgra: CGRA =>
      _genModMRRG(cgra.asInstanceOf[AbstractModule], ii)
    case base: AbstractBaseModule =>
      _genModMRRG(base, ii)
    case const: AbstractConstantUnit => 
      _genConstMRRG(const.asInstanceOf[AbstractConstantUnit], ii)
    case fu: AbstractFunctionUnit =>
      _genFUMRRG(fu.asInstanceOf[AbstractFunctionUnit], ii)
    case mux: AbstractMultiplexerUnit =>
      _genMuxMRRG(mux.asInstanceOf[AbstractMultiplexerUnit], ii)
    case reg: AbstractRegisterUnit =>
      _genRegMRRG(reg.asInstanceOf[AbstractRegisterUnit], ii)
    case rf: AbstractRegisterFileUnit =>
      _genRFMRRG(rf.asInstanceOf[AbstractRegisterFileUnit], ii)
    case in: AbstractInputUnit =>
      _genInputMRRG(in.asInstanceOf[AbstractInputUnit], ii)
    case out: AbstractOutputUnit =>
      _genOutputMRRG(out.asInstanceOf[AbstractOutputUnit], ii)
    case _ =>
      println(s"[ERROR] Cannot generate MRRG for modules of type (${mod.getClass()})")
      throw new MRRGGenerationException(s"unsupported module type ${mod.getClass()}")
  }

  /** Generate a top-level MRRG representation for an [[AbstractModule]]
   * @param mod the module to generate an MRRG for
   * @param ii the targeted initiation interval
   * @return an MRRG representation of `mod`
   * @note Returns an empty graph for non-composite modules.
   */
  private[MRRG] def _genModMRRG(mod: AbstractModule, ii: Int)(implicit conf: cgragen.Parameters): MRRG = {
    if (conf.MRRGDebug) println(s"[DEBUG] Generating MRRG representation of module (${mod.name})")
    val mrrg = new MRRG(ii)

    // If this module is not a composite, don't translate it
    if (mod.modType != ModComposite) {
      return mrrg
    }

    // Otherwise, generate MRRGs for all sub-modules in this module and merge 
    // them into a single graph
    val subMRRGs = mutable.HashMap.from(mod.subModules.map { case (name, subMod) => 
      (name, MRRG(subMod, ii)) 
    })

    // First, add the ports
    mod.ports.foreach { case (name, _) =>
      // For each cycle, add the required in nodes
      (0 until ii).foreach { cycle =>
        mrrg.addNode(MRRGNode(name, cycle, mod, essential = true))
      }
    }

    // Next, add all the sub-modules
    subMRRGs.foreach { case (modName, modMrrg) =>
      modMrrg.nodes.foreach { nodeMap =>
        nodeMap.foreach { case (nodeName, node) =>
          // Update the node's name and add it to the graph
          node.name = s"$modName.$nodeName"
          mrrg.addNode(node)
        }
      }
    }

    // And finally, add connections between ports and sub-module ports
    mod.connections.foreach { case (srcPort, conn) =>
      // Find the source module and port names
      val (srcModName, srcPortName) = (srcPort.parent.name, srcPort.name)

      // Connect the port to all its destinations
      conn.sinks.foreach { snkPort =>
        // Find the destination module and port names
        val (snkModName, snkPortName) = (snkPort.parent.name, snkPort.name)

        // Add connections in all cycles
        (0 until ii).foreach { cycle =>
          val srcNode = if (srcPort.parent == mod) {
            mrrg.nodes(cycle)(srcPortName)
          } else {
            subMRRGs(srcModName).nodes(cycle)(srcPortName)
          }
          val snkNode = if (snkPort.parent == mod) {
            mrrg.nodes(cycle)(snkPortName)
          } else {
            subMRRGs(snkModName).nodes(cycle)(snkPortName)
          }
          link(srcNode, snkNode)
        }
      }
    }

    mrrg
  }

  /** Generate the MRRG representation for an [[AbstractConstantUnit]]
   * @param mod the module to generate an MRRG for
   * @param ii the targeted initiation interval
   * @return an MRRG representation of `mod`
   */
  private[MRRG] def _genConstMRRG(mod: AbstractConstantUnit, ii: Int)(implicit conf: cgragen.Parameters) = {
    if (conf.MRRGDebug) println(s"[DEBUG] Generating MRRG representation of constant unit (${mod.name})")
    val mrrg = new MRRG(ii)

    // For each cycle, add the required nodes
    (0 until ii).foreach { cycle =>
      // Create the relevant constant and out nodes
      val const = MRRGNode("const", cycle, mod, NodeFunction, essential = true)
      val out   = MRRGNode("out", cycle, mod)

      // Add the nodes to the result
      mrrg.addNode(const)
      mrrg.addNode(out)

      // Connect the nodes
      link(const, out)

      // Add the supported operation to the const node
      const.addMode(cgragen.dfgparse.Opcode.OpConst)
    }

    mrrg
  }

  /** Generate the MRRG representation for an [[AbstractFunctionUnit]]
   * @param mod the module to generate an MRRG for
   * @param ii the targeted initiation interval
   * @return an MRRG representation of `mod`
   */
  private[MRRG] def _genFUMRRG(mod: AbstractFunctionUnit, ii: Int)(implicit conf: cgragen.Parameters) = {
    if (conf.MRRGDebug) println(s"[DEBUG] Generating MRRG representation of function unit (${mod.name})")
    val mrrg = new MRRG(ii)

    // Make sure all the given IIs and latencies are less than the II 
    // targeted for mapping
    if (!mod.operations.forall(_.ii <= ii) || !mod.operations.forall(_.lat < ii)) {
      print("[ERROR] Function units cannot have operations whose II or ")
      println("latency is longer than that targeted for mapping")
      throw new MRRGGenerationException("function unit IIs or latencies")
    }

    // For each cycle, add the required nodes
    (0 until ii).foreach { cycle =>
      // Create the relevant common in and out nodes
      val inA = MRRGNode("in_a", cycle, mod, essential = true)
      val inB = MRRGNode("in_b", cycle, mod, essential = true)
      val sel = MRRGNode("sel", cycle, mod, essential = true)
      val out = MRRGNode("out", cycle, mod, essential = true)

      // Add the nodes to the result
      mrrg.addNode(inA)
      mrrg.addNode(inB)
      mrrg.addNode(sel)
      mrrg.addNode(out)
      link(sel, out)

      // Create the relevant nodes for each mode supported by the function unit. 
      // Function nodes are added every II cycles
      mod.operations
        .filter(oprtn => (cycle % oprtn.ii) == 0)
        .foreach { oprtn =>
          // First, extract some information about this operation
          val modCycle = (cycle + oprtn.lat) % ii

          // Next, create the relevant nodes
          val opFu = MRRGNode(s"fu_${oprtn.op}", cycle, mod, NodeFunction, essential = true)

          // Add the nodes to the result
          mrrg.addNode(opFu)

          // Connect the nodes
          link(inA, opFu)
          link(inB, opFu)

          // Add the supported operation to the function node
          opFu.addMode(oprtn.op) }
    }

    // Connect the nodes across cycle boundaries
    (0 until ii).foreach { cycle =>
      // Connect mode function unit nodes to their relative output nodes
      mod.operations
        .zipWithIndex
        .filter { case (oprtn, _) => (cycle % oprtn.ii) == 0 }
        .foreach { case (oprtn, ind) =>
          // First, extract some information about this operation
          val modCycle = (cycle + oprtn.lat) % ii

          // Next, fetch the relevant nodes and connect them
          val opFu = mrrg.nodes(cycle)(s"fu_${oprtn.op}")
          val sel  = mrrg.nodes(modCycle)("sel")
          link(opFu, sel) }
    }

    mrrg
  }

  /** Generate the MRRG representation for an [[AbstractMultiplexerUnit]]
   * @param mod the module to generate an MRRG for
   * @param ii the targeted initiation interval
   * @return an MRRG representation of `mod`
   */
  private[MRRG] def _genMuxMRRG(mod: AbstractMultiplexerUnit, ii: Int)(implicit conf: cgragen.Parameters) = {
    if (conf.MRRGDebug) println(s"[DEBUG] Generating MRRG representation of multiplexer (${mod.name})")
    val mrrg = new MRRG(ii)

    // For each cycle, add the required nodes
    (0 until ii).foreach { cycle =>
      // Create the relevant in, mux and out nodes
      val ins = (0 until mod.muxSize).map { i =>
        MRRGNode(s"in$i", cycle, mod, essential = true)
      }
      val mux = MRRGNode("mux", cycle, mod, essential = true)
      val out = MRRGNode("out", cycle, mod, essential = true)

      // Add the nodes to the result
      ins.foreach(mrrg.addNode(_))
      mrrg.addNode(mux)
      mrrg.addNode(out)

      // Connect the nodes
      ins.foreach(link(_, mux))
      link(mux, out)
    }
    
    mrrg
  }

  /** Generate the MRRG representation for an [[AbstractRegisterUnit]]
   * @param mod the module to generate an MRRG for
   * @param ii the targeted initiation interval
   * @return an MRRG representation of `mod`
   */
  private[MRRG] def _genRegMRRG(mod: AbstractRegisterUnit, ii: Int)(implicit conf: cgragen.Parameters) = {
    if (conf.MRRGDebug) println(s"[DEBUG] Generating MRRG representation of register (${mod.name})")
    val mrrg = new MRRG(ii)

    // For each cycle, add the required nodes
    (0 until ii).foreach { cycle =>
      val modCycle = (cycle + 1) % ii

      // Create the relevant in, reg and out nodes
      val in  = MRRGNode("in", cycle, mod)
      val reg = MRRGNode("reg", cycle, mod, NodeRegister, essential = true)
      val out = MRRGNode("out", modCycle, mod, essential = true)

      // Add the nodes to the result
      mrrg.addNode(in)
      mrrg.addNode(reg)
      mrrg.addNode(out)
    }

    // Connect the nodes across cycle boundaries
    (0 until ii).foreach { cycle =>
      val modCycle = (cycle + 1) % ii

      // First, fetch the relevant nodes
      val in  = mrrg.nodes(cycle)("in")
      val reg = mrrg.nodes(cycle)("reg")
      val out = mrrg.nodes(modCycle)("out")

      // Next, connect the nodes accordingly
      link(in, reg)
      link(reg, out)
    }

    mrrg
  }

  /** Generate the MRRG representation for an [[AbstractRegisterFileUnit]]
   * @param mod the module to generate an MRRG for
   * @param ii the targeted initiation interval
   * @return an MRRG representation of `mod`
   */
  private[MRRG] def _genRFMRRG(mod: AbstractRegisterFileUnit, ii: Int)(implicit conf: cgragen.Parameters) = {
    if (conf.MRRGDebug) println(s"[DEBUG] Generating MRRG representation of register file (${mod.name})")
    val mrrg = new MRRG(ii)

    // For each cycle, add the required nodes
    (0 until ii).foreach { cycle =>
      // Create the relevant in nodes
      (0 until mod.numInputs).foreach { input =>
        mrrg.addNode(MRRGNode(s"in$input", cycle, mod, essential = true))
      }

      // Create the relevant reg nodes and intermediate selection nodes
      (0 until (1 << mod.numRegsLg2)).foreach { register =>
        mrrg.addNode(MRRGNode(s"reg$register", cycle, mod, NodeRegister, essential = true))
        (0 until mod.numInputs).foreach { input =>
          mrrg.addNode(MRRGNode(s"reg${register}_sel$input", cycle, mod, essential = true))
        }
      }

      // Create the relevant out nodes and intermediate selection nodes
      (0 until mod.numOutputs).foreach { output =>
        mrrg.addNode(MRRGNode(s"out$output", cycle, mod, essential = true))
        (0 until (1 << mod.numRegsLg2)).foreach { register =>
          mrrg.addNode(MRRGNode(s"out${output}_sel$register", cycle, mod, essential = true))
        }
      }
    }

    // Connect the nodes (has to be split here to ensure no invalid lookups)
    (0 until ii).foreach { cycle =>
      // Connect the inputs to the selection nodes
      (0 until mod.numInputs).foreach { input =>
        val in = mrrg.nodes(cycle)(s"in$input")
        (0 until (1 << mod.numRegsLg2)).foreach { register =>
          val regSel = mrrg.nodes(cycle)(s"reg${register}_sel$input")
          link(in, regSel)
        }
      }

      // Connect the register select nodes with the register nodes
      (0 until (1 << mod.numRegsLg2)).foreach { register =>
        val reg = mrrg.nodes(cycle)(s"reg$register")
        (0 until mod.numInputs).foreach { input =>
          val regSel = mrrg.nodes(cycle)(s"reg${register}_sel$input")
          link(regSel, reg)
        }
      }

      // Connect the registers to their respective register select nodes and 
      // output select nodes in the next cycle, if any
      val modCycle = (cycle + 1) % ii
      (0 until (1 << mod.numRegsLg2)).foreach { register =>
        val reg = mrrg.nodes(cycle)(s"reg$register")
        (0 until mod.numOutputs).foreach { output =>
          val outSel = mrrg.nodes(modCycle)(s"out${output}_sel$register")
          link(reg, outSel)
        }
        if (ii != 1) {
          val regNext = mrrg.nodes(modCycle)(s"reg${register}")
          link(reg, regNext)
        }
      }

      // Connect the output select nodes with the output nodes
      (0 until mod.numOutputs).foreach { output =>
        val out    = mrrg.nodes(cycle)(s"out$output")
        (0 until (1 << mod.numRegsLg2)).foreach { register =>
          val outSel = mrrg.nodes(cycle)(s"out${output}_sel$register")
          link(outSel, out)
        }
      }
    }

    mrrg
  }

  /** Generate the MRRG representation for [[AbstractInputUnit]]
   * @param mod the module to generate an MRRG for
   * @param ii the targeted initiation interval
   * @return an MRRG representation of `mod`
   */
  private[MRRG] def _genInputMRRG(mod: AbstractInputUnit, ii: Int)(implicit conf: cgragen.Parameters) = {
    if (conf.MRRGDebug) println(s"[DEBUG] Generating MRRG representation of input (${mod.name})")
    val mrrg = new MRRG(ii)

    // For each cycle, add the required nodes
    (0 until ii).foreach { cycle =>
      // Create the relevant input and out nodes
      val input = MRRGNode("input", cycle, mod, NodeFunction, essential = true)
      val out   = MRRGNode("out", cycle, mod)

      // Add the nodes to the result
      mrrg.addNode(input)
      mrrg.addNode(out)

      // Connect the nodes
      link(input, out)

      // Add the supported operation to the input node
      input.addMode(cgragen.dfgparse.Opcode.OpInput)
    }

    mrrg
  }

  /** Generate the MRRG representation for [[AbstractOutputUnit]]
   * @param mod the module to generate an MRRG for
   * @param ii the targeted initiation interval
   * @return an MRRG representation of `mod`
   */
  private[MRRG] def _genOutputMRRG(mod: AbstractOutputUnit, ii: Int)(implicit conf: cgragen.Parameters) = {
    if (conf.MRRGDebug) println(s"[DEBUG] Generating MRRG representation of input (${mod.name})")
    val mrrg = new MRRG(ii)

    // For each cycle, add the required nodes
    (0 until ii).foreach { cycle =>
      // Create the relevant in and output nodes
      val in     = MRRGNode("in", cycle, mod)
      val output = MRRGNode("output", cycle, mod, NodeFunction, essential = true)

      // Add the nodes to the result
      mrrg.addNode(in)
      mrrg.addNode(output)

      // Connect the nodes
      link(in, output)

      // Add the supported operation to the output node
      output.addMode(cgragen.dfgparse.Opcode.OpOutput)
    }

    mrrg
  }
}
