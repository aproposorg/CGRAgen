package cgragen.mapping

import scala.collection.mutable

import cgragen.cgra.{AbstractOperation, CGRA}
import cgragen.cgra.primitives.AbstractFunctionUnit

import cgragen.dfgparse._, DFG.components

import cgragen.mrrg.MRRGNode
import cgragen.mrrg.MRRGNodeType._

import scala.collection.mutable

/** Abstract representation of a mapping from a DFG to a CGRA in MRRG 
 * representation
 * @param cgra the CGRA that the mapping should target
 * @param dfg the DFG to map to the CGRA
 * @param ii the initiation interval of this mapping
 */
final class Mapping(val cgra: CGRA, val dfg: DFG, val ii: Int)(implicit conf: cgragen.Parameters) {
  /** Fields start ***********************************************************/
  // Abstract representation of a mapping with (DFG node, MRRG node) and
  // (DFG edge, MRRG node(s)) pairs
  private val _nodeMap = mutable.HashMap.empty[DFGNode, MRRGNode]
  private val _edgeMap = mutable.HashMap.empty[DFGEdge, Seq[MRRGNode]]

  // Approximation modes assigned to the mapped MRRG nodes
  private val _approxModes = mutable.HashMap.empty[DFGNode, Int]

  // Collective occupancy by this mapping
  private var _updated   = true
  private val _occupancy = mutable.HashSet.empty[MRRGNode]
  /** Fields end *************************************************************/

  /** Accessors start ********************************************************/
  /** Return the node mapping of this mapping */
  def nodeMap = _nodeMap.toMap

  /** Return the edge mapping of this mapping */
  def edgeMap = _edgeMap.toMap

  /** Return the approximation modes assigned in this mapping */
  def approxModes = _approxModes.toMap

  /** Return the collective occupancy of this mapping */
  def occupancy = {
    if (_updated) {
      _updated = false
      _occupancy.clear()
      _occupancy ++= _nodeMap.values ++ _edgeMap.flatMap(_._2)
    }
    _occupancy.toSet
  }
  /** Accessors end **********************************************************/

  /** Modifiers start ********************************************************/
  /** Map a DFG node to an MRRG node
   * @param dfgNode the DFG node to map
   * @param mrrgNode the MRRG node to map to
   */
  def mapNode(dfgNode: DFGNode, mrrgNode: MRRGNode): Unit = {
    _updated = true
    _nodeMap(dfgNode) = mrrgNode
  }

  /** Remove the mapping of a DFG node and its associated edges
   * @param dfgNode the DFG node to unmap
   */
  def unmapNode(dfgNode: DFGNode): Unit = {
    _updated = true
    _nodeMap.remove(dfgNode)
    _edgeMap.keys
      .filter(edge => edge.src == dfgNode || edge.snk == dfgNode)
      .foreach(_edgeMap.remove(_))
  }

  /** Map a DFG edge to a list of MRRG nodes
   * @param dfgEdge the DFG edge to map
   * @param mrrgNodes the list of MRRG nodes to map to
   */
  def mapEdge(dfgEdge: DFGEdge, mrrgNodes: Seq[MRRGNode]): Unit = {
    require(mrrgNodes.forall { node =>
      node.nodeType == NodeRouting || node.nodeType == NodeRegister })
    _updated = true
    _edgeMap(dfgEdge) = mrrgNodes
  }

  /** Remove the mapping of a DFG edge
   * @param dfgEdge the DFG edge to unmap
   */
  def unmapEdge(dfgEdge: DFGEdge): Unit = {
    _updated = true
    _edgeMap.remove(dfgEdge)
  }

  /** Add an approximation mode for a mapped DFG node
   * @param dfgNode the DFG node to assign an approximation mode
   * @param mode the mode to assign
   */
  def addMode(dfgNode: DFGNode, mode: Int): Unit = {
    require(_nodeMap.contains(dfgNode) && 
      (0 <= mode && mode <= conf.CGRAApproximationModes),
      "the operation must be approximable and the mode index within range")
    if (mode > 0) {
      val fu = _nodeMap(dfgNode).parent
      require(AbstractOperation.isApproximable(dfgNode.opcode) &&
        fu.isInstanceOf[AbstractFunctionUnit] &&
        fu.asInstanceOf[AbstractFunctionUnit].approx,
        "the function unit must support approximation of the given operation")
    }
    _approxModes(dfgNode) = mode
  }

  /** Remove an approximation mode for a mapped DFG node
   * @param dfgNode the DFG node to remove an approximate mode from
   */
  def removeMode(dfgNode: DFGNode): Unit = _approxModes.remove(dfgNode)
  /** Modifiers end **********************************************************/

  /** Various other methods start ********************************************/
  /** Return whether this mapping is complete */
  def isMapped: Boolean = {
    // Check for missing nodes, edges, or fanin/fanout connections
    val missing = _edgeMap
      .filter { case (edge, mrrgNodes) =>
        !_nodeMap.contains(edge.src) ||
        !_nodeMap.contains(edge.snk) ||
        !_edgeMap.contains(edge) ||
        // Fanouts ->
        !_nodeMap(edge.src).fanout.contains(_edgeMap(edge).head) ||
        !_edgeMap(edge).last.fanout.contains(_nodeMap(edge.snk)) ||
        !_edgeMap(edge).sliding(2).forall(slc => slc.head.fanout.contains(slc.last)) ||
        // Fanins <-
        !_edgeMap(edge).head.fanin.contains(_nodeMap(edge.src)) ||
        !_nodeMap(edge.snk).fanin.contains(_edgeMap(edge).last) ||
        !_edgeMap(edge).sliding(2).forall(slc => slc.last.fanin.contains(slc.head))
      }
    missing.foreach { case (edge, _) =>
      println(s"Warning! Mapping is disconnected on edge ($edge)") }

    // Check for invalid sharing of routing nodes between multiple edges
    val duplctd = _edgeMap
      .foldLeft(Map.empty[MRRGNode, Set[DFGNode]]) { case (acc, (edge, mrrgNodes)) =>
        mrrgNodes
          .foldLeft(acc) { case (acc, mrrgNode) =>
            if (acc.contains(mrrgNode)) acc + (mrrgNode -> (acc(mrrgNode) + edge.src))
            else acc + (mrrgNode -> Set(edge.src)) }}
      .filter { case (_, dfgNodes) => dfgNodes.size > 1 }
    duplctd.foreach { case (mrrgNode, dfgNodes) =>
      val nodeStr = dfgNodes.mkString("[", ", ", "]")
      print(s"Warning! Mapping has multiple DFG nodes (${nodeStr}) mapped to ")
      println(s"MRRG node (${mrrgNode})") }

    missing.isEmpty && duplctd.isEmpty
  }

  /** Return whether this mapping is approximable */
  def isApproximable: Boolean = _nodeMap
    .exists {
      case (dfgNode, fuNode) if fuNode.parent.isInstanceOf[AbstractFunctionUnit] =>
        val opOk = AbstractOperation.isApproximable(dfgNode.opcode)
        val fuOk = fuNode.parent.asInstanceOf[AbstractFunctionUnit].approx
        opOk && fuOk
      case _ => false
    }

  /** Return this mapping in a DOT-formatted string */
  def toDotString(): String = {
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

    // Keep track of clusters in a map (prefix, Cluster). Inter-cycle edges 
    // are kept in the empty-prefix cluster
    val clusters = mutable.HashMap("" -> Cluster("", None))
    (0 until ii).foreach { cycle =>
      val clstr = Cluster(cycle.toString, Some(clusters("")))
      clusters(clstr.prefix) = clstr
      clusters("").children += clstr
    }

    // Build cluster hierarchy
    val used = occupancy
    (0 until ii).foreach { cycle =>
      used.filter(_.cycle == cycle).foreach { src =>
        val srcClstr = findOrAddCluster(src, clusters(cycle.toString))
        srcClstr.nodes += src

        // Run through the node's fanout
        src.fanout.filter(used(_)).foreach { snk =>
          val snkClstr = findOrAddCluster(snk, clusters(snk.cycle.toString))
          val commonClstr = findCommonCluster(srcClstr, snkClstr)
          if (commonClstr != None) commonClstr.get.edges += ((src, snk))
        }
      }
    }

    // Build the string and return it
    val bs = new StringBuilder("digraph {\n")
    bs ++= subClusterString(clusters(""), 1)
    bs ++= "\n}\n"
    bs.mkString
  }

  /** Return this mapping in a string */
  override def toString(): String = toDotString()
  /** Various other methods end **********************************************/
}

object Mapping {
  /** Create a copy of a mapping
   * @param src the mapping to copy
   * @return a new mapping with the same entries as `src`
   */
  def from(src: Mapping)(implicit conf: cgragen.Parameters): Mapping = {
    val ret = new Mapping(src.cgra, src.dfg, src.ii)
    src.nodeMap.foreach { case (dfgNode, mrrgNode) =>
      ret.mapNode(dfgNode, mrrgNode) }
    src.edgeMap.foreach { case (dfgEdge, mrrgNodes) =>
      ret.mapEdge(dfgEdge, mrrgNodes.toSeq) }
    src.approxModes.foreach { case (dfgNode, mode) =>
      ret.addMode(dfgNode, mode) }
    ret
  }

  /** Compute the latency of all nodes in a mapping
   * @param src the mapping to measure latency in
   * @return a map of DFG nodes to latencies in `src`
   */
  def latencies(src: Mapping): Map[DFGNode, Int] = {
    // Find the set of source nodes with no in-going edges
    val compSrcNodes = components(src.dfg)
      .map(comp => comp.nodes.filter(node => !comp.edges.exists(_.snk == node)))

    if (compSrcNodes.exists(_.isEmpty)) {
      println(s"[ERROR] Cannot compute latencies for non-DAG DFGs")
      Map.empty[DFGNode, Int]
    } else {
      compSrcNodes
        .map(_.head)
        .foldLeft(Map.empty[DFGNode, Int]) { case (acc, srcNode) =>
          // Perform a DFS from one of the source nodes
          val lclLats = mutable.HashMap.empty[DFGNode, Int]
          def _dfs(node: DFGNode, lat: Int = 0): Unit = {
            lclLats += (node -> lat)

            // Run through all out-going edges
            val outLats = src.dfg.edges
              .filter(_.src == node)
              .map { dfgEdge =>
                val routLat = src.edgeMap(dfgEdge)
                  .count(_.nodeType == NodeRegister)
                (dfgEdge.snk -> (lclLats(node) + routLat)) }
            outLats.foreach {
              case (snkNode, snkLat) if !lclLats.contains(snkNode) =>
                _dfs(snkNode, snkLat)
              case (snkNode, snkLat) => if (lclLats(snkNode) != snkLat) {
                print(s"[ERROR] Mismatching latencies (${lclLats(snkNode)})")
                println(s" and (${snkLat}) for node (${snkNode})")
                throw new Exception("mismatching latencies")
              }}

            // Run through all in-going edges
            val inLats = src.dfg.edges
              .filter(_.snk == node)
              .map { dfgEdge =>
                val routLat = src.edgeMap(dfgEdge)
                  .count(_.nodeType == NodeRegister)
                (dfgEdge.src -> (lclLats(node) - routLat)) }
            inLats.foreach {
              case (srcNode, srcLat) if !lclLats.contains(srcNode) =>
                _dfs(srcNode, srcLat)
              case (srcNode, srcLat) => if (lclLats(srcNode) != srcLat) {
                print(s"[ERROR] Mismatching latencies (${lclLats(srcNode)})")
                println(s" and (${srcLat}) for node (${srcNode})")
                throw new Exception("mismatching latencies")
              }}
          }
          _dfs(srcNode)

          // Adjust the latencies according to their minimum
          val absMin = lclLats.values.min.abs
          lclLats.keys.foreach(lclLats(_) += absMin)

          acc ++ lclLats }
    }
  }
}
