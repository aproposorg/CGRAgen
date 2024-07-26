package cgragen.opt.passes

import cgragen.genUniqueName

import cgragen.cgra.CGRA

import cgragen.dfgparse.{DFG, DFGNode, DFGEdge}
import cgragen.dfgparse.Opcode.OpInput

import cgragen.opt.Pass

import scala.collection.mutable

/** Duplicate DFG nodes to limit the maximum fanout
 * 
 * @note Duplications are so far limited to all internal graph nodes and,
 *       therefore, does not consider input nodes.
 */
private[cgragen] case object MaxFanoutPass
  extends Pass(Seq.empty[Pass], Seq.empty[Pass]) {
  def apply(dfg: DFG, cgra: CGRA)(implicit conf: cgragen.Parameters): Unit = {
    if (conf.DFGDebug) println(s"[DEBUG] Limiting fanout in DFG to ${conf.DFGMaxFanout}")

    // Check whether the DFG has a node with too high fanout
    def _exceedsFanOut(dfg: DFG): Boolean = {
      dfg.nodes
        .filter(_.opcode != OpInput)
        .foldLeft(false) { case (acc, src) =>
          acc || dfg.edges.filter(_.src == src).size > conf.DFGMaxFanout }
    }

    // Create a map of fanouts from each node in a DFG
    def _maxFanOutNodeEdges(dfg: DFG): (DFGNode, Seq[DFGEdge]) = {
      val nodeEdges = dfg.nodes
        .filter(_.opcode != OpInput)
        .foldLeft(Map.empty[DFGNode, Seq[DFGEdge]]) { case (acc, src) =>
          acc + (src -> dfg.edges.filter(_.src == src).toSeq) }
      nodeEdges.maxBy(_._2.size)
    }

    // Keep track of inserted nodes and edges
    val createdNodes = mutable.ArrayBuffer.empty[DFGNode]
    val createdEdges = mutable.ArrayBuffer.empty[DFGEdge]

    while (_exceedsFanOut(dfg)) {
      // Pick the node with the highest fanout
      val (node, edges) = _maxFanOutNodeEdges(dfg)

      // Figure out the number of duplicates to create
      val nDups    = ((edges.size + (conf.DFGMaxFanout - 1)) / conf.DFGMaxFanout) - 1
      val dupNodes = (0 until nDups).map { i =>
        val dupName = genUniqueName(s"${node.name}_fo$i", dfg.nodes.map(_.name))
        val dup = new DFGNode(dupName, node.opcode)
        node.attrs.foreach { case (key, value) => dup.addAttr(key, value) }
        dup }
      createdNodes ++= dupNodes

      // Insert the additional nodes and add edges to them
      dupNodes.foreach(dfg.addNode(_))
      dfg.edges
        .filter(_.snk == node)
        .foreach { edge =>
          dupNodes.foreach { dup =>
            val dupEdge   = new DFGEdge(edge.src, dup, edge.operand)
            createdEdges += dupEdge
            dfg.addEdge(dupEdge) }
        }

      // Now split the fan-out edges between the duplicated nodes
      val replEdges = edges.drop(conf.DFGMaxFanout)
      replEdges.foreach(dfg.removeEdge(_))
      replEdges
        .grouped(conf.DFGMaxFanout)
        .zip(dupNodes)
        .foreach { case (grpEdges, dup) => 
          grpEdges.foreach { edge =>
            val dupEdge   = new DFGEdge(dup, edge.snk, edge.operand)
            createdEdges += dupEdge
            dfg.addEdge(dupEdge) }
        }
    }

    if (conf.DFGDebug) {
      print(s"[DEBUG] Added a total of (${createdNodes.size}) nodes and ")
      println(s"(${createdEdges.size}) edges to DFG")
    }
  }
}
