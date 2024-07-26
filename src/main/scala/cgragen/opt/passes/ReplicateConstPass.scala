package cgragen.opt.passes

import cgragen.genUniqueName

import cgragen.cgra.CGRA

import cgragen.dfgparse.{DFG, DFGEdge, DFGNode}
import cgragen.dfgparse.Opcode.OpConst

import cgragen.opt.Pass

import scala.collection.mutable

/** Replicate multi-cast constants */
private[cgragen] case object ReplicateConstPass
  extends Pass(Seq.empty[Pass], Seq.empty[Pass]) {
  /** Find replicable multi-cast constant nodes in a DFG
   * @param dfg the DFG to search through
   * @return all replicable constant nodes in `dfg`
   */
  private def _rplcblNodes(dfg: DFG): Seq[DFGNode] = {
    dfg.nodes
      .filter(node => node.opcode == OpConst && dfg.edges.count(_.src == node) > 1)
      .toSeq
  }

  def apply(dfg: DFG, cgra: CGRA)(implicit conf: cgragen.Parameters): Unit = {
    if (conf.DFGDebug) println(s"[DEBUG] Replicating multi-cast constants in DFG")

    // Replicate all the multi-cast constants and their edges
    val repNodes = mutable.ArrayBuffer.empty[DFGNode]
    _rplcblNodes(dfg).foreach { const =>
      // Find all its associated edges, except the first one
      val edges = dfg.edges.filter(_.src == const).tail

      // Remove the edges and replace them with edges to new constant nodes
      edges.zipWithIndex.foreach { case (edge, ind) =>
        dfg.removeEdge(edge)

        val replName = genUniqueName(s"${const.name}_repl${ind+1}", dfg.nodes.map(_.name))
        val replNode = new DFGNode(replName, OpConst)
        val replEdge = new DFGEdge(replNode, edge.snk, edge.operand)
        const.attrs.foreach { case (k, v) => replNode.addAttr(k, v) }

        dfg.addNode(replNode)
        dfg.addEdge(replEdge)
      }
    }

    if (conf.DFGDebug)
      print(s"[DEBUG] Replicated a total of (${repNodes.size}) constant nodes in DFG")
  }
}
