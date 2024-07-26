package cgragen.opt.passes

import cgragen.cgra.CGRA

import cgragen.dfgparse.DFG, DFG.fanInCone
import cgragen.dfgparse.Opcode.OpOutput

import cgragen.opt.Pass

/** Remove dead nodes from a DFG
 * 
 * @note A node is considered dead if it does not contribute to any output 
 *       node in the DFG. Activating this optimization on DFGs with no 
 *       output nodes will result in removal of all nodes and edges.
 */
private[cgragen] case object RemoveDeadPass
  extends Pass(Seq.empty[Pass], Seq.empty[Pass]) {
  def apply(dfg: DFG, cgra: CGRA)(implicit conf: cgragen.Parameters): Unit = {
    if (conf.DFGDebug) println("[DEBUG] Removing dead nodes from DFG")

    // Search the DFG backwards from its outputs to identify unused nodes
    val visited = dfg.nodes
      .filter(_.opcode == OpOutput)
      .flatMap(node => fanInCone(node, dfg))
      .toSet

    // Remove any nodes not discovered in the search
    val deadNodes = dfg.nodes.filterNot(visited(_))
    deadNodes.foreach(dfg.removeNode(_))

    val deadEdges = dfg.edges
      .filter(edge => deadNodes.contains(edge.src) || deadNodes.contains(edge.snk))
    deadEdges.foreach(dfg.removeEdge(_))

    if (conf.DFGDebug) {
      print(s"[DEBUG] Removed a total of (${deadNodes.size}) dead nodes ")
      println(s"and (${deadEdges.size}) edges from DFG")
    }
  }
}
