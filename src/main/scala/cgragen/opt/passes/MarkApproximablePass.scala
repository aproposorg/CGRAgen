package cgragen.opt.passes

import cgragen.cgra.CGRA

import cgragen.dfgparse.{DFG, DFGNode, isArith, isLogic}, DFG.fanInCone
import cgragen.dfgparse.Opcode.OpOutput

import cgragen.opt.Pass

import scala.collection.mutable

/** Mark DFG nodes as approximable based on their children
 * 
 * @note Considers only arithmetic and logic operations in the unique fan-in 
 *       cone of approximable output nodes with user-specified quality 
 *       constraints. Clears any approximability attributes set in other 
 *       nodes before execution.
 * 
 * @note This pass should be executed following any passes that change the 
 *       structure or content of the DFG.
 */
private[cgragen] case object MarkApproximablePass
  extends Pass(Seq.empty[Pass], Seq.empty[Pass]) {
  /** Find the set of approximable output nodes in a DFG
   * @param dfg the DFG to search through
   * @return the set of approximable output nodes in `dfg`
   */
  private def _apprxmblOuts(dfg: DFG): Seq[DFGNode] = {
    // Find all outputs marked as approximable
    val initial = dfg.nodes
      .filter(node => node.opcode == OpOutput && node.attrs.contains("approx"))
      .toSeq

    // Filter nodes without error constraints
    val invalids = initial
      .filter(_.attrs("approx").toDoubleOption == None)
    invalids.foreach { out =>
      print(s"Warning! DFG contains output node (${out.name}) marked ")
      print("approximable either without any constraints or with invalid ")
      println("constraints that will be de-marked.")
      out.removeAttr("approx")
    }

    initial.filter(!invalids.contains(_))
  }

  def apply(dfg: DFG, cgra: CGRA)(implicit conf: cgragen.Parameters): Unit = {    
    // Remove any approximability attributes in nodes other than outputs
    dfg.nodes
      .filter(node => node.opcode != OpOutput && node.attrs.contains("approx"))
      .foreach(_.removeAttr("approx"))

    // Check that the DFG has approximable outputs
    val apprxmlOuts = _apprxmblOuts(dfg)
    if (apprxmlOuts.isEmpty) {
      print("Warning! DFG contains no approximable output nodes. No more ")
      println("nodes will be marked approximable.")
      return
    }

    // Now perform this pass
    if (conf.DFGDebug) println("[DEBUG] Marking approximable nodes in DFG")

    // Keep track of marked nodes
    val markedNodes = mutable.HashSet.empty[DFGNode]

    // Find and unify the fan-in cones of the approximable outputs
    val apprxmblFIs = apprxmlOuts.foldLeft(Set.empty[DFGNode]) { case (acc, out) =>
      acc ++ fanInCone(out, dfg) }

    // Find and unify the fan-in cones of the non-approximable outputs
    val nonApprxmblFIs = dfg.nodes
      .filter(node => node.opcode == OpOutput && !apprxmlOuts.contains(node))
      .foldLeft(Set.empty[DFGNode]) { case (acc, out) =>
        acc ++ fanInCone(out, dfg) }

    // Mark the nodes that only feed approximable outputs
    (apprxmblFIs -- nonApprxmblFIs)
      .filter(node => isArith(node.opcode) || isLogic(node.opcode))
      .foreach { node =>
        node.addAttr("approx", "")
        markedNodes += node
      }

    if (conf.DFGDebug) {
      print(s"[DEBUG] Marked a total of (${markedNodes.size}) nodes as ")
      println("approximable in DFG")
    }
  }
}
