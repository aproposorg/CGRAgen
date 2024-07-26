package cgragen.opt.passes

import cgragen.cgra.CGRA

import cgragen.dfgparse.{DFG, DFGNode}, DFG.{components, isConnected}
import cgragen.dfgparse.Opcode.{OpInput, OpOutput}

import cgragen.opt.Pass

/** Check the integrity of a DFG following other passes
 * 
 * @note For now, this pass only warns on the following conditions:
 *       - empty graphs
 *       - disconnected graphs
 *       - lack of input or output nodes
 */
private[cgragen] case object IntegrityCheckPass
  extends Pass(Seq.empty[Pass], Seq.empty[Pass]) {
  def apply(dfg: DFG, cgra: CGRA)(implicit conf: cgragen.Parameters): Unit = {
    val cmpnts = components(dfg)

    // Warn the user about empty DFGs
    if (cmpnts.isEmpty) {
      print("Warning! DFG is empty. Mapping will trivially succeed without ")
      println("any useful outputs. Check your DFG and configuration settings.")
    }

    // Warn the user about disconnected DFGs
    if (cmpnts.size > 1) {
      println(s"Warning! DFG is disconnected into ${cmpnts.size} components:")
      cmpnts.foreach { subdfg =>
        val nodeStr = if (subdfg.nodes.size <= 3)
          subdfg.nodes.mkString("[", ", ", "]")
        else
          s"[${subdfg.nodes.take(3).mkString(", ")}, ...]"
        println(s" - Component ${subdfg.name}: $nodeStr")
      }
    }

    // Warn the user about missing input or output nodes
    def hasInput (dfg: DFG): Boolean = dfg.nodes.filter(_.opcode == OpInput ).nonEmpty
    def hasOutput(dfg: DFG): Boolean = dfg.nodes.filter(_.opcode == OpOutput).nonEmpty
    if (cmpnts.size == 1) {
      (hasInput(dfg), hasOutput(dfg)) match {
        case (true,  false) =>
          println("Warning! DFG has no output nodes.")
        case (false, true ) =>
          println("Warning! DFG has no input nodes.")
        case (false, false) =>
          println("Warning! DFG has no input or output nodes.")
        case _ => // no action needed
      }
    } else {
      cmpnts.foreach { subdfg =>
        (hasInput(subdfg), hasOutput(subdfg)) match {
          case (true,  false) =>
            println(s"Warning! DFG component ${subdfg.name} has no output nodes.")
          case (false, true ) =>
            println(s"Warning! DFG component ${subdfg.name} has no input nodes.")
          case (false, false) =>
            println(s"Warning! DFG component ${subdfg.name} has no input or output nodes.")
          case _ => // no action needed
        }
      }
    }
  }
}
