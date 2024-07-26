package cgragen.opt

import cgragen.cgra.CGRA

import cgragen.dfgparse.DFG

import scala.collection.mutable

/** Callable sequence of passes
 * @param passes the list of passes to include in the sequence
 * 
 * @note The list of passes is sorted topologically by their pre- and post-
 *       requisite passes. This sorting may fail if two or more passes are 
 *       inter-dependent. Therefore, passes should be minimally dependent on 
 *       other passes.
 */
private[cgragen] class PassSequence(passes: Seq[Pass]) {
  /** Graph of passes for sorting purposes */
  private class PassGraph {
    case class PassEdge(src: Pass, snk: Pass)

    private val _passes = mutable.HashSet.empty[Pass]
    private val _edges  = mutable.HashSet.empty[PassEdge]

    def passes: Seq[Pass]     = _passes.toSeq
    def edges : Seq[PassEdge] = _edges .toSeq

    def addPass(pass: Pass): Unit = _passes += pass

    def addEdge(src: Pass, snk: Pass): Unit = _edges += PassEdge(src, snk)
  }

  // Topologically sorted list of passes
  val order: Seq[Pass] = {
    // Recursively get all pre- and post-requisite passes
    def _collectReqs(pass: Pass, vstd: Set[Pass] = Set.empty[Pass]): Seq[Pass] = {
      // Add this pass to the set of visited passes
      val vstdWPass = vstd + pass

      // Run through all pre-requisites
      val vstdWPres = pass.preReqs
        .filterNot(vstdWPass(_)) // conservative filter
        .foldLeft(vstdWPass) { case (acc, p) => acc ++ _collectReqs(p, acc) }

      // Run through all post-requisites
      val vstdWPosts = pass.postReqs
        .filterNot(vstdWPres(_)) // conservative filter
        .foldLeft(vstdWPres) { case (acc, p) => acc ++ _collectReqs(p, acc) }

      // Return all the visited passes
      vstdWPosts.toSeq
    }
    val all = passes
      .flatMap(_collectReqs(_))
      .toSet.toSeq

    // Check that the passes form a DAG
    require(!all.combinations(2)
      .exists { case one :: two :: Nil =>
        (two.preReqs .contains(one) && one.preReqs .contains(two)) ||
        (two.postReqs.contains(one) && one.postReqs.contains(two)) },
      "cannot produce a pass sequence with inter-dependent passes")

    // Build a graph of the passes
    val graph = new PassGraph
    all.foreach { pass =>
      graph.addPass(pass)
      pass.preReqs .foreach(graph.addEdge(_, pass))
      pass.postReqs.foreach(graph.addEdge(pass, _))
    }

    // Perform a topological sorting on the graph
    val visited = mutable.HashSet.empty[Pass]
    val pOrder  = mutable.ArrayBuffer.empty[Pass]

    // Perform a BFS starting in nodes with only out-going edges
    def _bfs(pass: Pass): Unit = {
      visited += pass
      graph.edges
        .filter (edge => edge.src == pass && !visited(edge.snk))
        .foreach(edge => _bfs(edge.snk))
      pOrder += pass
    }
    val snks = graph.edges.map(_.snk).toSet
    graph.passes
      .filterNot(snks(_))
      .foreach { pass => if (!visited(pass)) _bfs(pass) }
    pOrder.reverse.toSeq
  }

  /** Apply this sequence of passes to a DFG-CGRA pair
   * @param dfg the DFG to apply this pass to
   * @param cgra the target CGRA
   */
  def apply(dfg: DFG, cgra: CGRA)(implicit conf: cgragen.Parameters): Unit = {
    order.foreach(_(dfg, cgra))
  }

  /** Return this sequence of passes in a string */
  override def toString(): String = {
    if (order.isEmpty) {
      "Empty sequence of passes"
    } else {
      val passList = order
        .map(pass => s" - $pass")
        .mkString("\n")
      s"Sequence of passes in the following order:\n${passList}"
    }
  }
}
