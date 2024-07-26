package cgragen.mapping.mappers

import cgragen.cgra.CGRA
import cgragen.cgra.AbstractOperation.isApproximable
import cgragen.cgra.primitives.{AbstractFunctionUnit, AbstractRegisterFileUnit}

import cgragen.dfgparse._, DFG.{components, topologicalSort}, Opcode._

import cgragen.mapping.{Mapper, Mapping}

import cgragen.mrrg.{MRRG, MRRGNode}
import cgragen.mrrg.MRRGNodeType._

import scala.collection.mutable

import scala.util.Random

import System.nanoTime

/** Heuristic mapping engine
 * @param cgra the CGRA to target
 * 
 * @note Implements a recursive mapping algorithm with local searching 
 *       inspired by PathSeeker upon failing to map a DFG node. Uses 
 *       randomization to break ties.
 */
private[mapping] final class HeuristicMapper(cgra: CGRA)
  (implicit conf: cgragen.Parameters) extends Mapper(cgra) {
  /** Various other methods start ********************************************/
  /** Find all free function unit nodes compatible with a given DFG node
   * @param dfgNode the DFG node
   * @param predDfgEdges the set of edges from mapped predecessor DFG nodes
   * @param succDfgEdges the set of edges to mapped successor DFG nodes
   * @param mrrg the MRRG to search through
   * @param mapping the current mapping
   * @param rng the RNG to use in decision making
   * @return a list of all function unit nodes in `mrrg` not currently mapped 
   *         in `mapping` and compatible with `dfgNode`
   * 
   * @note This function only checks that there exists at least one route from 
   *       all mapped predecessors to a target function unit node and from 
   *       there to all mapped successors, not whether the routes overlap!
   */
  private def _placement(dfgNode: DFGNode, predDfgEdges: Seq[DFGEdge],
    succDfgEdges: Seq[DFGEdge], mrrg: MRRG, mapping: Mapping, rng: Random): Seq[MRRGNode] = {
    // Get all free function unit nodes that can support the DFG node
    val freeFUs = {
      val allFUs = mrrg.nodes.flatMap(_.values.filter(_.nodeType == NodeFunction))
        .filter(fuNode => fuNode.canMapOp(dfgNode.opcode) && !mapping.occupancy(fuNode))
        .toSeq
      if (dfgNode.opcode == OpConst && dfgNode.attrs.contains("value")) {
        val cst = dfgNode.attrs("value").toInt
        allFUs
          .filter { fuNode =>
            val size = fuNode.parent.dataSize
            val hi = (1 << (size - 1)) - 1
            val lo = -(1 << (size - 1))
            lo <= cst && cst <= hi }
      } else allFUs
    }

    // Get all the mapped predecessors and successors of the DFG node
    val predMrrgNodes = predDfgEdges.map(edge => mapping.nodeMap(edge.src))
    val succMrrgNodes = succDfgEdges.map(edge => mapping.nodeMap(edge.snk))

    // Compute the distance from a source to all reachable MRRG nodes
    def distances(src: MRRGNode): Map[MRRGNode, Int] = {
      // Keep track of visited nodes and their distances
      val visited = mutable.HashMap.empty[MRRGNode, Int]

      // Find the set of occupied routing and register nodes
      val occupied = {
        val all = mapping.occupancy.filter(_.nodeType != NodeFunction)
        val srcDfgNode = mapping.nodeMap
          .filter(_._2 == src)
          .keys.headOption match {
            case Some(node) => node
            case _ => dfgNode
          }
        val sameSrc = mapping.edgeMap
          .filter(_._1.src == srcDfgNode)
          .values
          .foldLeft(Set.empty[MRRGNode]) { case (acc, route) => acc ++ route }
        all -- sameSrc
      }

      // Perform a BFS starting in the source node
      val queue = mutable.ArrayBuffer(src.fanout.toSeq.filter(!occupied(_)).map(fo => (fo, 1)):_*)
      while (queue.nonEmpty) {
        val (node, dist) = queue.head
        queue.dropInPlace(1)
        if (!visited.contains(node) || visited(node) > dist) {
          visited(node) = dist
          queue ++= node.fanout
            .filter(fo => !occupied(fo) && (!visited.contains(fo) || visited(fo) > (dist + 1)))
            .map(fo => (fo, dist + 1))
        }
      }

      visited.toMap
    }

    // Filter the function unit nodes by connectivity and sort them according 
    // to their collective distance
    val srcDsts = mutable.HashMap.empty[MRRGNode, Map[MRRGNode, Int]]
    val fuOpts = freeFUs
      .filter { fuNode =>
        predMrrgNodes.forall { predNode =>
          srcDsts
            .getOrElseUpdate(predNode, distances(predNode))
            .getOrElse(fuNode, Int.MaxValue) < Int.MaxValue } &&
        succMrrgNodes.forall { succNode =>
          srcDsts
            .getOrElseUpdate(fuNode, distances(fuNode))
            .getOrElse(succNode, Int.MaxValue) < Int.MaxValue }}
    val rands = Seq.fill(fuOpts.size) { rng.nextInt() }
    val res = fuOpts.zip(rands)
      .sortBy { case (fuNode, rand) =>
        val cost = predMrrgNodes.map(predNode => srcDsts(predNode)(fuNode)).sum +
                   succMrrgNodes.map(succNode => srcDsts(fuNode)(succNode)).sum
        val sprt = isApproximable(dfgNode.opcode) && fuNode.parent
          .asInstanceOf[AbstractFunctionUnit].approx
        (cost, !sprt, rand) }
      .map(_._1)

    if (conf.MapperDebug) {
      if (res.nonEmpty) {
        val subset = res.take(3).mkString(", ")
        print(s"[DEBUG] Found ${res.size} candidate function units for DFG ")
        println(s"node (${dfgNode}), including: ${subset}")
      } else {
        println(s"[DEBUG] Found no candidate function units for DFG node (${dfgNode})")
      }
    }

    res
  }

  /** Route type for improved readability */
  private type Route = Seq[MRRGNode]

  /** Perform routing for a function unit node and its source and sink nodes
   * @param dfgNode the DFG node
   * @param fuNode the function unit node
   * @param predDfgEdges the set of edges from mapped predecessor DFG nodes
   * @param succDfgEdges the set of edges to mapped successor DFG nodes
   * @param mrrg the MRRG to route through
   * @param mapping the current mapping
   * @param rng the RNG to use in decision making
   * @return a list of valid combinations of routes to and from all mapped 
   *         predecessors and successors of `dfgNode` in `mrrg` assuming 
   *         `dfgNode` is mapped to `fuNode`
   * 
   * @note Ignores matching delays on input nodes and allows for constant 
   *       nodes to have one of two delays: zero or the same as the other 
   *       connections. Inputs can be controlled externally.
   */
  private def _routing(dfgNode: DFGNode, fuNode: MRRGNode, predDfgEdges: Seq[DFGEdge],
    succDfgEdges: Seq[DFGEdge], mrrg: MRRG, mapping: Mapping, rng: Random): Seq[Map[DFGEdge, Route]] = {
    val succDfgNodes    = succDfgEdges.map(_.snk).toSet
    val exstngSuccRouts = mapping.edgeMap
      .filter { case (edge,  _) => succDfgNodes(edge.snk) }
      .map { case (edge, route) => (edge -> route.toSeq)  }
      .toMap

    // Generate all routes between source and sink MRRG nodes
    def allRoutes(dfgEdge: DFGEdge): Seq[Route] = {
      val srcMrrgNode = if (dfgEdge.src == dfgNode) fuNode else mapping.nodeMap(dfgEdge.src)
      val snkMrrgNode = if (dfgEdge.snk == dfgNode) fuNode else mapping.nodeMap(dfgEdge.snk)

      // Find the set of occupied routing and register nodes
      val occupied = {
        val all = mapping.occupancy.filter(_.nodeType != NodeFunction)
        val sameSrc = mapping.edgeMap
          .filter(_._1.src == dfgEdge.src)
          .values
          .foldLeft(Set.empty[MRRGNode]) { case (acc, route) => acc ++ route }
        all -- sameSrc
      }

      // Recursively build routes through an MRRG
      def build(route: Route): Seq[Route] = {
        // Get the last node on the route
        val mrrgNode = route.last

        // Return this route if it reaches the sink MRRG node in the right 
        // arithmetic position
        if (mrrgNode.fanout.contains(snkMrrgNode) && 
            snkMrrgNode.fanin.indexOf(mrrgNode) == dfgEdge.operand) Seq(route)
        // Stop recursing if the route includes more nodes than the square 
        // root of the total number of nodes in the MRRG
        else if (route.size > scala.math.sqrt(mrrg.nodeCnt / mrrg.ii).intValue * 3 / 4) Seq.empty[Route]
        // Otherwise keep building routes recursively
        else mrrgNode.fanout
          .filter (fo => fo.nodeType != NodeFunction && !occupied(fo) && !route.contains(fo))
          .flatMap(fo => build(route :+ fo))
          .toSeq
      }

      // Collect routes built from each fanout of the source node
      srcMrrgNode.fanout
        .filter (_.nodeType != NodeFunction)
        .flatMap(fo => build(Seq(fo)))
        .toSeq
    }

    // Generate the cross product of any number of edge mappings
    def cross(cands: Map[DFGEdge, Seq[Route]]): Seq[Map[DFGEdge, Route]] = {
      if (cands.isEmpty) { // no action possible
        Seq.empty[Map[DFGEdge, Route]]
      } else if (cands.size == 1) { // no recursive call
        val (edge, routes) = cands.head
        routes.map(route => Map(edge -> route))
      } else { // recursive call
        val (edge, routes) = cands.head
        val tailCross = cross(cands.tail)
        routes.flatMap { route =>
          tailCross.map(comb => comb + (edge -> route)) }
      }
    }

    // Compute the delay (i.e., number of registers) of a route
    def delay(route: Route): Int = route.count(_.nodeType == NodeRegister)

    // Check whether a combination of routes are non-overlapping
    def nonOverlapping(comb: Map[DFGEdge, Route]): Boolean = {
      val srcDfgNodes = comb.keys.map(_.src).toSet

      // Only edges with the same source DFG node can share MRRG nodes
      val srcOccpncs  = srcDfgNodes.toSeq
        .map { src => (src -> comb.filter(_._1.src == src).flatMap(_._2).toSet) }
        .toMap
      srcOccpncs.forall { case (src, route) =>
        val otherOccpncs = (srcOccpncs - src).flatMap(_._2).toSet
        route.forall(!otherOccpncs(_)) }
    }

    // Check whether a collection of routes have the same delay
    def sameDelayRouts(comb: Map[DFGEdge, Route]): Boolean = {
      // Implement checks separately for input, constant, and other nodes
      val constDelays = comb
        .collect { case (edge, route) if edge.src.opcode == OpConst => delay(route) }
        .toSet // okay iff {}, {0}, {c}, or {0, c}
      val otherDelays = comb
        .collect { case (edge, route) if edge.src.opcode != OpConst && edge.src.opcode != OpInput => delay(route) }
        .toSet // okay iff {} or {c}

      val othersOk = otherDelays.size <= 1
      val constOk  = constDelays.size match {
        case 0 => true
        case 1 => constDelays(0) || (otherDelays.isEmpty || constDelays(otherDelays.head))
        case 2 => constDelays(0) && (otherDelays.isEmpty || constDelays(otherDelays.head))
        case _ => false
      }

      constOk && othersOk
    }

    // Check whether a collection of routes have reconverging paths from the 
    // same source node
    def divReconvRouts(comb: Map[DFGEdge, Route]): Boolean = {
      val srcDfgNodes = comb.keys.map(_.src).toSet

      srcDfgNodes.forall { src =>
        val routes = comb.filter(_._1.src == src).values.toSeq

        // Edges with the same source cannot diverge and reconverge later
        val lpfx = routes.tail.foldLeft(routes.head) { case (acc, route) =>
          route.zip(acc).takeWhile { case (a, b) => a == b }.map(_._1) }
        val routOccpncs = routes.map(_.drop(lpfx.size))
        val reconvOk = (0 until routOccpncs.size).forall { i =>
          val otherOccpncs = routOccpncs.drop(i).tail
            .foldLeft(Set.empty[MRRGNode]) { case (acc, set) => acc ++ set }
          routes(i).forall(!otherOccpncs(_)) }
        
        // Edges with the same source cannot diverge in a register file
        val rfIns = routes
          .flatMap { _.filter { mrrgNode =>
            mrrgNode.parent.isInstanceOf[AbstractRegisterFileUnit] &&
            mrrgNode.parent.ports.contains(mrrgNode.name.split('.').last) } }
        val divRFOk = rfIns.forall { rfInNode =>
          val incls = routes.filter(_.contains(rfInNode))
          incls.flatMap { _.filter { mrrgNode =>
            mrrgNode.parent.isInstanceOf[AbstractRegisterFileUnit] &&
            mrrgNode.nodeType == NodeRegister } }.toSet.size <= 1 }

        reconvOk && divRFOk }
    }

    // Record all possible routes for each predecessor and successor edge
    val allRoutCands = (predDfgEdges ++ succDfgEdges)
      .foldLeft(Map.empty[DFGEdge, Seq[Route]]) { case (acc, edge) =>
        if (acc.values.forall(_.size > 0)) acc + (edge -> allRoutes(edge))
        else acc + (edge -> Seq.empty[Route]) }

    // Compute all possible combinations of the routes and filter the valid ones
    val crossRouts = cross(allRoutCands)
      .filter { comb =>
        val overlapOk = nonOverlapping(comb)
        val predsOk = overlapOk && sameDelayRouts(comb
          .filter { case (edge, _) => predDfgEdges.contains(edge) })
        val succsOk = predsOk && sameDelayRouts(comb
          .filter { case (edge, _) => succDfgNodes(edge.snk) } ++ exstngSuccRouts)
        succsOk && divReconvRouts(comb) }
      .toSeq
    val rands = Seq.fill(crossRouts.size) { rng.nextInt() }
    val res = crossRouts.zip(rands)
      .sortBy { case (comb, rand) => (comb.values.map(_.size).sum, rand) }
      .map(_._1)

    if (conf.MapperDebug) {
      if (predDfgEdges.isEmpty && succDfgEdges.isEmpty) {
        print("[DEBUG] No mapped predecessors or successors of DFG node ")
        println(s"(${dfgNode}), no routes needed")
      } else if (res.nonEmpty) {
        val spc    = " " * 8
        val subset = res.take(3).zipWithIndex
          .map { case (comb, ind) =>
            val keys = comb.keys
              .map(dfgEdge => (dfgEdge, dfgEdge.toString()))
              .toSeq.sortBy(_._2.size)
            val routes = keys
              .map { case (dfgEdge, edgeStr) =>
                val route = comb(dfgEdge)
                val routStr = if (route.size > 4) {
                  val pfx = route.take(2)     .mkString("->")
                  val sfx = route.takeRight(2).mkString("->")
                  s"through ${pfx}-> ... ->${sfx}"
                } else s"to ${comb(dfgEdge).mkString("->")}"
                s"${spc}  ${edgeStr} mapped ${routStr}" }
            s"${spc}Combination ${ind + 1}:\n${routes.mkString("\n")}" }
            .mkString("\n")
        print(s"[DEBUG] Found ${res.size} candicate combinations of routes ")
        println(s"to/from DFG node (${dfgNode}), including:\n${subset}")
      } else {
        print(s"[DEBUG] Found no candidate combinations of routes to/from ")
        println(s"DFG node (${dfgNode}) mapped to MRRG node (${fuNode})")
      }
    }

    res
  }

  /** Execute a PathSeeker-like mapping algorithm for a given DFG
   * @param dfg the DFG to map
   * @param mrrg the MRRG to map to
   * @param queue a FIFO-like queue of ordered DFG nodes to map
   * @param mapping the current node and edge mapping
   * @param start the start time of the mapping algorithm in nanoseconds
   * @param limit the time limit for the mapping procedure in seconds
   * @param rng the RNG to use in decision making
   * @return a tuple of a mapping of `dfg` to `mrrg`, if any, and a map of 
   *         counts of how many iterations of recovery have been executed 
   *         per node in `dfg`
   */
  private def _pathSeeker(dfg: DFG, mrrg: MRRG, queue: Seq[DFGNode], mapping: Mapping,
    start: Long, limit: Double, rng: Random): (Option[Mapping], Map[DFGNode, Int]) = {
    if (conf.MapperDebug && queue.nonEmpty) {
      print("[DEBUG] Running PathSeeker with queue ")
      if (queue.size > 3)
        println(s"([${queue.take(3).mkString(", ")}, ...])")
      else
        println(s"(${queue.mkString("[", ", ", "]")})")
    }

    // Check that there is still time left to do mapping
    def overdue: Boolean = (nanoTime() - start) > (limit * 10e9).toLong

    // Run as long as the queue is non-empty and there is time left
    queue.headOption match {
      case Some(dfgNode) if !overdue => // mapping in progress
        val reCnts = mutable.HashMap.empty[DFGNode, Int]

        // Get all edges to/from mapped predecessors and successors of the DFG node
        val predDfgEdges = dfg.edges
          .filter(edge => edge.snk == dfgNode && mapping.nodeMap.contains(edge.src))
          .toSeq
        val succDfgEdges = dfg.edges
          .filter(edge => edge.src == dfgNode && mapping.nodeMap.contains(edge.snk))
          .toSeq

        // Use these data to find the suitable function units and routes
        val connFUs  = _placement(dfgNode, predDfgEdges, succDfgEdges, mrrg, mapping, rng)
        val routCmbs = connFUs.view
          .flatMap {
            case fuNode if (predDfgEdges ++ succDfgEdges).nonEmpty =>
              _routing(dfgNode, fuNode, predDfgEdges, succDfgEdges, mrrg, mapping, rng)
                .map(routes => (fuNode, routes))
            case fuNode =>
              Seq((fuNode, Map.empty[DFGEdge, Route])) }

        // Pick a placement of the DFG node and perform a recursive mapping call
        val recMapping = routCmbs
          .map { case (fuNode, routes) =>
            // Create an updated mapping and add the selected routes to it
            val updMapping = Mapping.from(mapping)
            updMapping.mapNode(dfgNode, fuNode)
            routes.foreach { case (dfgEdge, route) =>
              updMapping.mapEdge(dfgEdge, route) }

            // Recursively call the mapping function on the updated mapping
            val (res, cnts) = _pathSeeker(dfg, mrrg, queue.tail, updMapping, start, limit, rng) 
            cnts.foreach { case (node, cnt) =>
              reCnts(node) = reCnts.getOrElse(node, 0) + cnt }

            res }
          .collectFirst { case Some(updMapping) => updMapping }

        // If the recursive call failed, try a PathSeeker-like level-1 recovery
        val l1RecMapping = recMapping match {
          case None if !overdue =>
            reCnts(dfgNode) = reCnts.getOrElse(dfgNode, 0) + 1

            // Create an updated mapping with all mapped predecessors and 
            // successors unmapped
            val predDfgNodes = predDfgEdges.map(_.src)
            val succDfgNodes = succDfgEdges.map(_.snk)
            val l1Mapping = Mapping.from(mapping)
            val nghbrs    = succDfgNodes ++ predDfgNodes
            nghbrs.foreach(l1Mapping.unmapNode(_))
            if (conf.MapperDebug) {
              print(s"[DEBUG] Failed to map DFG node (${dfgNode}), unmapping ")
              println(s"neighbor nodes (${nghbrs.mkString("[", ", ", "]")})")
            }

            // Recursively call the mapping function on the updated mapping, 
            // attempting to map the problematic node first
            val updQueue = Seq(dfgNode) ++ nghbrs ++ queue.tail
            val (res, cnts) = _pathSeeker(dfg, mrrg, updQueue, l1Mapping, start, limit, rng)
            cnts.foreach { case (node, cnt) =>
              reCnts(node) = reCnts.getOrElse(node, 0) + cnt }

            res

          case _ => recMapping
        }

        (l1RecMapping, reCnts.toMap)

      case None => // mapping complete
        (Some(mapping), Map.empty[DFGNode, Int])

      case _ => // mapper out of time
        println(s"[ERROR] Mapper out of time")
        (None, Map.empty[DFGNode, Int])
    }
  }

  def mapGraph(dfg: DFG, mrrg: MRRG, limit: Double): Option[Mapping] = {
    // Generate an infinite sequence of seeds for randomization in the algorithm
    val seeds = {
      val rng = new Random(conf.MapperArgs.getOrElse("StartSeed", "42").toInt)
      def loop(num: Int): LazyList[Int] = num #:: loop(rng.nextInt())
      loop(rng.nextInt())
    }

    // Store the starting time before attempting any mapping
    val start = nanoTime()
    def overdue: Boolean = (nanoTime() - start) > (limit * 10e9).toLong

    // Run a PathSeeker-like mapping algorithm starting at the ends of the 
    // topologically sorted lists of nodes in the DFG's components
    val reCnts = mutable.HashMap.empty[DFGNode, Int]
    val cmpnts = components(dfg)
      .map(cmp => (cmp, topologicalSort(cmp).reverse))
    val mappingOpt = seeds
      .take(conf.MapperArgs.getOrElse("NSeeds", "5").toInt)
      .takeWhile(_ => !overdue)
      .map { seed =>
        // Map the DFG's components one by one
        val rng   = new Random(seed)
        val mppng = new Mapping(cgra, dfg, mrrg.ii)
        cmpnts
          .foldLeft[Option[Mapping]](Some(mppng)) { case (acc, (cmp, srtng)) => acc match {
            case Some(mapping) => // map another component
              try {
                val (updMapping, cnts) = _pathSeeker(cmp, mrrg, srtng, mapping, start, limit, rng)
                cnts.foreach { case (dfgNode, cnt) =>
                  reCnts(dfgNode) = reCnts.getOrElse(dfgNode, 0) + cnt }
                updMapping
              } catch {
                case err: Throwable =>
                  println(s"[ERROR] Mapping failed with exception $err")
                  None
              }
            case _ => None // a component has failed
          }}}
      .collectFirst { case Some(mapping) if mapping.isMapped => mapping }

    // Provide some guidance if the mapping was unsuccessful
    mappingOpt match {
      case None if conf.MapperDebug && reCnts.nonEmpty =>
        val spc = " " * 8
        val top = reCnts.toSeq.sortBy(_._2).take(3)
          .map { case (dfgNode, cnt) =>
            s"${spc}${dfgNode} ($cnt)" }
          .mkString("\n")
        print("[DEBUG] Mapping unsuccessful. The mapper performed recovery ")
        println(s"most times for the following nodes (# recoveries):\n${top}")
      case _ => // no action needed
    }
    mappingOpt
  }
  /** Various other methods end **********************************************/
}
