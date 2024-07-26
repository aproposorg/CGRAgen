package cgragen.mapping.approximators

import cgragen.cgra.AbstractOperation.isApproximable
import cgragen.cgra.primitives.AbstractFunctionUnit

import cgragen.dfgparse.{DFG, DFGNode}, DFG.{components, fanInCone, topologicalSort}
import cgragen.dfgparse.Opcode._

import cgragen.mapping.{Approximator, Mapping}

import chisel3.util.log2Up

import scala.collection.mutable

import scala.math.sqrt

import scala.util.Random

/** Heuristic approximation engine
 * 
 * @note Implements an error modeling algorithm inspired by X-CGRA. Unlike the 
 *       strategy originally proposed, this implementation requires emulating 
 *       the DFG for a range of random input values to estimate the sensitivity 
 *       of the output nodes with respect to each approximable node.
 */
private [mapping] final class HeuristicApproximator(implicit conf: cgragen.Parameters)
  extends Approximator {
  /** Various other methods start ********************************************/
  /** Compute the error sensitivity values for a given DFG
   * @param dfg the DFG to compute error sensitivity values for
   * @param apprxmbl the set of approximable DFG nodes and their function units
   * @param constrs the set of output DFG nodes and their error constraints
   * @param mapping the current node and edge mapping
   * @param rng the RNG used in input and error generation
   * @return a map of DFG nodes and error sensitivity values
   * 
   * @note Computes error sensitivity values through "simulation/emulation" of 
   *       the DFG by generating and executing it for many uniform random 
   *       inputs with and without errors in individual nodes.
   */
  private def _sensitivity(dfg: DFG, apprxmbl: Map[DFGNode, AbstractFunctionUnit],
    constrs: Map[DFGNode, Double], mapping: Mapping, rng: Random): Map[DFGNode, Double] = {
    // Consider the output nodes and their fan-in cones separately
    val outEss = constrs
      .map { case (out, _) =>
        // Get the fan-in cone of the output and build the associated sub-DFG
        val fi = fanInCone(out, dfg).toSet
        val subDfg = new DFG()
        (fi + out).foreach(subDfg.addNode(_))
        dfg.edges
          .filter { edge => (fi + out)(edge.src) && (fi + out)(edge.snk) }
          .foreach(subDfg.addEdge(_))

        // Assign default values to all constant nodes in sub-DFG
        val cstNodeVals = subDfg.nodes
          .collect { case node if node.opcode == OpConst =>
            val fu   = mapping.nodeMap(node).parent
            val mask = (BigInt(1) << fu.dataSize) - 1
            (node -> (BigInt(node.attrs("value").toInt) & mask)) }
          .toMap

        // Generate 1000 pairs of random input values
        val inpNodeValCombs = {
          val inps = subDfg.nodes
            .filter(_.opcode == OpInput)
            .map(in => (in -> mapping.nodeMap(in).parent.dataSize))
          (0 until 1000)
            .map(_ => inps.map { case (in, size) => (in -> BigInt(size, rng)) })
        }

        // Compute the value of a DFG node given the values of its predecessors 
        // and any possible errors it may introduce
        // @todo update when topologicalSort is fixed
        def compute(dfgNode: DFGNode, nodeVals: Map[DFGNode, BigInt], error: BigInt): BigInt = {
          // Establish the bit-masks to use for the mapped module
          val size    = mapping.nodeMap(dfgNode).parent.dataSize
          val mask    = (BigInt(1) << size) - 1
          val sftMask = (BigInt(1) << log2Up(size)) - 1
          val sxt     = BigInt(-1) << size
          def sext(num: BigInt): BigInt = {
            (if (num.testBit(size-1)) sxt else BigInt(0)) | num
          }

          // Get the values of the source nodes feeding the current node
          val srcVals = subDfg.edges
            .filter(_.snk == dfgNode)
            .toSeq
            .sortBy(_.operand)
            .map(edge => nodeVals(edge.src))

          // Compute the value of the node and mask it
          ((dfgNode.opcode match {
            case OpAdd    => srcVals(0) + srcVals(1)
            case OpSub    => srcVals(0) - srcVals(1)
            case OpMul    => srcVals(0) * srcVals(1)
            case OpDiv    => if (srcVals(1) != 0) srcVals(0) / srcVals(1) else BigInt(-1)
            case OpAnd    => srcVals(0) & srcVals(1)
            case OpOr     => srcVals(0) | srcVals(1)
            case OpXor    => srcVals(0) ^ srcVals(1)
            case OpShl    => srcVals(0)       << (srcVals(1) & sftMask).intValue
            case OpShra   => sext(srcVals(0)) >> (srcVals(1) & sftMask).intValue
            case OpShrl   => srcVals(0)       >> (srcVals(1) & sftMask).intValue
            case OpOutput => srcVals(0)
          }) + error) & mask
        }

        // Compute the golden results
        val golden = inpNodeValCombs
          .map { inpNodeVals =>
            val nodeVals = mutable.HashMap((cstNodeVals ++ inpNodeVals).toSeq:_*)
            while (subDfg.nodes.exists(!nodeVals.contains(_))) {
              val victimOpt = subDfg.nodes
                .filter { node =>
                  !nodeVals.contains(node) &&
                  subDfg.edges
                    .filter(_.snk == node)
                    .forall(edge => nodeVals.contains(edge.src)) }
                .headOption
              victimOpt match {
                case Some(dfgNode) =>
                  nodeVals += (dfgNode -> compute(dfgNode, nodeVals.toMap, BigInt(0)))
                case _ =>
                  println(s"[ERROR] DFG is not a DAG and cannot be computed")
                  throw new Exception("non-DAG cannot be computed")
              }
            }
            nodeVals(out) }

        // Repeat the experiment for each of the approximable nodes
        apprxmbl.keys
          .filter(subDfg.nodes.contains(_))
          .map { apprxNode =>
            // Generate the input errors
            val apprxSize = mapping.nodeMap(apprxNode).parent.dataSize / 2
            val apprxEDS  = (0 until inpNodeValCombs.size)
              .map { _ =>
                val rand = (BigDecimal(BigInt(1) << apprxSize) * rng.nextGaussian()).toBigInt
                (if (rand.testBit(apprxSize-1)) (BigInt(-1) << apprxSize) else BigInt(0)) | rand }

            // Capture the output errors during execution
            val outpNodeVals = inpNodeValCombs.zip(apprxEDS)
              .map { case (inpNodeVals, ed) =>
                val nodeVals = mutable.HashMap((cstNodeVals ++ inpNodeVals).toSeq:_*)
                while (subDfg.nodes.exists(!nodeVals.contains(_))) {
                  val victimOpt = subDfg.nodes
                    .filter { node =>
                      !nodeVals.contains(node) &&
                      subDfg.edges
                        .filter(_.snk == node)
                        .forall(edge => nodeVals.contains(edge.src)) }
                    .headOption
                  victimOpt match {
                    case Some(dfgNode) =>
                      val error = if (dfgNode == apprxNode) ed else BigInt(0)
                      nodeVals += (dfgNode -> compute(dfgNode, nodeVals.toMap, error))
                    case _ =>
                      println(s"[ERROR] DFG is not a DAG and cannot be computed")
                      throw new Exception("non-DAG cannot be computed")
                  }
                }
                nodeVals(out) }
            val outpEDs = golden.zip(outpNodeVals).map { case (a, b) => (b - a).abs }

            // Compute and store the error sensitivity
            val inMED  = apprxEDS.map(_.abs.doubleValue).sum / apprxEDS.size
            val outMED = outpEDs .map(_.doubleValue)    .sum / outpEDs .size

            (apprxNode -> outMED / inMED) }
          .toMap
      }

    // Collect the results of the separate runs conservatively (i.e., by 
    // picking the highest sensitivity registered for each node)
    outEss.foldLeft(Map.empty[DFGNode, Double]) { case (acc, ess) =>
      acc ++ ess.filter { case (dfgNode, es) => !acc.contains(dfgNode) || acc(dfgNode) < es }}
  }

  /** Recursively apply approximation to a given DFG
   * @param dfg the DFG to approximate
   * @param ess the error sensitivities of the nodes in the DFG
   * @param apprxmbl the set of approximable DFG nodes and their function units
   * @param constrs the set of output DFG nodes and their error constraints
   * @param queue a FIFO-like queue of ordered DFG nodes to approximate
   * @param mapping the current node and edge mapping
   * @return all possible mappings with approximation of nodes in `dfg`
   * 
   * @note This function is very sensitive to the number of approximation 
   *       modes supported by the inexact arithmetic units. Therefore, it does 
   *       not provide as many debug messages as the heuristic mapping function.
   */
  private def _apprxSeeker(dfg: DFG, ess: Map[DFGNode, Double],
    apprxmbl: Map[DFGNode, AbstractFunctionUnit], constrs: Map[DFGNode, Double],
    queue: Seq[(DFGNode, AbstractFunctionUnit)], mapping: Mapping): Seq[Mapping] = {
    // Run as long as the queue is non-empty
    queue.headOption match {
      case Some((dfgNode, fu)) => // approximation in progress
        // Compute the mapped variance of each output in the current mapping
        val mppdVars = constrs
          .map { case (out, _) =>
            // Get the fan-in cone of the output
            val fi = fanInCone(out, dfg).toSet

            // Focus on the nodes that contribute to this output
            val sdreds = mapping.approxModes
              .filter { case (node, _) => fi(node) }
              .map { case (node, mode) =>
                val chars = apprxmbl(node).operations
                  .filter(_.op == node.opcode)
                  .head.chars
                (node -> chars(mode)._2) }

            // Get their associated function unit characteristics
            val vrnc = sdreds.foldLeft(0.0) { case (acc, (node, sdred)) =>
              acc + ess(node) * (sdred * sdred) }

            (out -> vrnc) }

        // Get all satisfactory approximation modes for this node
        val modes = fu.operations
          .filter(_.op == dfgNode.opcode)
          .head.chars
          .filter { case (_, (_, sdred)) =>
            val locVrnc = ess(dfgNode) * (sdred * sdred)
            mppdVars.forall { case (out, vrnc) =>
              2 * sqrt(vrnc + locVrnc) <= constrs(out) }}
          .keys

        // Pick an approximate mode for the DFG node and perform a recursive call
        _apprxSeeker(dfg, ess, apprxmbl, constrs, queue.tail, mapping) ++ modes.flatMap { mode =>
          // Create an updated mapping and add the selected approximation to it
          val updMapping = Mapping.from(mapping)
          updMapping.addMode(dfgNode, mode)

          // Recursively call the approximation function on the updated mapping
          if (queue.tail.nonEmpty) 
            _apprxSeeker(dfg, ess, apprxmbl, constrs, queue.tail, updMapping)
          else Seq(updMapping) }

      case _ => // approximation complete or out of options
        Seq.empty[Mapping]
    }
  }

  def apprxMapping(mapping: Mapping): Unit = {
    val rng = new Random(42)

    // Maintain a copy of the mapping for approximation
    val res = Mapping.from(mapping)

    // Find components in the DFG and consider their approximation individually
    components(mapping.dfg).foreach { subDFG =>
      // Make a copy of the original mapping for approximation here
      val cpMapping = Mapping.from(mapping)

      // Find the set of approximable DFG nodes and their mapped function units
      val apprxmbl = mapping.nodeMap
        .collect { case (dfgNode, mrrgNode) if
          dfgNode.attrs.contains("approx") &&
          subDFG.nodes.contains(dfgNode) &&
          isApproximable(dfgNode.opcode) &&
          mrrgNode.parent.asInstanceOf[AbstractFunctionUnit].approx =>
          (dfgNode -> mrrgNode.parent.asInstanceOf[AbstractFunctionUnit]) }

      // Find the set of output nodes and their error constraints
      val constrs = subDFG.nodes
        .collect { case node if node.opcode == OpOutput && node.attrs.contains("approx") =>
          (node -> node.attrs("approx").toDouble) }
        .toMap

      // Compute the error sensitivity values for approximable DFG nodes 
      // through simple execution with introduction of errors
      val ess = _sensitivity(subDFG, apprxmbl, constrs, cpMapping, rng)

      if (conf.MapperDebug && ess.nonEmpty) {
        val outs  = constrs.keys.map(_.name).mkString("[", ", ", "]")
        val senss = ess
          .map { case (dfgNode, es) => s"${dfgNode.name}: ${es}" }
          .mkString("{", ", ", "}")
        print(s"[DEBUG] For sub-DFG feeding output nodes (${outs}), got ")
        println(s"error sensitivities (${senss})")
      }

      // With the sensitivity values known, apply approximations recursively
      val order = apprxmbl.toSeq
        .sortBy { case (dfgNode, _) => ess(dfgNode) }
      val recs = _apprxSeeker(subDFG, ess, apprxmbl, constrs, order, cpMapping)

      // Pick the resulting mapping with the most nodes approximated (secondly, 
      // highest mode sum) and copy its data over to the final mapping
      val rands = Seq.fill(recs.size) { rng.nextInt() }
      val srtd  = recs.zip(rands).sortBy { case (mppng, rand) =>
        val cnt  = mppng.approxModes.size
        val sum  = mppng.approxModes.values.sum
        (cnt, sum, rand) }
        .map(_._1)
      srtd.lastOption match {
        case Some(updMapping) =>
          updMapping.approxModes
            .foreach { case (dfgNode, mode) => res.addMode(dfgNode, mode) }
        case _ => // no action needed
      }
    }

    // Transfer approximation modes from the copy of the mapping
    res.approxModes
      .foreach { case (dfgNode, mode) => mapping.addMode(dfgNode, mode) }

    // Report on the results of the approximation
    if (res.approxModes.isEmpty || res.approxModes.values.forall(_ == 0)) {
      println(s"No approximation applied to DFG (${mapping.dfg.name})")
    } else {
      println(s"Approximation applied to DFG (${res.dfg.name})")
      val apprxmtd = res.approxModes
        .filter(_._2 > 0)
      val lngstNameLen = (apprxmtd.keys.map(_.name).toSeq :+ "node").map(_.size).max
      val lbls = apprxmtd
        .map { case (dfgNode, mode) =>
          s"  ${dfgNode.name.padTo(lngstNameLen, ' ')}: ${mode}" }
        .mkString("\n")
      println(s"  ${"node".padTo(lngstNameLen, ' ')}: mode")
      println(lbls)
    }
  }
  /** Various other methods end **********************************************/
}
