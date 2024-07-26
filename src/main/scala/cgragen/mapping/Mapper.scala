package cgragen.mapping

import cgragen.cgra.{AbstractModule, AbstractBaseModule, CGRA}
import cgragen.cgra.primitives._

import cgragen.dfgparse._
import cgragen.dfgparse.Opcode._

import cgragen.mapping.MapperType._
import cgragen.mapping.mappers._

import cgragen.mrrg.{MRRG, MRRGNode}
import cgragen.mrrg.MRRGNodeType._

import scala.collection.mutable

import scala.math.exp

/** Base mapper
 * @param cgra the CGRA to target
 * 
 * @note Mapping engines should extend this class and implement the
 *       `mapGraph(dfg, mrrg, limit)` method. This class' `mapGraph(dfg)` 
 *       method handles executing mapping for the range of initiation interval 
 *       from an estimated minimum value to the specified maximum `conf.II`.
 */
private[mapping] abstract class Mapper(val cgra: CGRA)(implicit conf: cgragen.Parameters) {
  /** Various other methods start ********************************************/
  /** Generate a mapping from a DFG
   * @param dfg the DFG to map to the CGRA
   * @return a new instance of [[Mapping]], if any
   */
  def mapGraph(dfg: DFG): Option[Mapping] = {
    val utilLimit = conf.MapperArgs.getOrElse("FUUtilizationLimit", "1").toDouble
    if (0.0 >= utilLimit || utilLimit > 1.0) {
      println(s"[ERROR] Function unit utilization limit must be in (0, 1]")
      throw new Exception("invalid function unit utilization limit")
    }

    // Derive the start II solely based on available FUs and required operations by 
    // ... computing the number of each kind of operation in the DFG
    val numOps = Map.from(Opcode.values.map(opcode => (opcode -> dfg.nodes.count(_.opcode == opcode))))
    // ... computing the number of each FUs supporting each operation in the CGRA
    def getRecSupportedOps(mod: AbstractModule): Seq[Opcode] = mod match {
      case const: AbstractConstantUnit => Seq(OpConst)
      case fu   : AbstractFunctionUnit => fu.operations.map(_.op)
      case in   : AbstractInputUnit    => Seq(OpInput)
      case out  : AbstractOutputUnit   => Seq(OpOutput)
      case _ => mod.subModules.values.flatMap(getRecSupportedOps(_)).toSeq
    }
    val numFUs = {
      val supportedOps = cgra.subModules.values.flatMap(getRecSupportedOps(_))
      Map.from(Opcode.values.map(opcode => (opcode -> supportedOps.count(_ == opcode))))
    }
    // ... computing the total number of function units supporting arithmetic 
    // and logic operations in the CGRA
    def getRecFUCnt(mod: AbstractModule): Int = mod match {
      case base: AbstractBaseModule =>
        base.subModules.values.map(getRecFUCnt(_)).sum
      case fu: AbstractFunctionUnit if fu.operations
        .exists(oprtn => isArith(oprtn.op) || isLogic(oprtn.op)) => 1
      case _ => 0
    }
    val totalFUs = cgra.subModules.values.foldLeft(0) { case (acc, subMod) =>
      acc + getRecFUCnt(subMod) }
    // ... and finding the minimum II from these computations
    def supported(ii: Int): Boolean = {
      val numOk = numOps
        .collect { case (opcode, cnt) if isArith(opcode) || isLogic(opcode) => cnt }
        .sum <= (totalFUs * utilLimit * ii).intValue

      val ops = numOps.filter { case (opcode, opCnt) => opCnt > numFUs(opcode) * ii }
      if (ops.nonEmpty && conf.MapperDebug) {
        print(s"[DEBUG] Mapping for II=${ii} not possible due to too few ")
        print("function units available for operations ")
        print(s"(${ops.mkString("[", ", ", "]")}), available ")
        println(s"(${numFUs.mkString("[", ", ", "]")})")
      } else if (!numOk && conf.MapperDebug) {
        print(s"[DEBUG] Mapping for II=${ii} not possible due to too few ")
        println("simultaneously available function units")
      }

      ops.isEmpty && numOk
    }

    // Check if the CGRA supports the multi-cast features required by the DFG
    val mcOK = {
      // Generate the MRRG for II=1
      val mrrg = MRRG(cgra, 1)

      // Find the set of opcodes that support multi-cast
      val spprtdMCOps = numOps.keys
        .filter { opcode =>
          // Get all the supporting function units
          val fus = mrrg.nodes
            .flatMap(_.values.filter(_.nodeType == NodeFunction))
            .filter(_.canMapOp(opcode))

          // Check whether a function unit reaches two other function units
          def _reachesTwo(fu: MRRGNode): Boolean = {
            val reached = mutable.HashSet.empty[AbstractModule]
            val visited = mutable.HashSet.empty[MRRGNode]
            val queue   = mutable.ArrayBuffer(fu.fanout.toSeq:_*)
            while (queue.nonEmpty && reached.size < 2) {
              val node = queue.head
              queue.dropInPlace(1)
              visited += node
              if (node.nodeType == NodeFunction && !reached(node.parent) && node.parent != fu.parent) {
                reached += node.parent
              } else if (node.nodeType != NodeFunction) {
                queue ++= node.fanout
                  .filter(!visited(_))
              }
            }
            reached.size >= 2
          }

          // Filter and check if at least one function unit supports multi-cast
          fus.foldLeft(false) { case (acc, fu) => acc || _reachesTwo(fu) } }
        .toSet

      // Find the set of opcodes that need multi-cast support
      val reqMCOps = dfg.nodes
        .collect { case src if dfg.edges.count(_.src == src) > 1 => src.opcode }
        .toSet

      val res = reqMCOps.forall(spprtdMCOps(_))
      if (!res && conf.MapperDebug) {
        print("[DEBUG] CGRA supports multi-cast from function units with ")
        print(s"opcodes (${spprtdMCOps.mkString("{", ", ", "}")}), but DFG ")
        print("requires multi-cast from nodes with opcodes ")
        println(s"(${reqMCOps.mkString("{", ", ", "}")})")
      }
      res
    }

    // Compute the target IIs and their associated time limits
    val targetIIs = (1 to conf.II).filter(supported(_))
    val timeLimits = {
      val scale = targetIIs.map(exp(_)).sum
      targetIIs.map(ii => conf.MapperTimeLimit * exp(ii) / scale) }

    // Attempt mapping at each target II lazily
    val mappingOpt = targetIIs.zip(timeLimits).view
      .map { case (ii, limit) =>
        val mrrg = MRRG(cgra, ii)
        if (conf.MapperDebug) {
          print(s"[DEBUG] Built MRRG with ${mrrg.nodeCnt} nodes and ${mrrg.edgeCnt} ")
          println(s"edges for CGRA (${cgra.name}) with II (${mrrg.ii})")
        }

        mapGraph(dfg, mrrg, limit) }
      .collectFirst { case Some(map) if map.isMapped => map }

    // Return the mapping if found, otherwise return nothing
    mappingOpt match {
      case Some(map) =>
        if (conf.MapperDebug)
          println(s"[DEBUG] Succeeded in completing mapping for II (${map.ii})")
        Some(map)
      case _ =>
        println(s"[ERROR] Cannot perform mapping for maximum II (${conf.II})")
        None
    }
  }

  /** Generate a mapping from a DFG and a given initiation interval
   * @param dfg the DFG to map to the CGRA
   * @param mrrg the MRRG to map to
   * @param timeLimit the time limit of the mapper in seconds
   * @return a new instance of [[Mapping]], if any
   */
  def mapGraph(dfg: DFG, mrrg: MRRG, timeLimit: Double): Option[Mapping]
  /** Various other methods end **********************************************/
}

object Mapper {
  /** Create a new mapper with implicit arguments
   * @return a new instance of a sub-class of [[Mapper]] of the specified type
   */
  def apply(cgra: CGRA)(implicit conf: cgragen.Parameters) = conf.MapperType match {
    case HeuristicMapper          => new HeuristicMapper(cgra)
    case ILPMapper                => new ILPMapper(cgra)
    case SimulatedAnnealingMapper => new SimulatedAnnealingMapper(cgra)
    case _ =>
      println(s"[ERROR] Mapper type (${conf.MapperType}) not supported")
      throw new MapperTypeUnavailableException(s"${conf.MapperType} mapper not supported")
  }
}
