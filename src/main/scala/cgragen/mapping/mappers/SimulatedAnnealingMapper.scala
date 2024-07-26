package cgragen.mapping.mappers

import cgragen.cgra.CGRA

import cgragen.dfgparse._

import cgragen.mapping.{Mapper, Mapping}

import cgragen.mrrg.MRRG

/** Simulated annealing-based mapping engine
 * @param cgra the CGRA to target
 * 
 * @todo Implement this!
 */
private[mapping] final class SimulatedAnnealingMapper(cgra: CGRA)
  (implicit conf: cgragen.Parameters) extends Mapper(cgra) {
  /** Various other methods start ********************************************/
  def mapGraph(dfg: DFG, mrrg: MRRG, timeLimit: Double): Option[Mapping] = ???
  /** Various other methods end **********************************************/
}
