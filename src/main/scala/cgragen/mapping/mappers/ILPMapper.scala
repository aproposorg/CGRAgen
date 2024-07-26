package cgragen.mapping.mappers

import cgragen.cgra.CGRA

import cgragen.dfgparse._

import cgragen.mapping.{Mapper, Mapping}

import cgragen.mrrg.MRRG

/** ILP-based mapping engine
 * @param cgra the CGRA to target
 * 
 * @todo Implement this! Use an open-source solver here, e.g., Z3.
 */
private[mapping] final class ILPMapper(cgra: CGRA)
  (implicit conf: cgragen.Parameters) extends Mapper(cgra) {
  /** Various other methods start ********************************************/
  def mapGraph(dfg: DFG, mrrg: MRRG, timeLimit: Double): Option[Mapping] = ???
  /** Various other methods end **********************************************/
}
