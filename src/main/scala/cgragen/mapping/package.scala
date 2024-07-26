package cgragen

package object mapping {  
  /** Some new exception classes */
  private[mapping] class MapperTypeUnavailableException(msg: String) extends Exception(msg)
  private[mapping] class ApproximatorTypeUnavailableException(msg: String) extends Exception(msg)
  private[mapping] class MappingFailedException extends Exception
  private[mapping] class MissingDFGNodeException(msg: String) extends Exception(msg)

  // Mapper type enumeration
  object MapperType extends Enumeration {
    type MapperType = Value
    val HeuristicMapper          = Value("heuristic")
    val ILPMapper                = Value("ilp")
    val SimulatedAnnealingMapper = Value("simulated_annealing")
  }

  // Approximator type enumeration
  object ApproximatorType extends Enumeration {
    type ApproximatorType     = Value
    val HeuristicApproximator = Value("heuristic")
  }
}
