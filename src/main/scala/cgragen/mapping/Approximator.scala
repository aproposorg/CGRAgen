package cgragen.mapping

import cgragen.mapping.ApproximatorType._
import cgragen.mapping.approximators._

import scala.collection.mutable

/** Base approximator
 * 
 * @note Custom approximators should extend this class and implement the 
 *       `apprxMapping(mapping)` method.
 */
private[mapping] abstract class Approximator {
  /** Various other methods start ********************************************/
  /** Apply an approximator to a given mapping in place
   * @param mapping the mapping to approximate
   */
  def apprxMapping(mapping: Mapping): Unit
  /** Various other methods end **********************************************/
}

object Approximator {
  /** Create a new approximator with implicit arguments
   * @return a new instance of a sub-class of [[Approximator]]
   */
  def apply()(implicit conf: cgragen.Parameters): Approximator = conf.ApproximatorType match {
    case HeuristicApproximator => new HeuristicApproximator()
    case _ =>
      println(s"[ERROR] Approximator type (${conf.ApproximatorType}) not supported")
      throw new ApproximatorTypeUnavailableException(s"${conf.ApproximatorType} approximator not supported")
  }
}
