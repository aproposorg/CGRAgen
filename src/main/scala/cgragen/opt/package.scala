package cgragen

import cgragen.cgra.CGRA

import cgragen.dfgparse.DFG

import cgragen.opt.passes._

import scala.collection.mutable

package object opt {
  private[cgragen] class PassException(msg: String) extends Exception(msg)

  /** Abstract base pass class
   * @param preReqs pre-requisite passes for this pass
   * @param postReqs post-requisite passes for this pass
   * 
   * @note Inheriting objects or classes must define an `apply` method that 
   *       takes as parameters a `DFG` and a `CGRA`. The CGRA is used only for 
   *       architectural information and is not affected by the pass.
   */
  private[cgragen] abstract class Pass(val preReqs: Seq[Pass], val postReqs: Seq[Pass]) {
    // Validate that no pre-requisite passes are also listed as post-requisites
    require(preReqs
      .foldLeft(true) { case (acc, p) => acc && !postReqs.contains(p) },
      "cannot enlist the same pass as both pre- and post-requisite")

    /** Apply this pass to a DFG-CGRA pair
     * @param dfg the DFG to apply this pass to
     * @param cgra the target CGRA
     */
    def apply(dfg: DFG, cgra: CGRA)(implicit conf: Parameters): Unit
  }

  /** Compile the sequence of optimization passes requested by the user */
  private[cgragen] def getOptPasses()(implicit conf: Parameters): PassSequence = {
    val passes = mutable.ArrayBuffer.empty[Pass]

    // Insert the requested DFG optimization passes
    if (conf.DFGPropagateConstants)      passes += PropagateConstPass
    if (conf.DFGAvoidMulticastConstants) passes += ReplicateConstPass
    if (conf.DFGRemoveDeadNodes)         passes += RemoveDeadPass
    if (conf.DFGLimitFanout)             passes += MaxFanoutPass

    // Insert the requested DFG approximation passes
    if (conf.ApproximateArithmetic) {
      passes += new Pass(passes.toSeq, Seq.empty[Pass]) {
        def apply(dfg: DFG, cgra: CGRA)(implicit conf: Parameters): Unit = {
          MarkApproximablePass(dfg, cgra)
        }
        override def toString(): String = MarkApproximablePass.toString()
      }
      passes += new Pass(passes.toSeq, Seq.empty[Pass]) {
        def apply(dfg: DFG, cgra: CGRA)(implicit conf: Parameters): Unit = {
          EstimateErrorsPass(dfg, cgra)
        }
        override def toString(): String = EstimateErrorsPass.toString()
      }
    }

    // Ensure the integrity of the DFG
    if (passes.nonEmpty) {
      passes += new Pass(passes.toSeq, Seq.empty[Pass]) {
        def apply(dfg: DFG, cgra: CGRA)(implicit conf: Parameters): Unit = {
          IntegrityCheckPass(dfg, cgra)
        }
        override def toString(): String = IntegrityCheckPass.toString()
      }
    }

    new PassSequence(passes.toSeq)
  }
}
