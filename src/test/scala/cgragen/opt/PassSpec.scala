package cgragen.opt

import cgragen.{Parameters, TestConfiguration}

import cgragen.cgra.CGRA

import cgragen.dfgparse.DFG

import cgragen.opt.passes._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

/** Pass and PassSequence tests */
class PassSpec extends AnyFlatSpec with TestConfiguration {
  behavior of "Pass sequence"

  it should "fail to generate invalid sequences" in {
    // Both pre- and post-requisite pass
    case object Dupl extends Pass(Seq(IntegrityCheckPass), Seq(IntegrityCheckPass)) {
      def apply(dfg: DFG, cgra: CGRA)(implicit conf: Parameters): Unit = ???
    }
    intercept[IllegalArgumentException] { new PassSequence(Seq(Dupl)) }

    /** 
     * Defining inter-dependent passes causes stack overflow. Perhaps it is 
     * simply not an issue? Either way the code should be resistent to such 
     * examples already.
     */
  }

  it should "generate properly sorted sequences" in {
    // Realistic example from the other tests
    val cPass = new PassSequence(Seq(PropagateConstPass, IntegrityCheckPass))
    cPass.order should have size 3

    // Another realistic example from the other tests
    val locParams = params.copy(dfgPropagateConstants = true, approximateArithmetic = true)
    val aPass = getOptPasses()(locParams)
    aPass.order.map(_.toString()).toSet should (
      have size 4 and
      contain allElementsOf 
        Seq("PropagateConstPass", "RemoveDeadPass",
            "MarkApproximablePass", "IntegrityCheckPass"))

    /** 
     * As above, defining a pre- and post-requisite pair causes failure in 
     * building the sequence of passes. However, the failure seems not to 
     * stem from the actual functionality of PassSequence, but from how the 
     * inter-dependent singletons are constructed.
     */

    // More involved examples with both pre- and post-requisites
    case object A extends Pass(Seq.empty[Pass], Seq.empty[Pass]) {
      def apply(dfg: DFG, cgra: CGRA)(implicit conf: Parameters): Unit = ???
    }
    case object B extends Pass(Seq(A), Seq.empty[Pass]) {
      def apply(dfg: DFG, cgra: CGRA)(implicit conf: Parameters): Unit = ???
    }
    case object C extends Pass(Seq.empty[Pass], Seq(B)) {
      def apply(dfg: DFG, cgra: CGRA)(implicit conf: Parameters): Unit = ???
    }
    case object D extends Pass(Seq.empty[Pass], Seq(C)) {
      def apply(dfg: DFG, cgra: CGRA)(implicit conf: Parameters): Unit = ???
    }
    case object E extends Pass(Seq(B), Seq.empty[Pass]) {
      def apply(dfg: DFG, cgra: CGRA)(implicit conf: Parameters): Unit = ???
    }
    val dPass = new PassSequence(Seq(D))
    dPass.order should (
      have size 4 and // A and C/D can swap places, check one
      contain inOrderElementsOf Seq(D, C, B))
    val dePass = new PassSequence(Seq(D, E))
    dePass.order should (
      have size 5 and // A and C/D can swap places, check one
      contain inOrderElementsOf Seq(D, C, B, E))
  }
}
