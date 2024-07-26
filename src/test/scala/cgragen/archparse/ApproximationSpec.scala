package cgragen.archparse

import cgragen.TestConfiguration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class ApproximationSpec extends AnyFlatSpec with TestConfiguration {
  behavior of "Architecture parser"

  it should "capture approximation attributes on primitives" in {
    val temp = Architecture(
      <CGRA>
        <template name="pe">
          <input  name="in0"/>  <input  name="in1"/>
          <output name="out0"/> <output name="out1"/>
          <inst   name="fu"     module="FuncUnit" ops="add sub mul shl" approx="1"/>
          <inst   name="rgstr0" module="Register"/>
          <inst   name="rgstr1" module="Register"/>
          <connection from="this.in0" to="fu.in_a"/>
          <connection from="this.in1" to="fu.in_b"/>
          <connection from="fu.out" to="rgstr0.in"/>
          <connection from="rgstr0.out" to="this.out0"/>
          <connection select-from="this.in0 this.in1" to="rgstr1.in"/>
          <connection from="rgstr1.out" to="this.out1"/>
        </template>
        <architecture row="1" col="1"/>
      </CGRA>).modTemplates.head._2
    temp.primitives.map(_._1) should (
      have size 4 and
      contain allElementsOf Seq("FuncUnit", "Register", "Multiplexer"))
    temp.primitives.filter(_._1 == "FuncUnit").foreach { case (_, primArgs) =>
      primArgs.keys should contain ("approx")
      primArgs("approx") should be ("1")
    }
  }
}
