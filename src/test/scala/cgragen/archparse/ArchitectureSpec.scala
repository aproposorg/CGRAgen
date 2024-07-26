package cgragen.archparse

import java.io.{File, PrintWriter}

import cgragen.TestConfiguration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class ArchitectureSpec extends AnyFlatSpec with TestConfiguration {
  behavior of "Architecture parser"

  /** Creates a temporary file, writes the string to it, runs the parser 
   * on it, and deletes the file
   */
  def parseStringInFile(str: String) = {
    val file = File.createTempFile("invalid", ".xml")
    (new PrintWriter(file)).append(str).close()
    Architecture(file)
    file.delete()
  }

  it should "fail on missing and duplicate CGRA and architecture fields" in {
    /** Missing and duplicate CGRA fields */
    intercept[MissingFieldException] {
      Architecture(<architecture row="1" col="1">
                     <module name="buffer">
                       <input name="in"/>
                       <output name="out"/>
                       <connection from="this.in" to="this.out"/>
                     </module>
                     <block module="buffer"/>
                   </architecture>)
    }
    intercept[DuplicateFieldException] {
      Architecture(<CGRA>
                     <CGRA>
                       <architecture row="1" col="1"/>
                     </CGRA>
                     <architecture row="1" col="1"/>
                   </CGRA>)
    }

    /** Missing and duplicate architecture fields */
    intercept[MissingFieldException] {
      Architecture(<CGRA>
                     <module name="buffer">
                       <input name="in"/>
                       <output name="out"/>
                       <connection from="this.in" to="this.out"/>
                     </module>
                   </CGRA>)
    }
    intercept[DuplicateFieldException] {
      Architecture(<CGRA>
                     <architecture row="1" col="1"/>
                     <architecture row="1" col="1"/>
                   </CGRA>)
    }
  }

  it should "fail on missing and duplicate architecture information" in {
    intercept[MissingDataException] {
      Architecture(<CGRA>
                     <architecture col="1"/>
                   </CGRA>)
    }
    intercept[MissingDataException] {
      Architecture(<CGRA>
                     <architecture row="1"/>
                   </CGRA>)
    }
    intercept[IllegalArgumentException] {
      Architecture(<CGRA>
                     <architecture row="-1" col="1"/>
                   </CGRA>)
    }
  }

  it should "fail on missing and misplaced definition information" in {
    intercept[MissingDataException] {
      Architecture(<CGRA>
                     <definition/>
                     <architecture row="1" col="1"/>
                   </CGRA>)
    }
    intercept[MissingDataException] {
      Architecture(<CGRA>
                     <definition name="constant"/>
                     <architecture row="1" col="1"/>
                   </CGRA>)
    }

    intercept[MisplacedFieldException] {
      Architecture(<CGRA>
                     <definition name="constant" value="42"/>
                     <architecture row="1" col="1">
                       <definition name="local" value="42"/>
                     </architecture>
                   </CGRA>)
    }
  }

  it should "fail on missing, duplicate and misplaced module information" in {
    intercept[MissingDataException] { // template missing name
      Architecture(<CGRA>
                     <template/>
                     <architecture row="1" col="1"/>
                   </CGRA>)
    }
    intercept[MissingDataException] { // inst missing module type
      Architecture(<CGRA>
                     <template name="block">
                       <inst/>
                     </template>
                     <architecture row="1" col="1"/>
                   </CGRA>)
    }
    intercept[MissingDataException] { // sub-module missing name
      Architecture(<CGRA>
                     <template name="block">
                       <submodule/>
                     </template>
                     <architecture row="1" col="1"/>
                   </CGRA>)
    }
    intercept[MissingDataException] { // sub-module missing module type
      Architecture(<CGRA>
                     <template name="block">
                       <submodule name="block"/>
                     </template>
                     <architecture row="1" col="1"/>
                   </CGRA>)
    }

    intercept[DuplicateDefinitionException] { // duplicate template declaration
      Architecture(<CGRA>
                     <template name="block"/>
                     <template name="block"/>
                     <architecture row="1" col="1"/>
                   </CGRA>)
    }

    intercept[MisplacedFieldException] { // template declaration in architecture
      Architecture(<CGRA>
                     <template name="block"/>
                     <architecture row="1" col="1">
                       <template name="local">
                       </template>
                     </architecture>
                   </CGRA>)
    }

    intercept[MissingDataException] { // undefined sub-module type
      Architecture(
        <CGRA>
          <template name="passthrough">
            <input name="in"/> <output name="out"/>
            <submodule name="buf" module="buffer"/>
            <connection from="this.in" to="buf.in"/>
            <connection from="buf.out" to="this.out"/>
          </template>
          <architecture row="1" col="1"/>
        </CGRA>)
    }

    intercept[RecursiveDefinitionException] { // recursive dependency on buffer/passthrough
      Architecture(
        <CGRA>
          <template name="buffer">
            <input name="in"/> <output name="out"/>
            <submodule name="pt" module="passthrough"/>
            <connection from="this.in" to="pt.in"/>
            <connection from="pt.out" to="this.out"/>
          </template>
          <template name="passthrough">
            <input name="in"/> <output name="out"/>
            <submodule name="buf" module="buffer"/>
            <connection from="this.in" to="buf.in"/>
            <connection from="buf.out" to="this.out"/>
          </template>
          <architecture row="1" col="1"/>
        </CGRA>)
    }
  }

  it should "fail on missing and duplicate wire information" in {
    intercept[MissingDataException] {
      Architecture(
        <CGRA>
          <template name="block"> <wire/> </template>
          <architecture row="1" col="1"/>
        </CGRA>)
    }

    intercept[DuplicateDefinitionException] {
      Architecture(
        <CGRA>
          <template name="block">
            <wire name="cable"/> <wire name="cable"/>
          </template>
          <architecture row="1" col="1"/>
        </CGRA>)
    }
  }

  it should "fail on missing and duplicate port information" in {
    List("input", "output").foreach { portDirection =>
      intercept[MissingDataException] {
        parseStringInFile(s"""<CGRA>
                     |  <template name="block">
                     |    <$portDirection/>
                     |  </template>
                     |  <architecture row="1" col="1"/>
                     |</CGRA>""".stripMargin)
      }

      intercept[DuplicateDefinitionException] {
        parseStringInFile(s"""<CGRA>
                     |  <template name="block">
                     |    <$portDirection name="port"/>
                     |    <$portDirection name="port"/>
                     |  </template>
                     |  <architecture row="1" col="1"/>
                     |</CGRA>""".stripMargin)
      }
    }

    intercept[DuplicateDefinitionException] {
      Architecture(
        <CGRA>
          <template name="block">
            <input name="port"/> <output name="port"/>
          </template>
          <architecture row="1" col="1"/>
        </CGRA>)
    }
  }

  it should "fail on missing and duplicate connection information" in {
    List("to", "distribute-to").foreach { toType =>
      intercept[MissingDataException] {
        parseStringInFile(s"""<CGRA>
                     |  <template name="block">
                     |    <input name="in"/>
                     |    <output name="out"/>
                     |    <connection $toType="this.out"/>
                     |  </template>
                     |  <architecture row="1" col="1"/>
                     |</CGRA>""".stripMargin)
      }

      intercept[DuplicateDataException] {
        parseStringInFile(s"""<CGRA>
                     |  <template name="block">
                     |    <input name="in"/>
                     |    <output name="out"/>
                     |    <connection from="this.in" select-from="this.in" $toType="this.out"/>
                     |  </template>
                     |  <architecture row="1" col="1"/>
                     |</CGRA>""".stripMargin)
      }
    }

    List("from", "select-from").foreach { fromType => 
      intercept[MissingDataException] {
        parseStringInFile(s"""<CGRA>
                     |  <template name="block">
                     |    <input name="in"/>
                     |    <output name="out"/>
                     |    <connection $fromType="this.in"/>
                     |  </template>
                     |  <architecture row="1" col="1"/>
                     |</CGRA>""".stripMargin)
      }

      intercept[DuplicateDataException] {
        parseStringInFile(s"""<CGRA>
                     |  <template name="block">
                     |    <input name="in"/>
                     |    <output name="out"/>
                     |    <connection $fromType="this.in" to="this.out" distribute-to="this.out"/>
                     |  </template>
                     |  <architecture row="1" col="1"/>
                     |</CGRA>""".stripMargin)
      }
    }

    intercept[MissingDataException] { // invalid argument to sext
      Architecture(
        <CGRA>
          <template name="constwrap">
            <output name="out"/>
            <inst name="const" module="ConstUnit" size="16"/>
            <connection from="const.out" to="this.out" sext="hello"/>
          </template>
          <architecture row="1" col="1"/>
        </CGRA>)
    }

    intercept[MissingDataException] { // typo in connection to-argument
      Architecture(
        <CGRA>
          <template name="buffer">
            <input name="in"/> <output name="out"/>
            <wire name="conn"/>
            <connection from="this.in" to="conn"/>
            <connection from="conn" to="this.out"/>
          </template>
          <template name="passthrough">
            <input name="in"/> <output name="out"/>
            <submodule name="ptbuf" module="buffer"/>
            <connection from="this.in" to="ptbuf.in.conn"/>
            <connection from="ptbuf.out" to="this.out"/>
          </template>
          <architecture row="1" col="1"/>
        </CGRA>)
    }

    intercept[MissingDataException] { // missing input connection to ptbuf in passthrough
      Architecture(
        <CGRA>
          <template name="buffer">
            <input name="in"/> <output name="out"/>
            <wire name="conn"/>
            <connection from="this.in" to="conn"/>
            <connection from="conn" to="this.out"/>
          </template>
          <template name="passthrough">
            <input name="in"/> <output name="out"/>
            <submodule name="ptbuf" module="buffer"/>
            <connection from="ptbuf.out" to="this.out"/>
          </template>
          <architecture row="1" col="1"/>
        </CGRA>)
    }
  }

  it should "fail on missing pattern information" in {
    val file = File.createTempFile("invalid", ".xml")
    intercept[MissingDataException] {
      Architecture(
        <CGRA>
          <architecture row="1" col="1">
            <pattern/>
          </architecture>
        </CGRA>)
    }
    intercept[MissingDataException] {
      Architecture(
        <CGRA>
          <architecture row="1" col="1">
            <pattern row-range="0 0"/>
          </architecture>
        </CGRA>)
    }
    intercept[MissingDataException] { // missing end column on range
      Architecture(
        <CGRA>
          <architecture row="1" col="1">
            <pattern row-range="0 0" col-range="0"/>
          </architecture>
        </CGRA>)
    }

    intercept[DuplicateDefinitionException] { // two counters with the same name
      Architecture(
        <CGRA>
          <architecture row="1" col="1">
            <pattern row-range="0 0" col-range="0 0" counter="i" row-counter="i"/>
          </architecture>
        </CGRA>)
    }
    intercept[MissingDataException] { // missing counter definition (and modules, but they are parsed later)
      Architecture(
        <CGRA>
          <architecture row="2" col="2">
            <pattern row-range="0 1" col-range="1 1">
              <connection from="(rel -(row) -1).out(row)" to="(rel 0 0).in"/>
            </pattern>
          </architecture>
        </CGRA>)
    }
    intercept[DuplicateDefinitionException] { // counter and definition with the same name
      Architecture(
        <CGRA>
          <definition name="cnt" value="42"/>
          <architecture row="1" col="1">
            <pattern row-range="0 0" col-range="0 0" counter="cnt"/>
          </architecture>
        </CGRA>)
    }

    intercept[MissingDataException] { // sub-nodule is missing module type
      Architecture(
        <CGRA>
          <architecture row="1" col="1">
            <pattern row-range="0 0" col-range="0 0">
              <block/>
            </pattern>
          </architecture>
        </CGRA>)
    }
    intercept[MissingFieldException] { // no module template for sub-module
      Architecture(
        <CGRA>
          <architecture row="1" col="1">
            <pattern row-range="0 0" col-range="0 0">
              <block module="block"/>
            </pattern>
          </architecture>
        </CGRA>)
    }
    intercept[DuplicateFieldException] { // too many blocks per pattern iteration
      Architecture(
        <CGRA>
          <template name="block">
            <input name="in"/> <output name="out"/>
            <connection from="this.in" to="this.out"/>
          </template>
          <architecture row="1" col="1">
            <pattern row-range="0 0" col-range="0 0">
              <block module="block"/> <block module="block"/>
            </pattern>
          </architecture>
        </CGRA>)
    }
  }

  it should "fail on invalid identifiers" in {
    intercept[InvalidIdentifierException] { // invalid module name
      Architecture(<CGRA>
                     <template name="reg"/>
                     <architecture row="1" col="1"/>
                   </CGRA>)
    }
    intercept[InvalidIdentifierException] { // invalid port name
      Architecture(<CGRA>
                     <template name="block">
                       <input name="input"/>
                     </template>
                     <architecture row="1" col="1"/>
                   </CGRA>)
    }
    intercept[InvalidIdentifierException] { // invalid wire name
      Architecture(<CGRA>
                     <template name="block">
                       <wire name="wire"/>
                     </template>
                     <architecture row="1" col="1"/>
                   </CGRA>)
    }
  }

  it should "fail on unconnected input ports" in {
    intercept[MissingDataException] { // unconnected primitive port
      Architecture(
        <CGRA>
          <template name="rgstr">
            <input name="in"/> <output name="out"/>
            <inst name="rgstr_int" module="Register"/>
            <connection from="rgstr_int.out" to="this.out"/>
          </template>
          <architecture row="1" col="1"/>
        </CGRA>)
    }
    intercept[MissingDataException] { // unconnected sub-module port
      Architecture(
        <CGRA>
          <template name="rgstr">
            <input name="in"/> <output name="out"/>
            <inst name="rgstr_int" module="Register"/>
            <connection from="this.in" to="rgstr_int.in"/>
            <connection from="rgstr_in.out" to="this.out"/>
          </template>
          <template name="shift_register">
            <input name="in"/> <output name="out"/>
            <submodule name="rgstr0" module="rgstr"/>
            <submodule name="rgstr1" module="rgstr"/>
            <connection from="this.in" to="rgstr0.in"/>
            <connection from="rgstr1.out" to="this.out"/>
          </template>
          <architecture row="1" col="1"/>
        </CGRA>)
    }
  }

  it should "fail on multiply-driven input ports" in {
    intercept[DuplicateDataException] { // multiply-driven primitive port
      Architecture(
        <CGRA>
          <template name="rgstr">
            <input  name="in0"/> <input name="in1"/>
            <output name="out"/>
            <inst name="rgstr_int" module="Register"/>
            <connection from="this.in0" to="rgstr_int.in"/>
            <connection from="this.in1" to="rgstr_int.in"/>
            <connection from="rgstr_int.out" to="this.out"/>
          </template>
          <architecture row="1" col="1"/>
        </CGRA>)
    }
    intercept[DuplicateDataException] { // multiply-driven sub-module port
      Architecture(
        <CGRA>
          <template name="rgstr">
            <input name="in"/> <output name="out"/>
            <inst name="rgstr_int" module="Register"/>
            <connection from="this.in" to="rgstr_int.in"/>
            <connection from="rgstr_int.out" to="this.out"/>
          </template>
          <template name="shift_register">
            <input name="in"/> <output name="out"/>
            <submodule name="rgstr0" module="rgstr"/>
            <submodule name="rgstr1" module="rgstr"/>
            <connection from="this.in" distribute-to="rgstr0.in rgstr1.in"/>
            <connection from="rgstr0.out" to="rgstr1.in"/>
            <connection from="rgstr1.out" to="this.out"/>
          </template>
          <architecture row="1" col="1"/>
        </CGRA>)
    }
  }

  it should "fail on invalid data sizes" in {
    intercept[MissingDataException] { // non-integral data size in template
      Architecture(
        <CGRA>
          <template name="rgstr" size="hello">
            <input name="in"/> <output name="out"/>
            <inst name="rgstr_int" module="Register"/>
            <connection from="this.in" to="rgstr_int.in"/>
            <connection from="rgstr_int.out" to="this.out"/>
          </template>
          <architecture row="1" col="1"/>
        </CGRA>)
    }
    intercept[NegativeDataSizeException] { // negative data size in template
      Architecture(
        <CGRA>
          <template name="rgstr" size="-1">
            <input name="in"/> <output name="out"/>
            <inst name="rgstr_int" module="Register"/>
            <connection from="this.in" to="rgstr_int.in"/>
            <connection from="rgstr_int.out" to="this.out"/>
          </template>
          <architecture row="1" col="1"/>
        </CGRA>)
    }

    intercept[MissingDataException] { // non-integral port size
      Architecture(
        <CGRA>
          <template name="rgstr">
            <input name="in" size="hello"/> <output name="out"/>
            <inst name="rgstr_int" module="Register"/>
            <connection from="this.in" to="rgstr_int.in"/>
            <connection from="rgstr_int.out" to="this.out"/>
          </template>
          <architecture row="1" col="1"/>
        </CGRA>)
    }
    intercept[NegativeDataSizeException] { // negative port size
      Architecture(
        <CGRA>
          <template name="rgstr">
            <input name="in" size="-1"/> <output name="out"/>
            <inst name="rgstr_int" module="Register"/>
            <connection from="this.in" to="rgstr_int.in"/>
            <connection from="rgstr_int.out" to="this.out"/>
          </template>
          <architecture row="1" col="1"/>
        </CGRA>)
    }

    intercept[MissingDataException] { // non-integral data size in primitive instance
      Architecture(
        <CGRA>
          <template name="rgstr">
            <input name="in"/> <output name="out"/>
            <inst name="rgstr_int" module="Register" size="hello"/>
            <connection from="this.in" to="rgstr_int.in"/>
            <connection from="rgstr_int.out" to="this.out"/>
          </template>
          <architecture row="1" col="1"/>
        </CGRA>)
    }
    intercept[NegativeDataSizeException] { // negative data size in primitive instance
      Architecture(
        <CGRA>
          <template name="rgstr">
            <input name="in"/> <output name="out"/>
            <inst name="rgstr_int" module="Register" size="-1"/>
            <connection from="this.in" to="rgstr_int.in"/>
            <connection from="rgstr_int.out" to="this.out"/>
          </template>
          <architecture row="1" col="1"/>
        </CGRA>)
    }

    intercept[NegativeDataSizeException] { // negative data size in template from definition
      Architecture(
        <CGRA>
          <definition name="width" value="-1"/>
          <template name="rgstr" size="width">
            <input name="in"/> <output name="out"/>
            <inst name="rgstr_int" module="Register"/>
            <connection from="this.in" to="rgstr_int.in"/>
            <connection from="rgstr_int.out" to="this.out"/>
          </template>
          <architecture row="1" col="1"/>
        </CGRA>)
    }
    intercept[NegativeDataSizeException] { // negative port size from definition
      Architecture(
        <CGRA>
          <definition name="width" value="-1"/>
          <template name="rgstr">
            <input name="in"/> <output name="out" size="width"/>
            <inst name="rgstr_int" module="Register"/>
            <connection from="this.in" to="rgstr_int.in"/>
            <connection from="rgstr_int.out" to="this.out"/>
          </template>
          <architecture row="1" col="1"/>
        </CGRA>)
    }
    intercept[NegativeDataSizeException] { // negative data size in primitive instance from definition
      Architecture(
        <CGRA>
          <definition name="width" value="-1"/>
          <template name="rgstr">
            <input name="in"/> <output name="out"/>
            <inst name="rgstr_int" module="Register" size="width"/>
            <connection from="this.in" to="rgstr_int.in"/>
            <connection from="rgstr_int.out" to="this.out"/>
          </template>
          <architecture row="1" col="1"/>
        </CGRA>)
    }
  }

  it should "catch constant data sizes in templates" in {
    val temp = Architecture(
      <CGRA>
        <template name="rgstr" size="16">
          <input name="in" size="24"/> <output name="out" size="42"/>
          <inst name="rgstr_int" module="Register" size="18"/>
          <inst name="mux_int" module="Multiplexer" ninput="2"/>
          <connection from="this.in" to="rgstr_int.in"/>
          <connection from="rgstr_int.out" to="mux_int.in0"/>
          <connection from="this.in" to="mux_int.in1"/>
          <connection from="mux_int.out" to="this.out"/>
        </template>
        <architecture row="1" col="1"/>
      </CGRA>).modTemplates.head._2
    temp.dataSize should be (16)
    temp.ports.keys should (
      have size 2 and
      contain allElementsOf Seq("in", "out"))
    temp.ports("in")._2  should be (24)
    temp.ports("out")._2 should be (42)
    temp.primitives.map(_._1) should (
      have size 2 and
      contain allElementsOf Seq("Register", "Multiplexer"))
    temp.primitives.filter(_._1 == "Register").foreach { case (_, primArgs) =>
      primArgs.keys should contain ("size")
      primArgs("size") should be ("18") }
    temp.primitives.filter(_._1 == "Multiplexer").foreach { case (_, primArgs) =>
      primArgs.keys should contain ("size")
      primArgs("size") should be ("16") }
  }

  it should "correctly identify top-level templates" in {
    val temps = Architecture(
      <CGRA>
        <template name="pe1">
          <input name="in"/> <output name="out"/>
          <submodule name="rgstr" module="rgstr"/>
          <connection from="this.in" to="rgstr.in"/>
          <connection from="rgstr.out" to="this.out"/>
        </template>
        <template name="rgstr">
          <input name="in"/> <output name="out"/>
          <inst name="rgstr_int" module="Register"/>
          <connection from="this.in" to="rgstr_int.in"/>
          <connection from="rgstr_int.out" to="this.out"/>
        </template>
        <template name="pe2">
          <input name="in1"/> <input name="in2"/>
          <output name="out_fu"/> <output name="out_rt"/>

          <inst name="fu" module="FuncUnit" ops="add sub"/>
          <inst name="fu_reg" module="Register"/>
          <connection from="this.in1" to="fu.in_a"/>
          <connection from="this.in2" to="fu.in_b"/>
          <connection from="fu.out" to="fu_reg.in"/>
          <connection from="fu_reg.out" to="this.out_fu"/>

          <inst name="rt_reg" module="Register"/>
          <connection select-from="this.in1 this.in2" to="rt_reg.in"/>
          <connection from="rt_reg.out" to="this.out_rt"/>
        </template>
        <architecture row="1" col="1"/>
      </CGRA>).getTopModTemplates()
    temps.keys should (have size 2 and contain allElementsOf (Seq("pe1", "pe2")))
  }

  it should "parse patterns with wrap-around on columns" in {
    val arch = Architecture(twoInTwoOutArchWrapCol)
    val subModNames = (0 until 4).flatMap(r => (0 until 4).map(c => s"block_${r}_${c}"))
    arch.subModules.keys should (
      have size (subModNames.size) and
      contain allElementsOf (subModNames))
    val subModConns = (0 until 3).flatMap { r =>
      (0 until 4).flatMap { c =>
        Seq((s"block_${r}_${c}.out0", s"block_${r+1}_${c}.in0"),
            (s"block_${r}_${c}.out1", s"block_${r+1}_${(c + 2) % 4}.in1"))
      }
    }
    arch.connections.map { case Connection(_, toPort, _, fromPort, _) => (fromPort, toPort) } should (
      have size (subModConns.size) and
      contain allElementsOf (subModConns))
  }

  it should "parse patterns with wrap-around on rows" in {
    val arch = Architecture(twoInTwoOutArchWrapRow)
    val subModNames = (0 until 4).flatMap(r => (0 until 4).map(c => s"block_${r}_${c}"))
    arch.subModules.keys should (
      have size (subModNames.size) and
      contain allElementsOf (subModNames))
    val subModConns = (0 until 4).flatMap { r =>
      (0 until 3).flatMap { c =>
        Seq((s"block_${r}_${c}.out0", s"block_${r}_${c+1}.in0"),
            (s"block_${r}_${c}.out1", s"block_${(r + 2) % 4}_${c+1}.in1"))
      }
    }
    arch.connections.map { case Connection(_, toPort, _, fromPort, _) => (fromPort, toPort) } should (
      have size (subModConns.size) and
      contain allElementsOf (subModConns))
  }
}
