package cgragen.cgra

import cgragen.archparse.Architecture

import cgragen.cgra.primitives.{AbstractInputUnit, AbstractOutputUnit}

import cgragen.TestConfiguration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class ConversionSpec extends AnyFlatSpec with TestConfiguration {
  behavior of "Architecture-to-CGRA converter"

  val testArch = <CGRA>
    <template name="rgstr" size="16">
      <input name="in" size="24"/> <output name="out" size="42"/>
      <inst name="rgstr_int" module="Register" size="18"/>
      <inst name="mux_int" module="Multiplexer" ninput="2"/>
      <connection from="this.in" to="rgstr_int.in"/>
      <connection from="rgstr_int.out" to="mux_int.in0"/>
      <connection from="this.in" to="mux_int.in1"/>
      <connection from="mux_int.out" to="this.out"/>
    </template>
    <architecture row="1" col="1">
      <pattern row-range="0 0" col-range="0 0">
        <block module="rgstr"/> <!-- block_0_0 -->
      </pattern>
    </architecture>
  </CGRA>

  it should "propagate data sizes through conversion" in {
    val cgra = CGRA(Architecture(testArch))

    // Validate some overall characteristics first
    cgra.dataSize should be (params.DataSize)
    cgra.subModules.keys should (have size 1 and contain allElementsOf Seq("block_0_0"))

    // Then check that the instantiated module has the right parameters
    val block = cgra.subModules("block_0_0")

    block.ports.keys should (have size 2 and contain allElementsOf (Seq("in", "out")))
    block.ports("in").dataSize  should be (24)
    block.ports("out").dataSize should be (42)

    block.subModules.keys should contain allElementsOf (Seq("rgstr_int", "mux_int"))
    block.subModules("rgstr_int").dataSize should be (18)
    block.subModules("mux_int")  .dataSize should be (block.dataSize)
  }

  it should "fail to convert template with non-unique input name" in {
    intercept[DuplicateDefinitionException] { CGRA(Architecture(<CGRA>
        <template name="intf">
          <input  name="from_cgra"/> <output name="to_cgra"/>
          <inst   name="from_cgra" module="InputUnit"/> <!-- Invalid -->
          <inst   name="outport" module="OutputUnit"/>
          <connection from="this.from_cgra" to="outport.in"/>
          <connection from="from_cgra.out" to="this.to_cgra"/>
        </template>
        <architecture row="1" col="1">
          <pattern row-range="0 0" col-range="0 0">
            <block module="intf"/>
          </pattern>
        </architecture>
      </CGRA>)) }
  }

  it should "fail to convert hierarchical template with non-unique input name" in {
    intercept[DuplicateDefinitionException] { CGRA(Architecture(<CGRA>
        <template name="intf">
          <input  name="from_cgra"/> <output name="to_cgra"/>
          <inst   name="inport"  module="InputUnit"/>
          <inst   name="to_cgra_pt" module="OutputUnit"/> <!-- Invalid -->
          <connection from="this.from_cgra" to="to_cgra_pt.in"/>
          <connection from="inport.out" to="this.to_cgra"/>
        </template>
        <template name="pt">
          <input name="from_cgra_pt"/> <output name="to_cgra_pt"/>
          <submodule name="io" module="intf"/>
          <connection from="this.from_cgra_pt" to="io.from_cgra"/>
          <connection from="io.to_cgra" to="this.to_cgra_pt"/>
        </template>
        <architecture row="1" col="1">
          <pattern row-range="0 0" col-range="0 0">
            <block module="pt"/>
          </pattern>
        </architecture>
      </CGRA>)) }
  }

  val partialIOArch = <CGRA>
    <template name="pe">
      <input name="in1"/> <input name="in2"/>
      <output name="out_fu"/> <output name="out_rt"/>

      <!-- Invalid IO definition -->
      <inst name="io_in" module="InputUnit"/>
      <inst name="fu" module="FuncUnit" ops="add sub"/>
      <inst name="fu_reg" module="Register"/>
      <connection select-from="this.in1 io_in.out" to="fu.in_a"/>
      <connection from="this.in2" to="fu.in_b"/>
      <connection from="fu.out" to="fu_reg.in"/>
      <connection from="fu_reg.out" to="this.out_fu"/>

      <inst name="rt_reg" module="Register"/>
      <connection select-from="this.in1 this.in2" to="rt_reg.in"/>
      <connection from="rt_reg.out" to="this.out_rt"/>
    </template>
    <architecture row="2" col="2">
      <!-- Instantiating PEs -->
      <pattern row-range="0 1" col-range="0 1">
        <block module="pe"/> </pattern>

      <!-- Inter-PE connections -->
      <pattern row-range="0 0" col-range="0 1">
        <connection from="(rel 0 0).out_fu" to="(rel 1 0).in1"/>
        <connection from="(rel 1 0).out_rt" to="(rel 0 0).in2"/>
      </pattern>
    </architecture>
  </CGRA>

  it should "fail on partially specified IO" in {
    val locParams = params.copy(cgraInferTopLevelIO = true)
    intercept[TopLevelIOInferenceException] { CGRA(Architecture(partialIOArch))(locParams) }
  }

  it should "infer top-level IO" in {
    val locParams = params.copy(cgraInferTopLevelIO = true)
    val cgra = CGRA(Architecture(ioLessArch))(locParams)

    // Check that the architecture has the right modules
    val subModNames = (0 until 2).flatMap(r => (0 until 2).map(c => s"block_${r}_${c}")) :+ locParams.CGRATopLevelIOName
    cgra.subModules.keys should (
      have size (subModNames.size) and
      contain allElementsOf (subModNames))

    // Check the inferred IO
    val topLevelIO = (0 until 2).flatMap { c =>
      Seq(s"block_0_${c}_in1_inf", s"block_0_${c}_out_rt_inf",
          s"block_1_${c}_in2_inf", s"block_1_${c}_out_fu_inf")
    }
    cgra.subModules(locParams.CGRATopLevelIOName).ports.keys should (
      have size (topLevelIO.size) and
      contain allElementsOf (topLevelIO))
  }

  it should "infer top-level IO with wrap-around on columns" in {
    val locParams = params.copy(cgraInferTopLevelIO = true)
    val cgra = CGRA(Architecture(twoInTwoOutArchWrapCol))(locParams)

    // Check that the architecture has the right modules
    val subModNames = (0 until 4).flatMap(r => (0 until 4).map(c => s"block_${r}_${c}")) :+ locParams.CGRATopLevelIOName
    cgra.subModules.keys should (
      have size (subModNames.size) and
      contain allElementsOf (subModNames))

    // Check the inferred IO
    val topLevelIO = (0 until 4).flatMap { c =>
      Seq(s"block_0_${c}_in0_inf",  s"block_0_${c}_in1_inf",
          s"block_3_${c}_out0_inf", s"block_3_${c}_out1_inf")
    }
    cgra.subModules(locParams.CGRATopLevelIOName).ports.keys should (
      have size (topLevelIO.size) and
      contain allElementsOf (topLevelIO))
  }

  it should "infer top-level IO with wrap-around on rows" in {
    val locParams = params.copy(cgraInferTopLevelIO = true)
    val cgra = CGRA(Architecture(twoInTwoOutArchWrapRow))(locParams)

    // Check that the architecture has the right modules
    val subModNames = (0 until 4).flatMap(r => (0 until 4).map(c => s"block_${r}_${c}")) :+ locParams.CGRATopLevelIOName
    cgra.subModules.keys should (
      have size (subModNames.size) and
      contain allElementsOf (subModNames))

    // Check the inferred IO
    val topLevelIO = (0 until 4).flatMap { r =>
      Seq(s"block_${r}_0_in0_inf",  s"block_${r}_0_in1_inf",
          s"block_${r}_3_out0_inf", s"block_${r}_3_out1_inf")
    }
    cgra.subModules(locParams.CGRATopLevelIOName).ports.keys should (
      have size (topLevelIO.size) and
      contain allElementsOf (topLevelIO))
  }

  val pipeArch = <CGRA>
    <template name="pe">
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

    <template name="io">
      <input  name="from_cgra"/> <output name="to_cgra"/>
      <inst   name="in"  module="InputUnit"/>
      <inst   name="out" module="OutputUnit"/>
      <connection from="this.from_cgra" to="out.in"/>
      <connection from="in.out" to="this.to_cgra"/>
    </template>
    <architecture row="4" col="2">
      <!-- Instantiating PEs -->
      <pattern row-range="1 2" col-range="0 1">
        <block module="pe"/> </pattern>

      <!-- Instantiating I/Os -->
      <pattern row-range="0 0" col-range="0 1">
        <block module="io"/> </pattern>
      <pattern row-range="3 3" col-range="0 1">
        <block module="io"/> </pattern>
      
      <!-- I/O connections -->
      <pattern row-range="1 1" col-range="0 1">
        <connection from="(rel -1 0).to_cgra" to="(rel 0 0).in1"/>
        <connection from="(rel 0 0).out_rt" to="(rel -1 0).from_cgra"/>
      </pattern>
      <pattern row-range="2 2" col-range="0 1">
        <connection from="(rel 1 0).to_cgra" to="(rel 0 0).in2"/>
        <connection from="(rel 0 0).out_fu" to="(rel 1 0).from_cgra"/>
      </pattern>

      <!-- Inter-PE connections -->
      <pattern row-range="1 1" col-range="0 1">
        <connection from="(rel 0 0).out_fu" to="(rel 1 0).in1"/>
        <connection from="(rel 1 0).out_rt" to="(rel 0 0).in2"/>
      </pattern>
    </architecture>
  </CGRA>

  it should "list and order all configuration cells" in {
    val cgra = CGRA(Architecture(pipeArch))

    // Check that the architecture has the right modules
    val subModNames = (0 until 4).flatMap(r => (0 until 2).map(c => s"block_${r}_${c}"))
    cgra.subModules.keys should (have size 8 and contain allElementsOf (subModNames))

    // Get all the configuration cells
    val confCells     = cgra.configOrder
    val confCellNames = (1 until 3).flatMap(r => (0 until 2).flatMap { c =>
      Seq(s"block_${r}_${c}.fu_select", s"block_${r}_${c}.mux_rt_reg_in_select")
    })
    confCells should have size 8
    confCells.map(cell => s"${cell.port.parent.fullPath}_${cell.port.name}")
      .zip(confCellNames)
      .foldLeft(true) { case (acc, (rec, exp)) => acc && (rec == exp) } should be (true)
  }
}
