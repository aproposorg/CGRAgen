package cgragen

import scala.language.implicitConversions

trait TestConfiguration {
  implicit val params = cgragen.Parameters(cgragen.getConfParams())

  implicit def file2Str(file: java.io.File) = file.toString

  val ioLessArch = <CGRA>
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

  val twoInTwoOutArchWrapCol = <CGRA>
    <!-- Declare a simple PE with two inputs and two outputs. -->
    <template name="simplePE">
      <input  name="in0"/>  <input  name="in1"/>
      <output name="out0"/> <output name="out1"/>
      <inst name="rgstr" module="Register"/>
      <inst name="alu" module="FuncUnit" ops="add sub mul div and or xor shl shra shrl"/>
      <connection from="alu.out" to="rgstr.in"/>
      <connection select-from="this.in0 this.in1 rgstr.out" to="alu.in_a"/>
      <connection select-from="this.in0 this.in1 rgstr.out" to="alu.in_b"/>
      <connection from="rgstr.out" distribute-to="this.out0 this.out1"/>
    </template>

    <!-- Declare a simple 4x4 architecture with top-down-connected PEs -->
    <architecture row="4" col="4">
      <!-- Instantiating PEs -->
      <pattern row-range="0 3" col-range="0 3">
        <block module="simplePE"/>
      </pattern>

      <!-- Vertical connections -->
      <pattern row-range="0 2" col-range="0 3" wrap-col="1">
        <connection from="(rel 0 0).out0" to="(rel 1 0).in0"/>
        <connection from="(rel 0 0).out1" to="(rel 1 2).in1"/>
      </pattern>
    </architecture>
  </CGRA>

  val twoInTwoOutArchWrapRow = <CGRA>
    <!-- Declare a simple PE with two inputs and two outputs. -->
    <template name="simplePE">
      <input  name="in0"/>  <input  name="in1"/>
      <output name="out0"/> <output name="out1"/>
      <inst name="rgstr" module="Register"/>
      <inst name="alu" module="FuncUnit" ops="add sub mul div and or xor shl shra shrl"/>
      <connection from="alu.out" to="rgstr.in"/>
      <connection select-from="this.in0 this.in1 rgstr.out" to="alu.in_a"/>
      <connection select-from="this.in0 this.in1 rgstr.out" to="alu.in_b"/>
      <connection from="rgstr.out" distribute-to="this.out0 this.out1"/>
    </template>

    <!-- Declare a simple 4x4 architecture with left-right-connected PEs -->
    <architecture row="4" col="4">
      <!-- Instantiating PEs -->
      <pattern row-range="0 3" col-range="0 3">
        <block module="simplePE"/>
      </pattern>

      <!-- Vertical connections -->
      <pattern row-range="0 3" col-range="0 2" wrap-row="1">
        <connection from="(rel 0 0).out0" to="(rel 0 1).in0"/>
        <connection from="(rel 0 0).out1" to="(rel 2 1).in1"/>
      </pattern>
    </architecture>
  </CGRA>

  val fourInFourOutArch = <CGRA>
    <!-- Declare a simple PE with four inputs and four outputs. Inputs and outputs are
         numbered as follows: 0 = north, 1 = east, 2 = south, and 3 = west. -->
    <template name="simplePE">
      <input  name="in0"/>  <input  name="in1"/>  <input  name="in2"/>  <input  name="in3"/>
      <output name="out0"/> <output name="out1"/> <output name="out2"/> <output name="out3"/>
      <inst name="rgstr" module="Register"/>
      <inst name="alu" module="FuncUnit" ops="add sub mul div and or xor shl shra shrl"/>
      <connection from="alu.out" to="rgstr.in"/>
      <connection select-from="this.in0 this.in1 this.in2 this.in3 rgstr.out" to="alu.in_a"/>
      <connection select-from="this.in0 this.in1 this.in2 this.in3 rgstr.out" to="alu.in_b"/>
      <connection from="rgstr.out" distribute-to="this.out0 this.out1 this.out2 this.out3"/>
    </template>

    <!-- Declare a simple 4x4 architecture with mesh-connected PEs -->
    <architecture row="4" col="4">
      <!-- Instantiating PEs -->
      <pattern row-range="0 3" col-range="0 3">
        <block module="simplePE"/>
      </pattern>

      <!-- Vertical connections -->
      <pattern row-range="0 2" col-range="0 3">
        <connection from="(rel 0 0).out2" to="(rel 1 0).in0"/>
        <connection from="(rel 1 0).out0" to="(rel 0 0).in2"/>
      </pattern>

      <!-- Horizontal connections -->
      <pattern row-range="0 3" col-range="0 2">
        <connection from="(rel 0 0).out1" to="(rel 0 1).in3"/>
        <connection from="(rel 0 1).out3" to="(rel 0 0).in1"/>
      </pattern>
    </architecture>
  </CGRA>

  val nonApprxmblArch = <CGRA>
    <template name="pe">
      <input  name="in0"/>  <input  name="in1"/>
      <output name="out0"/> <output name="out1"/>
      <inst   name="fu"     module="FuncUnit" ops="shl shra shrl"/>
      <inst   name="rgstr0" module="Register"/>
      <inst   name="rgstr1" module="Register"/>
      <connection from="this.in0" to="fu.in_a"/>
      <connection from="this.in1" to="fu.in_b"/>
      <connection from="fu.out" to="rgstr0.in"/>
      <connection from="rgstr0.out" to="this.out0"/>
      <connection select-from="this.in0 this.in1" to="rgstr1.in"/>
      <connection from="rgstr1.out" to="this.out1"/>
    </template>
    <architecture row="1" col="1">
      <pattern row-range="0 0" col-range="0 0">
        <block module="pe"/> <!-- block_0_0 -->
      </pattern>
    </architecture>
  </CGRA>

  val apprxmblArch = <CGRA>
    <template name="pe0">
      <input  name="in0"/>  <input  name="in1"/>
      <output name="out0"/> <output name="out1"/>
      <inst   name="fu"     module="FuncUnit" ops="mul shl shra shrl"/>
      <inst   name="rgstr0" module="Register"/>
      <inst   name="rgstr1" module="Register"/>
      <connection from="this.in0" to="fu.in_a"/>
      <connection from="this.in1" to="fu.in_b"/>
      <connection from="fu.out" to="rgstr0.in"/>
      <connection from="rgstr0.out" to="this.out0"/>
      <connection select-from="this.in0 this.in1" to="rgstr1.in"/>
      <connection from="rgstr1.out" to="this.out1"/>
    </template>
    <template name="pe1">
      <input  name="in0"/>  <input  name="in1"/>
      <output name="out0"/> <output name="out1"/>
      <inst   name="fu"    module="FuncUnit" ops="add div and or xor" approx="1"/>
      <inst   name="rgstr" module="Register"/>
      <connection from="this.in0" to="fu.in_a"/>
      <connection from="this.in1" to="fu.in_b"/>
      <connection from="fu.out" to="rgstr.in"/>
      <connection from="rgstr.out" distribute-to="this.out0 this.out1"/>
    </template>
    <architecture row="2" col="2">
      <pattern row-range="0 0" col-range="0 1">
        <block module="pe0"/>
      </pattern>
      <pattern row-range="1 1" col-range="0 1">
        <block module="pe1"/>
      </pattern>
      <pattern row-range="0 0" col-range="0 1" wrap-col="1">
        <connection from="(rel 0 0).out0" to="(rel 1 0).in0"/>
        <connection from="(rel 0 0).out1" to="(rel 1 1).in1"/>
      </pattern>
      <pattern row-range="1 1" col-range="0 1" wrap-row="1">
        <connection from="(rel 0 0).out1" to="(rel 1 0).in1"/>
      </pattern>
    </architecture>
  </CGRA>
}
