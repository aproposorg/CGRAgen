package cgragen.hwgen

import cgragen.archparse.Architecture

import cgragen.cgra.{
  AbstractModule, AbstractOperation, CGRA, confWidth, isHWApproximable, log2Ceil
}
import cgragen.cgra.primitives._

import cgragen.dfgparse.Opcode._

import cgragen.hwgen.primitives._
import cgragen.hwgen.ConfigurationType._

import cgragen.TestConfiguration

import chisel3.getVerilogString

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class ConversionSpec extends AnyFlatSpec with TestConfiguration with HWSpecUtils {
  behavior of "Hardware generation"

  /** To be run after the Architecture-to-CGRA conversion tests. Validates 
   * that abstract CGRA components can be successfully converted to hardware 
   * descriptions. For now, we only check for high-level equivalence. 
   * 
   * Automated tests for primitives are available in `PrimitiveSpec.scala`, 
   * and equivalents for composite modules are in `CompositeSpec.scala`.
   */

  // We can only execute tests given that the maximum data size is greater 
  // than the number of configurations explored
  assert(NoConfs <= MaxDataSize)

  /** 
   * Run tests for converting primitives
   */

  it should "fail to generate non-composite modules" in {
    the [Exception] thrownBy(
      HWGen(AbstractMultiplexerUnit("mux", 2, 32))
    ) should have message ("requirement failed: non-composite modules cannot be at the top-level")
  }

  rng.shuffle(1 to MaxDataSize).take(NoConfs).foreach { dataSize =>
    it should s"generate constant units of width $dataSize" in {
      val constName = s"const_${dataSize}b"
      val abstractConst = AbstractConstantUnit(constName, dataSize)
      val constVerilog  = getVerilogString(new ConstantUnit(abstractConst))
      constVerilog should (
        include regex (moduleRegex(constName)) and
        include regex (portRegex("in",  "ctrl_ins_const", dataSize)) and  // control input
        include regex (portRegex("out", "io_outs_out",    dataSize))      // data output
      )
    }
  }

  it should "fail to generate function unit with unsupported operation" in {
    the [Exception] thrownBy(
      getVerilogString(new FunctionUnit(AbstractFunctionUnit("func", Seq(OpLoad), Seq(1), Seq(0))))
    ) should have message ("cannot generate hardware for operation load")
  }

  it should "fail to generate function unit with initiation interval > 1" in {
    the [AssertionError] thrownBy(
      getVerilogString(new FunctionUnit(AbstractFunctionUnit("func", Seq(OpDiv), Seq(4), Seq(0))))
    ) should have message ("assumption failed: function unit currently only supports initiation interval 1")
  }

  rng.shuffle(1 to MaxDataSize).take(NoConfs).foreach { dataSize =>
    val numOps = rng.nextInt(SupportedOps.size) + 1
    val ops    = rng.shuffle(SupportedOps).take(numOps)
    val iis    = Seq.fill(numOps)(1) // @todo randomize these when supported!
    val lats   = Seq.fill(numOps) { rng.nextInt(5) }
    val operations = (0 until numOps).map(i => (ops(i), iis(i), lats(i)))
    it should s"generate function units of width $dataSize" in {
      val funcName = s"func_${dataSize}b"
      val abstractFU = AbstractFunctionUnit(funcName, ops, iis, lats, size=dataSize)
      val fuVerilog  = getVerilogString(new FunctionUnit(abstractFU))
      fuVerilog should include regex (moduleRegex(funcName))
      operations
        .foreach   { case (op, ii, lat) =>
          fuVerilog should include regex (moduleRegex(s"${op}_ii${ii}_lat${lat}")) }
      if (operations.size > 1) {
        fuVerilog should include regex (
          portRegex("in", "ctrl_ins_select", log2Ceil(operations.size))) // control input
      }
      fuVerilog should (
        include regex (portRegex("in", "io_ins_in_a", dataSize)) and  // data inputs
        include regex (portRegex("in", "io_ins_in_b", dataSize)) and
        include regex (portRegex("out", "io_outs_out", dataSize)))    // data output
    }
  }

  rng.shuffle(1 to MaxDataSize).take(NoConfs).foreach { dataSize =>
    val muxSize  = rng.nextInt(15) + 2
    it should s"generate $muxSize-input multiplexers of width $dataSize" in {
      val muxName = s"mux${muxSize}_${dataSize}b"
      val abstractMux = AbstractMultiplexerUnit(muxName, muxSize, dataSize)
      val muxVerilog  = getVerilogString(new Multiplexer(abstractMux))
      muxVerilog should include regex (moduleRegex(muxName))
      (0 until muxSize).foreach { i =>
        muxVerilog should include regex (portRegex("in", s"io_ins_in$i", dataSize))           // data inputs
      }
      muxVerilog should include regex (portRegex("in", "ctrl_ins_select", log2Ceil(muxSize))) // control input
      muxVerilog should include regex (portRegex("out", "io_outs_out", dataSize))             // data output
    }
  }

  rng.shuffle(1 to MaxDataSize).take(NoConfs).foreach { dataSize =>
    it should s"generate registers of width $dataSize" in {
      val regName = s"reg_${dataSize}b"
      val abstractReg = AbstractRegisterUnit(regName, dataSize)
      val regVerilog  = getVerilogString(new Register(abstractReg))
      regVerilog should include regex (moduleRegex(regName))
      regVerilog should (
        include regex (portRegex("in",  "io_ins_in",   dataSize)) and // data input
        include regex (portRegex("out", "io_outs_out", dataSize))     // data output
      )
    }
  }

  rng.shuffle(1 to MaxDataSize).take(NoConfs).foreach { dataSize =>
    val numInputs  = rng.nextInt(4) + 1
    val numOutputs = rng.nextInt(4) + 1
    val numRegsLg2 = rng.nextInt(5) + 1
    it should s"generate $numInputs-input $numOutputs-output register files with ${1 << numRegsLg2} registers of width $dataSize" in {
      val rfName = s"rf_in${numInputs}_out${numOutputs}_reg${1 << numRegsLg2}_${dataSize}b"
      val abstractRF = AbstractRegisterFileUnit(rfName, numInputs, numOutputs, numRegsLg2, dataSize)
      val rfVerilog  = getVerilogString(new RegisterFile(abstractRF))
      rfVerilog should include regex (moduleRegex(rfName))
      (0 until numInputs).foreach { i =>
        rfVerilog should (
          include regex (portRegex("in", s"ctrl_ins_addr_in$i", numRegsLg2)) and // input ports
          include regex (portRegex("in", s"ctrl_ins_WE$i",      1)) and
          include regex (portRegex("in", s"io_ins_in$i",        dataSize)))
      }
      (0 until numOutputs).foreach { i =>
        rfVerilog should (
          include regex (portRegex("in",  s"ctrl_ins_addr_out$i", numRegsLg2)) and // output ports
          include regex (portRegex("out", s"io_outs_out$i",       dataSize)))
      }
    }
  }

  /** 
   * Run tests for converting composite modules
   */

  it should "generate configuration-free templates" in {
    val arch = Architecture(
      <CGRA>
        <template name="intf">
          <input  name="from_cgra"/> <output name="to_cgra"/>
          <inst   name="inport"  module="InputUnit"/>
          <inst   name="outport" module="OutputUnit"/>
          <connection from="this.from_cgra" to="outport.in"/>
          <connection from="inport.out" to="this.to_cgra"/>
        </template>
        <architecture row="1" col="1"/>
      </CGRA>)

    // Validate that it only has one module template and convert it to a 
    // corresponding Verilog description
    arch.modTemplates.keys should (have size 1 and contain ("intf"))
    val aIO = AbstractModule("intf", arch.modTemplates("intf"), arch)
    val ioV = HWGen(aIO)

    // Verify that the required data ports exist
    ioV should (
      include regex (portRegex("in", "io_ins_from_cgra", params.DataSize)) and
      include regex (portRegex("in", "io_ins_inport",    params.DataSize)) and
      include regex (portRegex("out", "io_outs_to_cgra", params.DataSize)) and
      include regex (portRegex("out", "io_outs_outport", params.DataSize)))
  }

  it should "pass inputs and outputs through hierarchy" in {
    val arch = Architecture(
      <CGRA>
        <template name="intf">
          <input  name="from_cgra"/> <output name="to_cgra"/>
          <submodule name="sub_intf_in"  module="intf_in"/>
          <submodule name="sub_intf_out" module="intf_out"/>
          <connection from="this.from_cgra" to="sub_intf_out.from_cgra"/>
          <connection from="sub_intf_in.to_cgra" to="this.to_cgra"/>
        </template>
        <template name="intf_in">
          <output name="to_cgra"/>
          <inst name="inport" module="InputUnit"/>
          <connection from="inport.out" to="this.to_cgra"/>
        </template>
        <template name="intf_out">
          <input name="from_cgra"/>
          <inst name="outport" module="OutputUnit"/>
          <connection from="this.from_cgra" to="outport.in"/>
        </template>
        <architecture row="1" col="1"/>
      </CGRA>)

    // Validate that it has three module templates and convert the top
    // template to its corresponding Verilog description
    arch.modTemplates.keys should (have size 3 and contain allElementsOf Seq("intf", "intf_in", "intf_out"))
    val aIO = AbstractModule("intf", arch.modTemplates("intf"), arch)
    val ioV = HWGen(aIO)

    // Verify that the correct sub-modules exist
    ioV should (
      include regex (moduleRegex("intf")) and
      include regex (moduleRegex("intf_sub_intf_in")) and
      include regex (moduleRegex("intf_sub_intf_out")))

    // Verify that the required data ports exist
    ioV should (
      include regex (portRegex("in", "io_ins_from_cgra", params.DataSize)) and
      include regex (portRegex("in", "io_ins_inport",    params.DataSize)) and
      include regex (portRegex("out", "io_outs_to_cgra", params.DataSize)) and
      include regex (portRegex("out", "io_outs_outport", params.DataSize)))
  }

  it should "propagate data sizes" in {
    val arch = Architecture(
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
        <architecture row="1" col="1">
          <pattern row-range="0 0" col-range="0 0">
            <block module="rgstr"/> <!-- block_0_0 -->
          </pattern>
        </architecture>
      </CGRA>)

    // Validate that it only has one module template and convert it to a 
    // corresponding Verilog description
    arch.modTemplates.keys should (have size 1 and contain ("rgstr"))
    val aRgstr = AbstractModule("pe", arch.modTemplates("rgstr"), arch)
    val rgstrV = HWGen(aRgstr)

    // Verify that the correct sub-modules exist
    rgstrV should (
      include regex (moduleRegex("pe")) and
      include regex (moduleRegex("pe_rgstr_int")) and
      include regex (moduleRegex("pe_mux_int")))

    // Verify that the correct top-level ports exist
    rgstrV should (
      include regex (portRegex("in",  "io_in",  24)) and
      include regex (portRegex("out", "io_out", 42)))

    // Verify that the sub-modules have the right sizes
    rgstrV should (
      include regex (portRegex("in",  "io_ins_in",   18)) and // register unit
      include regex (portRegex("out", "io_outs_out", 18)) and
      include regex (portRegex("in",  "io_ins_in0",  16)) and // multiplexer unit
      include regex (portRegex("in",  "io_ins_in1",  16)) and
      include regex (portRegex("out", "io_outs_out", 16)))
  }

  it should "convert simple templates" in {
    // Grab the example architecture and parse it
    val simpleArch = Architecture("src/test/resources/archparse/simple_arch.xml")

    // Validate that it only has one module template and convert it to a 
    // corresponding Verilog description
    simpleArch.modTemplates.keys should (have size 1 and contain ("pe"))
    val aPE = AbstractModule("pe", simpleArch.modTemplates("pe"), simpleArch)
    val peV = HWGen(aPE)

    // Verify that the correct sub-modules exist
    peV should (
      include regex (moduleRegex("pe")) and       // top module
      include regex (moduleRegex("pe_rgstr")) and // register unit
      include regex (moduleRegex("pe_func"))  and // function unit
      include regex (moduleRegex("pe_const")) and // constant unit
      include regex (moduleRegex("pe_mux_func_in_a")) and
      include regex (moduleRegex("pe_mux_func_in_b")) and
      include regex (moduleRegex("pe_mux_rgstr_in")))

    // Verify that the correct top-level ports exist
    (0 until 4).foreach { i =>
      peV should include regex (portRegex("in", s"io_ins_in$i", params.DataSize)) // data input
    }
    peV should (
      include regex (portRegex("out", "io_outs_out",     params.DataSize)) and // data output
      include regex (portRegex("in", "ctrl_ins_conf",    confWidth(aPE))) and  // control input
      include regex (portRegex("in", "ctrl_ins_conf_en", 1)))
  }

  it should "convert complex templates" in {
    // Grab the example architecture and parse it
    val complexArch = Architecture("src/test/resources/archparse/complex_arch.xml")

    // Validate that it has two module templates and convert them to their 
    // corresponding Verilog descriptions
    complexArch.modTemplates.keys should (have size 2 and contain ("hycubePE") and contain ("globalRF"))

    // Check the PE
    val aPE = AbstractModule("hycubePE", complexArch.modTemplates("hycubePE"), complexArch)
    val peV = HWGen(aPE, SerialConfiguration)

    // Verify that the correct sub-modules exist
    peV should (
      include regex (moduleRegex("hycubePE")) and             // top module
      include regex (moduleRegex("hycubePE_alu")) and         // function unit
      include regex (moduleRegex("hycubePE_const")) and       // constant unit
      include regex (moduleRegex("hycubePE_reg_op0")) and     // register unit (operand a)
      include regex (moduleRegex("hycubePE_reg_op1")) and     // register unit (operand b)
      include regex (moduleRegex("hycubePE_reg_res")) and     // register unit (result)
      include regex (moduleRegex("hycubePE_mux_reg_op0")) and
      include regex (moduleRegex("hycubePE_mux_reg_op1")))
    (0 until 4).foreach { i =>
      peV should (
        include regex (moduleRegex(s"hycubePE_reg$i")) and    // register unit (input i)
        include regex (moduleRegex(s"hycubePE_mux_bp$i")) and
        include regex (moduleRegex(s"hycubePE_mux_out$i")))
    }

    // Verify that the correct top-level ports exist
    (0 until 4).foreach { i =>
      peV should (
        include regex (portRegex("in",  s"io_ins_in$i",   params.DataSize)) and // data input
        include regex (portRegex("out", s"io_outs_out$i", params.DataSize)))    // data output
    }
    peV should (
      include regex (portRegex("in",  "ctrl_ins_scan_en",   1)) and // control input
      include regex (portRegex("in",  "ctrl_ins_scan_in",   1)) and
      include regex (portRegex("out", "ctrl_outs_scan_out", 1)))    // control output

    // Check the register file
    val aRF = AbstractModule("globalRF", complexArch.modTemplates("globalRF"), complexArch)
    val rfV = HWGen(aRF)

    // Verify that the correct sub-modules exist
    rfV should (
      include regex (moduleRegex("globalRF")) and // top module
      include regex (moduleRegex("globalRF_rf"))) // register file unit

    // Verify that the correct top-level ports exist
    (0 until 4).foreach { i =>
      rfV should (
        include regex (portRegex("in",  s"io_ins_in$i",   params.DataSize)) and  // data input
        include regex (portRegex("out", s"io_outs_out$i", params.DataSize)))     // data output
    }
    rfV should (
      include regex (portRegex("in", "ctrl_ins_conf", confWidth(aRF))) and  // control input
      include regex (portRegex("in", "ctrl_ins_conf_en", 1)))
  }

  it should "convert complex hierarchical templates" in {
    // Grab the example architecture and parse it
    val hierArch = Architecture("src/test/resources/archparse/complex_hier_arch.xml")

    // Validate that it has five module templates and convert the two main 
    // ones to their corresponding Verilog descriptions
    hierArch.modTemplates.keys should (
      have size 5 and
      contain ("hycubePE") and
      contain ("hycube_bypass") and
      contain ("hycube_alu") and
      contain ("hycube_crossbar") and
      contain ("globalRF"))

    // Check the PE
    val aPE = AbstractModule("hycubePE", hierArch.modTemplates("hycubePE"), hierArch)
    val peV = HWGen(aPE)

    // Verify that the correct sub-modules exist
    peV should (
      include regex (moduleRegex("hycubePE")) and             // top module
      include regex (moduleRegex("hycubePE_bp")) and          // bypass sub-module
      include regex (moduleRegex("hycubePE_alu")) and         // function unit sub-module
      include regex (moduleRegex("hycubePE_xb")) and          // crossbar sub-module
      include regex (moduleRegex("hycubePE_const")) and       // constant unit
      include regex (moduleRegex("hycubePE_mux_alu_op0")) and
      include regex (moduleRegex("hycubePE_mux_alu_op1")))

    // Verify that the correct top-level ports exist
    (0 until 4).foreach { i =>
      peV should (
        include regex (portRegex("in",  s"io_ins_in$i",   params.DataSize)) and // data input
        include regex (portRegex("out", s"io_outs_out$i", params.DataSize)))    // data output
    }
    peV should (
      include regex (portRegex("in", "ctrl_ins_conf", confWidth(aPE))) and  // control input
      include regex (portRegex("in", "ctrl_ins_conf_en", 1)))

    // Check the register file
    val aRF = AbstractModule("globalRF", hierArch.modTemplates("globalRF"), hierArch)
    val rfV = HWGen(aRF)

    // Verify that the correct sub-modules exist
    rfV should (
      include regex (moduleRegex("globalRF")) and // top module
      include regex (moduleRegex("globalRF_rf"))) // register file unit

    // Verify that the correct top-level ports exist
    (0 until 4).foreach { i =>
      rfV should (
        include regex (portRegex("in",  s"io_ins_in$i",   params.DataSize)) and // data input
        include regex (portRegex("out", s"io_outs_out$i", params.DataSize)))    // data output
    }
    rfV should (
      include regex (portRegex("in", "ctrl_ins_conf", confWidth(aRF))) and  // control input
      include regex (portRegex("in", "ctrl_ins_conf_en", 1)))
  }

  /** 
   * Run tests for converting CGRAs
   */

  it should "fail to convert CGRA with combinational loops" in {
    import firrtl.transforms.CheckCombLoops.CombLoopException

    // HyCUBE-like architecture has combinational loops through its crossbars
    val arch = Architecture("src/test/resources/archparse/complex_arch.xml")
    intercept[CombLoopException] { HWGen(CGRA(arch)) }
  }

  it should "convert CGRA without top-level IO" in {
    val locParams = params.copy(cgraInferTopLevelIO = true)

    // Simple architecture has fully populated outputs, which are not inferred
    val arch  = Architecture("src/test/resources/archparse/simple_arch.xml")
    val cgraV = HWGen(CGRA(arch)(locParams))

    // Verify that only the inferred IO module exists
    anyModuleRegex.r.findAllIn(cgraV) should have size 1
    cgraV should include regex (moduleRegex("CGRA"))
  }

  it should "infer top-level IO with wrap-around on columns" in {
    val locParams = params.copy(cgraInferTopLevelIO = true, hwParallelConfiguration = false)

    // Convert the architecture to hardware
    val cgraV = HWGen(CGRA(Architecture(twoInTwoOutArchWrapCol))(locParams))

    // Verify that the right modules exist
    cgraV should (
      include regex (moduleRegex("CGRA")) and
      include regex (moduleRegex("io_inf")))
    (0 until 4).foreach { r =>
      (0 until 4).foreach { c =>
        cgraV should include regex (moduleRegex(s"block_${r}_${c}"))
      }
    }

    // Verify that the proper top-level IO exists
    val topIO = "module\\s+CGRA\\s*\\(([^\\)]*)\\);".r.findAllIn(cgraV).matchData
    topIO should have size 1
    topIO.foreach { mtch =>
      val cgraPorts = mtch.group(1)
      // Data ports
      (0 until 4).flatMap { c =>
        Seq(s"block_0_${c}_in0_inf",  s"block_0_${c}_in1_inf")
      }.foreach { port =>
        cgraPorts should include regex (portRegex("in", port, locParams.DataSize)) }
      (0 until 4).flatMap { c =>
        Seq(s"block_3_${c}_out0_inf", s"block_3_${c}_out1_inf")
      }.foreach { port =>
        cgraPorts should include regex (portRegex("out", port, locParams.DataSize)) }
      // Configuration ports
      cgraPorts should (
        include regex (portRegex("in", "scan_in", 1)) and
        include regex (portRegex("in", "scan_en", 1)))
    }
  }

  it should "infer top-level IO with wrap-around on rows" in {
    val locParams = params.copy(cgraInferTopLevelIO = true)

    // Convert the architecture to hardware
    val arch  = CGRA(Architecture(twoInTwoOutArchWrapRow))(locParams)
    val cgraV = HWGen(arch)

    // Verify that the right modules exist
    cgraV should (
      include regex (moduleRegex("CGRA")) and
      include regex (moduleRegex("io_inf")))
    (0 until 4).foreach { r =>
      (0 until 4).foreach { c =>
        cgraV should include regex (moduleRegex(s"block_${r}_${c}"))
      }
    }

    // Verify that the proper top-level IO exists
    val topIO = "module\\s+CGRA\\s*\\(([^\\)]*)\\);".r.findAllIn(cgraV).matchData
    topIO should have size 1
    topIO.foreach { mtch =>
      val cgraPorts = mtch.group(1)
      // Data ports
      (0 until 4).flatMap { r =>
        Seq(s"block_${r}_0_in0_inf",  s"block_${r}_0_in1_inf")
      }.foreach { port =>
        cgraPorts should include regex (portRegex("in", port, locParams.DataSize)) }
      (0 until 4).flatMap { r =>
        Seq(s"block_${r}_3_out0_inf", s"block_${r}_3_out1_inf")
      }.foreach { port =>
        cgraPorts should include regex (portRegex("out", port, locParams.DataSize)) }
      // Configuration ports
      (0 until 4).foreach { r =>
        (0 until 4).foreach { c =>
          val block = s"block_${r}_${c}"
          cgraPorts should (
            include regex (portRegex("in", s"${block}_conf", confWidth(arch.subModules(block)))) and
            include regex (portRegex("in", s"${block}_conf_en", 1)))
        }
      }
    }
  }

  it should "infer top-level IO in larger CGRA" in {
    val locParams = params.copy(cgraInferTopLevelIO = true)

    // Convert the architecture to hardware
    val arch  = CGRA(Architecture(fourInFourOutArch))(locParams)
    val cgraV = HWGen(arch)
    
    // Verify that the right modules exist
    cgraV should (
      include regex (moduleRegex("CGRA")) and
      include regex (moduleRegex("io_inf")))
    (0 until 4).foreach { r =>
      (0 until 4).foreach { c =>
        cgraV should include regex (moduleRegex(s"block_${r}_${c}"))
      }
    }

    // Verify that the proper top-level IO exists
    val topIO = "module\\s+CGRA\\s*\\(([^\\)]*)\\);".r.findAllIn(cgraV).matchData
    topIO should have size 1
    topIO.foreach { mtch =>
      val cgraPorts = mtch.group(1)
      // Data ports
      (0 until 4).flatMap { i =>
        Seq(s"block_0_${i}_in0_inf", s"block_3_${i}_in2_inf",
            s"block_${i}_0_in3_inf", s"block_${i}_3_in1_inf")
      }.foreach { port =>
        cgraPorts should include regex (portRegex("in", port, locParams.DataSize)) }
      (0 until 4).flatMap { i =>
        Seq(s"block_0_${i}_out0_inf", s"block_3_${i}_out2_inf",
            s"block_${i}_0_out3_inf", s"block_${i}_3_out1_inf")
      }.foreach { port =>
        cgraPorts should include regex (portRegex("out", port, locParams.DataSize)) }
      // Configuration ports
      (0 until 4).foreach { r =>
        (0 until 4).foreach { c =>
          val block = s"block_${r}_${c}"
          cgraPorts should (
            include regex (portRegex("in", s"${block}_conf", confWidth(arch.subModules(block)))) and
            include regex (portRegex("in", s"${block}_conf_en", 1)))
        }
      }
    }
  }

  it should "convert larger CGRA with IO" in {
    val locParams = params.copy(hwParallelConfiguration = false)

    // Convert the architecture to hardware
    val arch  = CGRA(Architecture("src/test/resources/archparse/large_arch_io.xml"))(locParams)
    val cgraV = HWGen(arch)

    // Verify that the right modules exist
    cgraV should include regex (moduleRegex("CGRA"))
    (0 until 6).foreach { r =>
      (if (r == 0 || r == 5) (1 until 5) else (0 until 6)).foreach { c =>
        cgraV should include regex (moduleRegex(s"block_${r}_${c}"))
      }
    }

    // Verify that the proper top-level IO exists
    val topIO = "module\\s+CGRA\\s*\\(([^\\)]*)\\);".r.findAllIn(cgraV).matchData
    topIO should have size 1
    topIO.foreach { mtch =>
      val cgraPorts = mtch.group(1)
      // Data ports
      (1 until 5).flatMap { i =>
        Seq(s"block_0_${i}_in", s"block_5_${i}_in",
            s"block_${i}_0_in", s"block_${i}_5_in")
      }.foreach { port =>
        cgraPorts should include regex (portRegex("in", port, params.DataSize)) }
      (1 until 5).flatMap { i =>
        Seq(s"block_0_${i}_out", s"block_5_${i}_out",
            s"block_${i}_0_out", s"block_${i}_5_out")
      }.foreach { port =>
        cgraPorts should include regex (portRegex("out", port, params.DataSize)) }
      // Configuration ports
      cgraPorts should (
        include regex (portRegex("in", "scan_in", 1)) and
        include regex (portRegex("in", "scan_en", 1)))
    }
  }
}
