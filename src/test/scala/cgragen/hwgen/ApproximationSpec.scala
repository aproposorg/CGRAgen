package cgragen.hwgen

import cgragen.archparse.Architecture

import cgragen.cgra.{AbstractModule, CGRA, log2Ceil}
import cgragen.cgra.AbstractOperation.isApproximable
import cgragen.cgra.primitives.AbstractFunctionUnit

import cgragen.dfgparse.Opcode._

import cgragen.hwgen.primitives.FunctionUnit

import cgragen.TestConfiguration

import chisel3.getVerilogString

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class ApproximationSpec extends AnyFlatSpec with TestConfiguration with HWSpecUtils {
  behavior of "Hardware generation"

  rng.shuffle(1 to MaxDataSize).take(NoConfs).foreach { dataSize =>
    val numOps = rng.nextInt(SupportedOps.size) + 1
    val ops    = rng.shuffle(SupportedOps).take(numOps)
    val iis    = Seq.fill(numOps)(1) // @todo randomize these when supported!
    val lats   = Seq.fill(numOps) { rng.nextInt(5) }
    val operations = (0 until numOps).map(i => (ops(i), iis(i), lats(i)))
    it should s"generate function units of width $dataSize with approximation" in {
      // Create the abstract function unit and establish its parameters
      val funcName = s"func_${dataSize}b"
      val abstractFU  = AbstractFunctionUnit(funcName, ops, iis, lats, true, dataSize)
      val hasApprxmbl = operations.exists { case (op, _, _) => isApproximable(op) }
      val hasAdd      = operations.exists { case (op, _, _) => op == OpAdd }
      val hasSub      = operations.exists { case (op, _, _) => op == OpSub }
      val hasMult     = operations.exists { case (op, _, _) => op == OpMul }

      // Generate the Verilog description and check it
      val fuVerilog  = getVerilogString(new FunctionUnit(abstractFU))
      fuVerilog should include regex (moduleRegex(funcName))
      operations
        .foreach   { case (op, ii, lat) =>
          fuVerilog should include regex (moduleRegex(s"${op}_ii${ii}_lat${lat}")) }
      if (operations.size > 1) { // control input
        fuVerilog should include regex (
          portRegex("in", "ctrl_ins_select", log2Ceil(operations.size)))
      }
      if (hasApprxmbl) { // approximate modules
        fuVerilog should include regex (
          portRegex("in", "ctrl_ins_mode", log2Ceil(params.CGRAApproximationModes+1)))

        if (hasAdd || hasSub)
          fuVerilog should include regex (moduleRegex("AdaptiveOFLOCA"))
        if (hasMult)
          fuVerilog should include regex (moduleRegex("AdaptiveRadix2Multiplier"))
      }
      fuVerilog should (
        include regex (portRegex("in", "io_ins_in_a", dataSize)) and  // data inputs
        include regex (portRegex("in", "io_ins_in_b", dataSize)) and
        include regex (portRegex("out", "io_outs_out", dataSize)))    // data output
    }
  }

  it should "leave non-approximable function unit unchanged" in {
    val locParams = params.copy(approximateArithmetic = true)
    val arch = Architecture(nonApprxmblArch)
    arch.modTemplates.keys should (have size 1 and contain ("pe"))

    // Verify that the correct sub-modules exist
    val aPE = AbstractModule("pe", arch.modTemplates("pe"), arch)
    val peV = HWGen(aPE)
    peV should (
      include regex (moduleRegex("pe")) and
      include regex (moduleRegex("pe_fu")) and
      include regex (moduleRegex("pe_rgstr0")))
    aPE.subModules("fu") match {
      case fu: AbstractFunctionUnit =>
        fu.operations
          .foreach { oprtn =>
            peV should include regex (moduleRegex(s"${oprtn.op}_ii${oprtn.ii}_lat${oprtn.lat}"))
          }
      case _ => throw new Exception("invalid function unit")
    }
    peV should not contain (moduleRegex("AdaptiveOFLOCA"))
    peV should not contain (moduleRegex("AdaptiveRadix2Multiplier"))

    // Verify that the correct top-level ports exist
    peV should (
      include regex (portRegex("in",  "io_ins_in0",   params.DataSize)) and
      include regex (portRegex("in",  "io_ins_in1",   params.DataSize)) and
      include regex (portRegex("out", "io_outs_out0", params.DataSize)) and
      include regex (portRegex("out", "io_outs_out1", params.DataSize)))
    peV should (
      include regex (portRegex("in", "ctrl_ins_select",
        log2Ceil(aPE.subModules("fu").asInstanceOf[AbstractFunctionUnit].operations.size))) and
      not include regex (portRegex("in", "ctrl_ins_mode",
        log2Ceil(params.CGRAApproximationModes + 1))))
  }

  it should "capture locally specified approximable function unit" in {
    val arch = Architecture(apprxmblArch)
    arch.modTemplates.keys should (
      have size 2 and
      contain allElementsOf Seq("pe0", "pe1"))

    // PE 0: Verify that the correct sub-modules exist
    val aPE0 = AbstractModule("pe0", arch.modTemplates("pe0"), arch)
    val pe0V = HWGen(aPE0)
    pe0V should (
      include regex (moduleRegex("pe0")) and
      include regex (moduleRegex("pe0_fu")) and
      include regex (moduleRegex("pe0_rgstr0")) and
      include regex (moduleRegex("pe0_rgstr1")))
    aPE0.subModules("fu") match {
      case fu: AbstractFunctionUnit =>
        fu.operations
          .foreach { oprtn =>
            pe0V should include regex (moduleRegex(s"${oprtn.op}_ii${oprtn.ii}_lat${oprtn.lat}"))
          }
      case _ => throw new Exception("invalid function unit")
    }
    pe0V should not include regex (moduleRegex("AdaptiveOFLOCA"))
    pe0V should not include regex (moduleRegex("AdaptiveRadix2Multiplier"))

    // PE 0: Verify that the correct top-level ports exist
    pe0V should (
      include regex (portRegex("in",  "io_ins_in0",   params.DataSize)) and
      include regex (portRegex("in",  "io_ins_in1",   params.DataSize)) and
      include regex (portRegex("out", "io_outs_out0", params.DataSize)) and
      include regex (portRegex("out", "io_outs_out1", params.DataSize)))
    pe0V should (
      include regex (portRegex("in", "ctrl_ins_select",
        log2Ceil(aPE0.subModules("fu").asInstanceOf[AbstractFunctionUnit].operations.size))) and
      not include regex (portRegex("in", "ctrl_ins_mode", 
        log2Ceil(params.CGRAApproximationModes + 1))))

    // PE 1: Verify that the correct sub-modules exist
    val aPE1 = AbstractModule("pe1", arch.modTemplates("pe1"), arch)
    val pe1V = HWGen(aPE1)
    pe1V should (
      include regex (moduleRegex("pe1")) and
      include regex (moduleRegex("pe1_fu")) and
      include regex (moduleRegex("pe1_rgstr")))
    aPE1.subModules("fu") match {
      case fu: AbstractFunctionUnit =>
        fu.operations
          .foreach { oprtn =>
            pe1V should include regex (moduleRegex(s"${oprtn.op}_ii${oprtn.ii}_lat${oprtn.lat}"))
          }
      case _ => throw new Exception("invalid function unit")
    }
    pe1V should     include regex (moduleRegex("AdaptiveOFLOCA"))
    pe1V should not include regex (moduleRegex("AdaptiveRadix2Multiplier"))

    // PE 1: Verify that the required data ports exist
    pe1V should (
      include regex (portRegex("in",  "io_ins_in0",   params.DataSize)) and
      include regex (portRegex("in",  "io_ins_in1",   params.DataSize)) and
      include regex (portRegex("out", "io_outs_out0", params.DataSize)) and
      include regex (portRegex("out", "io_outs_out1", params.DataSize)))
    pe1V should (
      include regex (portRegex("in", "ctrl_ins_select",
        log2Ceil(aPE1.subModules("fu").asInstanceOf[AbstractFunctionUnit].operations.size))) and
      include regex (portRegex("in", "ctrl_ins_mode", 
        log2Ceil(params.CGRAApproximationModes + 1))))
  }

  it should "capture globally specified approximable function unit" in {
    val locParams = params.copy(approximateArithmetic = true)
    val arch = Architecture(apprxmblArch)
    arch.modTemplates.keys should (
      have size 2 and
      contain allElementsOf Seq("pe0", "pe1"))

    // PE 0: Verify that the correct sub-modules exist
    val aPE0 = AbstractModule("pe0", arch.modTemplates("pe0"), arch)(locParams)
    val pe0V = HWGen(aPE0)(locParams)
    pe0V should (
      include regex (moduleRegex("pe0")) and
      include regex (moduleRegex("pe0_fu")) and
      include regex (moduleRegex("pe0_rgstr0")) and
      include regex (moduleRegex("pe0_rgstr1")))
    aPE0.subModules("fu") match {
      case fu: AbstractFunctionUnit =>
        fu.operations
          .foreach { oprtn =>
            pe0V should include regex (moduleRegex(s"${oprtn.op}_ii${oprtn.ii}_lat${oprtn.lat}"))
          }
      case _ => throw new Exception("invalid function unit")
    }
    pe0V should not include regex (moduleRegex("AdaptiveOFLOCA"))
    pe0V should     include regex (moduleRegex("AdaptiveRadix2Multiplier"))

    // PE 0: Verify that the required data ports exist
    pe0V should (
      include regex (portRegex("in",  "io_ins_in0",   params.DataSize)) and
      include regex (portRegex("in",  "io_ins_in1",   params.DataSize)) and
      include regex (portRegex("out", "io_outs_out0", params.DataSize)) and
      include regex (portRegex("out", "io_outs_out1", params.DataSize)))
    pe0V should (
      include regex (portRegex("in", "ctrl_ins_select",
        log2Ceil(aPE0.subModules("fu").asInstanceOf[AbstractFunctionUnit].operations.size))) and
      include regex (portRegex("in", "ctrl_ins_mode", 
        log2Ceil(params.CGRAApproximationModes + 1))))

    // PE 1: Verify that the correct sub-modules exist
    val aPE1 = AbstractModule("pe1", arch.modTemplates("pe1"), arch)(locParams)
    val pe1V = HWGen(aPE1)(locParams)
    pe1V should (
      include regex (moduleRegex("pe1")) and
      include regex (moduleRegex("pe1_fu")) and
      include regex (moduleRegex("pe1_rgstr")))
    aPE1.subModules("fu") match {
      case fu: AbstractFunctionUnit =>
        fu.operations
          .filter(_.op != OpAdd)
          .foreach { oprtn =>
            pe1V should include regex (moduleRegex(s"${oprtn.op}_ii${oprtn.ii}_lat${oprtn.lat}"))
          }
      case _ => throw new Exception("invalid function unit")
    }
    pe1V should     include regex (moduleRegex("AdaptiveOFLOCA"))
    pe1V should not include regex (moduleRegex("AdaptiveRadix2Multiplier"))

    // PE 1: Verify that the required data ports exist
    pe1V should (
      include regex (portRegex("in",  "io_ins_in0",   params.DataSize)) and
      include regex (portRegex("in",  "io_ins_in1",   params.DataSize)) and
      include regex (portRegex("out", "io_outs_out0", params.DataSize)) and
      include regex (portRegex("out", "io_outs_out1", params.DataSize)))
    pe1V should (
      include regex (portRegex("in", "ctrl_ins_select",
        log2Ceil(aPE1.subModules("fu").asInstanceOf[AbstractFunctionUnit].operations.size))) and
      include regex (portRegex("in", "ctrl_ins_mode", 
        log2Ceil(params.CGRAApproximationModes + 1))))
  }
}
